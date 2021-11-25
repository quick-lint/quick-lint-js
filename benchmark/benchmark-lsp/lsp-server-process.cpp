// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/benchmark-config.h>
#include <quick-lint-js/boost-json.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-logging.h>
#include <quick-lint-js/lsp-server-process.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/pipe.h>
#include <signal.h>
#include <spawn.h>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <utility>
#include <vector>

extern "C" {
extern char** environ;
}

namespace quick_lint_js {
byte_buffer make_text_document_did_open_notification(string8_view uri,
                                                     std::int64_t version,
                                                     string8_view text) {
  byte_buffer notification;
  notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":")");
  write_json_escaped_string(notification, uri);
  notification.append_copy(u8R"(","languageId":"javascript","version":)");
  notification.append_decimal_integer(version);
  notification.append_copy(u8R"(,"text":")");
  write_json_escaped_string(notification, text);
  notification.append_copy(u8R"("}}})");
  return notification;
}

byte_buffer make_text_document_did_fully_change_notification(
    string8_view uri, std::int64_t version, string8_view text) {
  byte_buffer notification;
  notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"version":)");
  notification.append_decimal_integer(version);
  notification.append_copy(u8R"(,"uri":")");
  write_json_escaped_string(notification, uri);
  notification.append_copy(u8R"("},"contentChanges":[{"text":")");
  write_json_escaped_string(notification, text);
  notification.append_copy(u8R"("}]}})");
  return notification;
}

byte_buffer make_text_document_did_close_notification(string8_view uri) {
  byte_buffer notification;
  notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"textDocument/didClose","params":{"textDocument":{"uri":")");
  write_json_escaped_string(notification, uri);
  notification.append_copy(u8R"("}}})");
  return notification;
}

lsp_server_process lsp_server_process::spawn(
    const benchmark_config_server& config) {
  pipe_fds server_to_client = make_pipe();
  pipe_fds client_to_server = make_pipe();

  ::posix_spawn_file_actions_t file_actions;
  posix_spawn_file_actions_init(&file_actions);
  posix_spawn_file_actions_adddup2(&file_actions, client_to_server.reader.get(),
                                   STDIN_FILENO);
  posix_spawn_file_actions_adddup2(&file_actions, server_to_client.writer.get(),
                                   STDOUT_FILENO);

  std::vector<char*> argv;
  for (const std::string& arg : config.command) {
    argv.push_back(const_cast<char*>(arg.c_str()));
  }
  argv.push_back(nullptr);
  const char* exe_file = config.command.at(0).c_str();

  std::filesystem::path old_cwd = std::filesystem::current_path();
  std::filesystem::path server_root = old_cwd;
  if (config.cwd.has_value()) {
    server_root.append(*config.cwd);
  }
  std::filesystem::current_path(server_root);
  ::pid_t pid;
  int rc = ::posix_spawnp(/*pid=*/&pid, /*file=*/exe_file,
                          /*file_actions=*/&file_actions,
                          /*attrp=*/nullptr,
                          /*argv=*/argv.data(),
                          /*envp=*/environ);
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to spawn %s: %s\n", exe_file,
                 std::strerror(errno));
    std::exit(1);
  }
  std::filesystem::current_path(old_cwd);

  posix_spawn_file_actions_destroy(&file_actions);

#if QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING
  server_to_client.reader.set_pipe_non_blocking();
#endif
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD
  client_to_server.writer.set_pipe_non_blocking();
#endif

  return lsp_server_process(server_root, config, pid,
                            std::move(server_to_client.reader),
                            std::move(client_to_server.writer));
}

void lsp_server_process::kill() {
  int rc = ::kill(this->pid_, SIGKILL);
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to kill process %lld: %s\n",
                 narrow_cast<long long>(this->pid_), std::strerror(errno));
    std::exit(1);
  }
}

void lsp_server_process::stop_future_writes() {
  // Caller should have flushed our writer.
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD && QLJS_HAVE_POLL
  QLJS_ASSERT(!this->message_writer_.get_event_fd().has_value());
#endif

  this->writer_.close();
}

void lsp_server_process::wait_for_exit() {
retry:
  int status;
  ::pid_t rc = ::waitpid(this->pid_, &status, /*options=*/0);
  if (rc == -1) {
    if (errno == EINTR) {
      goto retry;
    }
    std::fprintf(stderr, "error: failed to wait for process %lld: %s\n",
                 narrow_cast<long long>(this->pid_), std::strerror(errno));
    std::exit(1);
  }
}

bool lsp_server_process::wait_for_exit_for(std::chrono::milliseconds timeout) {
  using namespace std::chrono;
  using clock = std::chrono::steady_clock;
  auto deadline = clock::now() + timeout;
  for (;;) {
    int status;
    ::pid_t rc = ::waitpid(this->pid_, &status, /*options=*/WNOHANG);
    if (rc == -1) {
      std::fprintf(stderr, "error: failed to wait for process %lld: %s\n",
                   narrow_cast<long long>(this->pid_), std::strerror(errno));
      std::exit(1);
    }
    if (rc == this->pid_) {
      return true;
    }
    QLJS_ASSERT(rc == 0);
    bool timed_out = deadline >= clock::now();
    if (timed_out) {
      return false;
    }
  }
}

lsp_task<void> lsp_server_process::initialize_lsp_async() {
  std::int64_t initialize_request_id = this->new_message_id();
  byte_buffer initialize_request;
  initialize_request.append_copy(u8R"({"jsonrpc":"2.0","id":)");
  initialize_request.append_decimal_integer(initialize_request_id);
  initialize_request.append_copy(
      u8R"(,"method":"initialize","params":{"rootPath":")");
  string8 server_root_8 = this->server_root_.u8string();
  string8 server_root_uri = this->file_to_uri(server_root_8);
  write_json_escaped_string(initialize_request, server_root_8);
  initialize_request.append_copy(u8R"(","rootUri":")");
  write_json_escaped_string(initialize_request, server_root_uri);
  initialize_request.append_copy(u8R"(","initializationOptions":)");
  initialize_request.append_copy(
      to_string8_view(this->initialization_options_json_));
  initialize_request.append_copy(u8R"(,"workspaceFolders":[{"uri":")");
  write_json_escaped_string(initialize_request, server_root_uri);
  // publishDiagnostics is required by the TypeScript LSP server since version
  // 0.6.0:
  // https://github.com/typescript-language-server/typescript-language-server/pull/229
  initialize_request.append_copy(
      u8R"(","name":"benchmarks"}],"capabilities":{"textDocument":{"publishDiagnostics":{"versionSupport":true}}}}})");
  this->send_message(std::move(initialize_request));

  for (;;) {
    ::boost::json::object response =
        (co_await this->get_message_async()).as_object();
    if (std::int64_t* request_id = if_int64(response, "id")) {
      if (initialize_request_id == *request_id) {
        ::boost::json::value text_document_sync =
            look_up(response, "result", "capabilities", "textDocumentSync");
        if (std::int64_t* text_document_sync_kind =
                text_document_sync.if_int64()) {
          this->text_document_sync_kind_ = *text_document_sync_kind;
        } else if (std::int64_t* text_document_sync_kind =
                       look_up(text_document_sync, "change").if_int64()) {
          this->text_document_sync_kind_ = *text_document_sync_kind;
        }
        break;
      }
    }
  }

  byte_buffer initialized_notification;
  initialized_notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"initialized","params":{}})");
  this->send_message(std::move(initialized_notification));

  co_return;
}

lsp_task<void> lsp_server_process::shut_down_lsp() {
  std::int64_t shutdown_request_id = this->new_message_id();
  byte_buffer shutdown_request;
  shutdown_request.append_copy(u8R"({"jsonrpc":"2.0","id":)");
  shutdown_request.append_decimal_integer(shutdown_request_id);
  shutdown_request.append_copy(u8R"(,"method":"shutdown","params":null})");
  this->send_message(std::move(shutdown_request));

  for (;;) {
    ::boost::json::object response =
        (co_await this->get_message_async()).as_object();
    if (std::int64_t* request_id = if_int64(response, "id")) {
      if (shutdown_request_id == *request_id) {
        break;
      }
    }
  }

  byte_buffer exit_notification;
  exit_notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"exit","params":{}})");
  this->send_message(std::move(exit_notification));
}

void lsp_server_process::handle_misc_message(::boost::json::object& message) {
  if (::boost::json::string* method = if_string(message, "method")) {
    if (*method == "client/registerCapability") {
      byte_buffer response;
      response.append_copy(u8R"({"jsonrpc":"2.0","id":)");
      response.append_decimal_integer(look_up(message, "id").get_int64());
      response.append_copy(u8R"(,"result":null})");
      this->send_message(std::move(response));
      return;
    } else if (*method == "workspace/configuration") {
      byte_buffer response;
      response.append_copy(u8R"({"jsonrpc":"2.0","id":)");
      response.append_decimal_integer(look_up(message, "id").get_int64());
      response.append_copy(u8R"(,"result":[)");
      response.append_copy(
          to_string8_view(this->workspace_configuration_json_));
      response.append_copy(u8R"(]})");
      this->send_message(std::move(response));
      return;
    } else if (*method == "jshint/confirmLibraryUsage") {
      byte_buffer response;
      response.append_copy(u8R"({"jsonrpc":"2.0","id":)");
      response.append_decimal_integer(look_up(message, "id").get_int64());
      response.append_copy(u8R"(,"result":true})");
      this->send_message(std::move(response));
      return;
    }
  }
}

lsp_task<::boost::json::array> lsp_server_process::wait_for_diagnostics_async(
    string8_view document_uri, std::int64_t document_version) {
  co_return co_await this->wait_for_diagnostics_async(
      [&](::boost::json::object& params) {
        ::boost::json::string diagnostics_uri =
            look_up(params, "uri").get_string();
        if (diagnostics_uri != to_string_view(document_uri)) {
          return false;
        }
        std::int64_t* diagnostics_version = if_int64(params, "version");
        return !diagnostics_version || *diagnostics_version == document_version;
      });
}

lsp_task<::boost::json::array> lsp_server_process::wait_for_diagnostics_async(
    std::int64_t document_version) {
  co_return co_await this->wait_for_diagnostics_async(
      [&](::boost::json::object& params) {
        std::int64_t* diagnostics_version = if_int64(params, "version");
        return !diagnostics_version || *diagnostics_version == document_version;
      });
}

template <class ParamsPredicate>
lsp_task<::boost::json::array> lsp_server_process::wait_for_diagnostics_async(
    ParamsPredicate&& predicate) {
  ::boost::json::object notification =
      co_await this->wait_for_diagnostics_notification_async(
          std::forward<ParamsPredicate>(predicate));
  co_return look_up(notification, "params", "diagnostics").get_array();
}

lsp_task<::boost::json::object>
lsp_server_process::wait_for_diagnostics_notification_async() {
  co_return co_await this->wait_for_diagnostics_notification_async(
      []([[maybe_unused]] ::boost::json::object& params) { return true; });
}

template <class ParamsPredicate>
lsp_task<::boost::json::object>
lsp_server_process::wait_for_diagnostics_notification_async(
    ParamsPredicate&& predicate) {
  for (std::int64_t i = 0; i < this->diagnostics_messages_to_ignore_; ++i) {
    co_await this->wait_for_first_diagnostics_notification_async(predicate);
  }
  co_return co_await this->wait_for_first_diagnostics_notification_async(
      predicate);
}

lsp_task<::boost::json::object>
lsp_server_process::wait_for_first_diagnostics_notification_async() {
  co_return co_await this->wait_for_first_diagnostics_notification_async(
      []([[maybe_unused]] ::boost::json::object& params) { return true; });
}

template <class ParamsPredicate>
lsp_task<::boost::json::object>
lsp_server_process::wait_for_first_diagnostics_notification_async(
    ParamsPredicate&& predicate) {
  for (;;) {
    ::boost::json::object message =
        (co_await this->get_message_async()).as_object();
    if (::boost::json::string* method = if_string(message, "method")) {
      if (*method == "textDocument/publishDiagnostics") {
        if (predicate(look_up(message, "params").get_object())) {
          co_return message;
        }
      } else {
        this->handle_misc_message(message);
      }
    }
  }
}

void lsp_server_process::send_message(byte_buffer&& message) {
  if (log_file) {
    if (log_colors) {
      std::fprintf(log_file, "\x1b[33m");
    }
    std::fprintf(log_file, "~~> send: ");
    message.enumerate_chunks(
        [&](const std::byte* c_begin, const std::byte* c_end) -> void {
          std::fprintf(log_file, "%.*s", narrow_cast<int>(c_end - c_begin),
                       reinterpret_cast<const char*>(c_begin));
        });
    std::fprintf(log_file, "\n");
    if (log_colors) {
      std::fprintf(log_file, "\x1b[0m");
    }
  }
  this->message_writer_.send_message(std::move(message));
}

std::filesystem::path lsp_server_process::file_to_path(string8_view path) {
  return this->server_root_ / path;
}

string8 lsp_server_process::file_to_uri(string8_view path) {
  string8 uri = u8"file://" + this->file_to_path(path).u8string();
  // HACK(strager): Flow's LSP server can't handle a trailing / in URIs.
  if (uri.ends_with(u8'/')) {
    uri.resize(uri.size() - 1);
  }
  return uri;
}

void lsp_server_process::create_file_on_disk_if_needed(string8_view path) {
  if (this->need_files_on_disk_) {
    std::filesystem::path absolute_path = this->file_to_path(path);
    FILE* file = std::fopen(absolute_path.c_str(), "w");
    if (!file) {
      std::fprintf(stderr, "error: failed to open %s: %s\n",
                   absolute_path.c_str(), std::strerror(errno));
      std::exit(1);
    }
    std::fclose(file);
  }
}

::boost::json::value lsp_server_process::get_message_awaitable::await_resume() {
  QLJS_ASSERT(!this->message_content_.empty());
  std::error_code error;
  ::boost::json::value root =
      ::boost::json::parse(to_string_view(this->message_content_), error);
  if (error != std::error_code()) {
    std::fprintf(stderr, "error: parsing JSON from LSP server failed\n");
    std::exit(1);
  }
  return root;
}

void lsp_server_process::continuing_lsp_message_parser::message_parsed(
    string8_view message_content) {
  if (log_file) {
    if (log_colors) {
      std::fprintf(log_file, "\x1b[32m");
    }
    std::fprintf(log_file, "<~~ recv: %.*s\n",
                 narrow_cast<int>(message_content.size()),
                 reinterpret_cast<const char*>(message_content.data()));
    if (log_colors) {
      std::fprintf(log_file, "\x1b[0m");
    }
  }

  auto continuation = std::exchange(this->continuation_, nullptr);
  if (continuation) {
    string8_view* out_message_content =
        std::exchange(this->out_message_content_, nullptr);
    QLJS_ALWAYS_ASSERT(out_message_content);
    *out_message_content = message_content;
    continuation();
  }
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
