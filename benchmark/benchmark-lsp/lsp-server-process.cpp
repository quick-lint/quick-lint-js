// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/benchmark-config.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/lsp-logging.h>
#include <quick-lint-js/lsp-server-process.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/process.h>
#include <quick-lint-js/util/narrow-cast.h>
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
Byte_Buffer make_text_document_did_open_notification(String8_View uri,
                                                     std::int64_t version,
                                                     String8_View text) {
  Byte_Buffer notification;
  notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":")"sv);
  write_json_escaped_string(notification, uri);
  notification.append_copy(u8R"(","languageId":"javascript","version":)"sv);
  notification.append_decimal_integer(version);
  notification.append_copy(u8R"(,"text":")"sv);
  write_json_escaped_string(notification, text);
  notification.append_copy(u8R"("}}})"sv);
  return notification;
}

Byte_Buffer make_text_document_did_fully_change_notification(
    String8_View uri, std::int64_t version, String8_View text) {
  Byte_Buffer notification;
  notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"version":)"sv);
  notification.append_decimal_integer(version);
  notification.append_copy(u8R"(,"uri":")"sv);
  write_json_escaped_string(notification, uri);
  notification.append_copy(u8R"("},"contentChanges":[{"text":")"sv);
  write_json_escaped_string(notification, text);
  notification.append_copy(u8R"("}]}})"sv);
  return notification;
}

Byte_Buffer make_text_document_did_close_notification(String8_View uri) {
  Byte_Buffer notification;
  notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"textDocument/didClose","params":{"textDocument":{"uri":")"sv);
  write_json_escaped_string(notification, uri);
  notification.append_copy(u8R"("}}})"sv);
  return notification;
}

LSP_Server_Process LSP_Server_Process::spawn(
    const Benchmark_Config_Server& config) {
  Pipe_FDs server_to_client = make_pipe();
  Pipe_FDs client_to_server = make_pipe();

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

  return LSP_Server_Process(server_root, config, pid,
                            std::move(server_to_client.reader),
                            std::move(client_to_server.writer));
}

void LSP_Server_Process::kill() {
  int rc = ::kill(this->pid_, SIGKILL);
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to kill process %lld: %s\n",
                 narrow_cast<long long>(this->pid_), std::strerror(errno));
    std::exit(1);
  }
}

void LSP_Server_Process::stop_event_loop() {
  // Caller should have flushed our writer.
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD && QLJS_HAVE_POLL
  QLJS_ASSERT(!this->message_writer_.get_event_fd().has_value());
#endif

  this->should_stop_event_loop_ = true;
}

void LSP_Server_Process::wait_for_exit() { wait_for_process_exit(this->pid_); }

bool LSP_Server_Process::wait_for_exit_for(std::chrono::milliseconds timeout) {
  using namespace std::chrono;
  using Clock = std::chrono::steady_clock;
  auto deadline = Clock::now() + timeout;
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
    bool timed_out = deadline >= Clock::now();
    if (timed_out) {
      return false;
    }
  }
}

LSP_Task<void> LSP_Server_Process::initialize_lsp_async() {
  std::int64_t initialize_request_id = this->new_message_id();
  Byte_Buffer initialize_request;
  initialize_request.append_copy(u8R"({"jsonrpc":"2.0","id":)"sv);
  initialize_request.append_decimal_integer(initialize_request_id);
  initialize_request.append_copy(
      u8R"(,"method":"initialize","params":{"rootPath":")"sv);
  String8 server_root_8 = this->server_root_.u8string();
  String8 server_root_uri = this->file_to_uri(server_root_8);
  write_json_escaped_string(initialize_request, server_root_8);
  initialize_request.append_copy(u8R"(","rootUri":")"sv);
  write_json_escaped_string(initialize_request, server_root_uri);
  initialize_request.append_copy(u8R"(","initializationOptions":)"sv);
  initialize_request.append_copy(
      to_string8_view(this->initialization_options_json_));
  initialize_request.append_copy(u8R"(,"workspaceFolders":[{"uri":")"sv);
  write_json_escaped_string(initialize_request, server_root_uri);
  // publishDiagnostics is required by the TypeScript LSP server since version
  // 0.6.0:
  // https://github.com/typescript-language-server/typescript-language-server/pull/229
  initialize_request.append_copy(
      u8R"(","name":"benchmarks"}],"capabilities":{"textDocument":{"publishDiagnostics":{"versionSupport":true}}}}})"sv);
  this->send_message(std::move(initialize_request));

  for (;;) {
    ::simdjson::dom::object response;
    if ((co_await this->get_message_async()).get(response) !=
        ::simdjson::SUCCESS) {
      std::fprintf(stderr, "fatal: message should be an object\n");
      std::exit(1);
    }
    std::int64_t request_id;
    if (response["id"].get(request_id) == ::simdjson::SUCCESS) {
      if (initialize_request_id == request_id) {
        ::simdjson::simdjson_result<::simdjson::dom::element>
            text_document_sync =
                response["result"]["capabilities"]["textDocumentSync"];
        std::int64_t text_document_sync_kind;
        if (text_document_sync.get(text_document_sync_kind) ==
            ::simdjson::SUCCESS) {
          this->text_document_sync_kind_ = text_document_sync_kind;
        } else if (text_document_sync["change"].get(text_document_sync_kind) ==
                   ::simdjson::SUCCESS) {
          this->text_document_sync_kind_ = text_document_sync_kind;
        }
        break;
      }
    }
  }

  Byte_Buffer initialized_notification;
  initialized_notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"initialized","params":{}})"sv);
  this->send_message(std::move(initialized_notification));

  co_return;
}

LSP_Task<void> LSP_Server_Process::shut_down_lsp() {
  std::int64_t shutdown_request_id = this->new_message_id();
  Byte_Buffer shutdown_request;
  shutdown_request.append_copy(u8R"({"jsonrpc":"2.0","id":)"sv);
  shutdown_request.append_decimal_integer(shutdown_request_id);
  shutdown_request.append_copy(u8R"(,"method":"shutdown"})"sv);
  this->send_message(std::move(shutdown_request));

  for (;;) {
    ::simdjson::dom::object response;
    if ((co_await this->get_message_async()).get(response) !=
        ::simdjson::SUCCESS) {
      std::fprintf(stderr, "fatal: response should be an object\n");
      std::exit(1);
    }
    std::int64_t request_id;
    if (response["id"].get(request_id) == ::simdjson::SUCCESS) {
      if (shutdown_request_id == request_id) {
        break;
      }
    }
  }

  Byte_Buffer exit_notification;
  exit_notification.append_copy(
      u8R"({"jsonrpc":"2.0","method":"exit","params":{}})"sv);
  this->send_message(std::move(exit_notification));
}

void LSP_Server_Process::handle_misc_message(::simdjson::dom::object& message) {
  std::string_view method;
  if (message["method"].get(method) == ::simdjson::SUCCESS) {
    if (method == "client/registerCapability"sv) {
      std::int64_t id;
      if (message["id"].get(id) != ::simdjson::SUCCESS) {
        std::fprintf(stderr, "fatal: could not parse id\n");
        std::exit(1);
      }

      Byte_Buffer response;
      response.append_copy(u8R"({"jsonrpc":"2.0","id":)"sv);
      response.append_decimal_integer(id);
      response.append_copy(u8R"(,"result":null})"sv);
      this->send_message(std::move(response));
      return;
    } else if (method == "workspace/configuration"sv) {
      std::int64_t id;
      if (message["id"].get(id) != ::simdjson::SUCCESS) {
        std::fprintf(stderr, "fatal: could not parse id\n");
        std::exit(1);
      }

      Byte_Buffer response;
      response.append_copy(u8R"({"jsonrpc":"2.0","id":)"sv);
      response.append_decimal_integer(id);
      response.append_copy(u8R"(,"result":[)"sv);
      response.append_copy(
          to_string8_view(this->workspace_configuration_json_));
      response.append_copy(u8R"(]})"sv);
      this->send_message(std::move(response));
      return;
    }
  }
}

LSP_Task<::simdjson::dom::array> LSP_Server_Process::wait_for_diagnostics_async(
    String8_View document_uri, std::int64_t document_version) {
  co_return co_await this->wait_for_diagnostics_async(
      [&](::simdjson::dom::object& params) {
        std::string_view diagnostics_uri;
        if (params["uri"].get(diagnostics_uri) != ::simdjson::SUCCESS) {
          std::fprintf(stderr, "fatal: missing 'uri' from diagnostic\n");
          std::exit(1);
        }
        if (to_string8_view(diagnostics_uri) != document_uri) {
          return false;
        }
        std::int64_t diagnostics_version;
        if (params["version"].get(diagnostics_version) == ::simdjson::SUCCESS) {
          return diagnostics_version == document_version;
        }
        return true;
      });
}

LSP_Task<::simdjson::dom::array> LSP_Server_Process::wait_for_diagnostics_async(
    std::int64_t document_version) {
  return this->wait_for_diagnostics_ignoring_async(
      document_version,
      /*messages_to_ignore=*/this->diagnostics_messages_to_ignore_);
}

LSP_Task<::simdjson::dom::array>
LSP_Server_Process::wait_for_diagnostics_after_incremental_change_async(
    std::int64_t document_version) {
  return this->wait_for_diagnostics_ignoring_async(
      document_version,
      /*messages_to_ignore=*/this
          ->diagnostics_messages_to_ignore_after_incremental_change_);
}

LSP_Task<::simdjson::dom::array>
LSP_Server_Process::wait_for_diagnostics_ignoring_async(
    std::int64_t document_version, std::int64_t messages_to_ignore) {
  co_return co_await this->wait_for_diagnostics_ignoring_async(
      [&](::simdjson::dom::object& params) {
        std::int64_t diagnostics_version;
        if (params["version"].get(diagnostics_version) == ::simdjson::SUCCESS) {
          return diagnostics_version == document_version;
        }
        return true;
      },
      /*messages_to_ignore=*/messages_to_ignore);
}

template <class Params_Predicate>
LSP_Task<::simdjson::dom::array> LSP_Server_Process::wait_for_diagnostics_async(
    Params_Predicate&& predicate) {
  return this->wait_for_diagnostics_ignoring_async(
      std::forward<Params_Predicate>(predicate),
      /*messages_to_ignore=*/this->diagnostics_messages_to_ignore_);
}

template <class Params_Predicate>
LSP_Task<::simdjson::dom::array>
LSP_Server_Process::wait_for_diagnostics_ignoring_async(
    Params_Predicate&& predicate, std::int64_t messages_to_ignore) {
  ::simdjson::dom::object notification =
      co_await this->wait_for_diagnostics_notification_async(
          std::forward<Params_Predicate>(predicate),
          /*messages_to_ignore=*/messages_to_ignore);
  ::simdjson::dom::array diagnostics;
  if (notification["params"]["diagnostics"].get(diagnostics) !=
      ::simdjson::SUCCESS) {
    std::fprintf(stderr, "fatal: params.diagnostics should be an array\n");
    std::exit(1);
  }
  co_return diagnostics;
}

LSP_Task<::simdjson::dom::object>
LSP_Server_Process::wait_for_diagnostics_notification_async() {
  co_return co_await this->wait_for_diagnostics_notification_async(
      []([[maybe_unused]] ::simdjson::dom::object& params) { return true; });
}

template <class Params_Predicate>
LSP_Task<::simdjson::dom::object>
LSP_Server_Process::wait_for_diagnostics_notification_async(
    Params_Predicate&& predicate) {
  return this->wait_for_diagnostics_notification_async(
      std::forward<Params_Predicate>(predicate),
      /*messages_to_ignore=*/this->diagnostics_messages_to_ignore_);
}

template <class Params_Predicate>
LSP_Task<::simdjson::dom::object>
LSP_Server_Process::wait_for_diagnostics_notification_async(
    Params_Predicate&& predicate, std::int64_t messages_to_ignore) {
  for (std::int64_t i = 0; i < messages_to_ignore; ++i) {
    co_await this->wait_for_first_diagnostics_notification_async(predicate);
  }
  co_return co_await this->wait_for_first_diagnostics_notification_async(
      predicate);
}

LSP_Task<::simdjson::dom::object>
LSP_Server_Process::wait_for_first_diagnostics_notification_async() {
  co_return co_await this->wait_for_first_diagnostics_notification_async(
      []([[maybe_unused]] ::simdjson::dom::object& params) { return true; });
}

template <class Params_Predicate>
LSP_Task<::simdjson::dom::object>
LSP_Server_Process::wait_for_first_diagnostics_notification_async(
    Params_Predicate&& predicate) {
  for (;;) {
    ::simdjson::dom::element message_value = co_await this->get_message_async();
    ::simdjson::dom::object message;
    if (message_value.get(message) != ::simdjson::SUCCESS) {
      std::fprintf(stderr, "fatal: message should be an object\n");
    }
    std::string_view method;
    if (message["method"].get(method) == ::simdjson::SUCCESS) {
      if (method == "textDocument/publishDiagnostics"sv) {
        ::simdjson::dom::object params;
        if (message["params"].get(params) != ::simdjson::SUCCESS) {
          std::fprintf(stderr, "fatal: message params should be an object\n");
        }
        if (predicate(params)) {
          co_return message;
        }
      } else {
        this->handle_misc_message(message);
      }
    }
  }
}

void LSP_Server_Process::send_message(Byte_Buffer&& message) {
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

std::filesystem::path LSP_Server_Process::file_to_path(String8_View path) {
  return this->server_root_ / path;
}

String8 LSP_Server_Process::file_to_uri(String8_View path) {
  String8 uri = u8"file://" + this->file_to_path(path).u8string();
  // HACK(strager): Flow's LSP server can't handle a trailing / in URIs.
  if (uri.ends_with(u8'/')) {
    uri.resize(uri.size() - 1);
  }
  return uri;
}

void LSP_Server_Process::create_file_on_disk_if_needed(String8_View path) {
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

::simdjson::dom::element
LSP_Server_Process::Get_Message_Awaitable::await_resume() {
  QLJS_ASSERT(!this->message_content_.empty());
  ::simdjson::dom::element root;
  // TODO(strager): Pad message_content_ to avoid a copy here.
  ::simdjson::error_code error =
      this->process_->json_parser_
          .parse(reinterpret_cast<const std::uint8_t*>(
                     this->message_content_.data()),
                 this->message_content_.size())
          .get(root);
  if (error != ::simdjson::SUCCESS) {
    std::fprintf(stderr, "error: parsing JSON from LSP server failed\n");
    std::exit(1);
  }
  return root;
}

void LSP_Server_Process::Continuing_LSP_Message_Parser::message_parsed(
    String8_View message_content) {
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
    String8_View* out_message_content =
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
