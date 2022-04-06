// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_SERVER_PROCESS_H
#define QUICK_LINT_JS_LSP_SERVER_PROCESS_H

#include <boost/json/value.hpp>
#include <chrono>
#include <cstdint>
#include <experimental/coroutine>
#include <filesystem>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/event-loop.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/lsp-pipe-writer.h>
#include <quick-lint-js/lsp-server.h>
#include <type_traits>
#include <unistd.h>
#include <utility>

#if QLJS_HAVE_KQUEUE
#include <sys/event.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/windows.h>
#endif

#if QLJS_HAVE_POLL
#include <poll.h>
#endif

namespace quick_lint_js {
class benchmark_server_config;
namespace std_coroutine = std::experimental;

byte_buffer make_text_document_did_open_notification(string8_view uri,
                                                     std::int64_t version,
                                                     string8_view text);
byte_buffer make_text_document_did_fully_change_notification(
    string8_view uri, std::int64_t version, string8_view text);
byte_buffer make_text_document_did_close_notification(string8_view uri);

class lsp_task_promise_type_base {
 public:
  std_coroutine::suspend_never initial_suspend() { return {}; }

  auto final_suspend() noexcept { return final_suspend_awaitable(); }

  std_coroutine::coroutine_handle<> continuation_ = {};

 private:
  struct final_suspend_awaitable {
    bool await_ready() const noexcept { return false; }

    template <class Promise>
    std::experimental::coroutine_handle<> await_suspend(
        std::experimental::coroutine_handle<Promise> continuation) noexcept {
      auto cont = continuation.promise().continuation_;
      return cont ? cont : std_coroutine::noop_coroutine();
    }

    void await_resume() noexcept {}
  };

  void unhandled_exception() {}
};

template <class T>
class lsp_task {
 public:
  class promise_type : public lsp_task_promise_type_base {
   public:
    lsp_task get_return_object() {
      return lsp_task(
          std_coroutine::coroutine_handle<promise_type>::from_promise(*this));
    }

    template <class U>
    void return_value(U&& value) {
      QLJS_ASSERT(!this->return_value_.has_value());
      this->return_value_.emplace(std::forward<U>(value));
    }

    std::optional<T> return_value_;
  };

  explicit lsp_task(
      std_coroutine::coroutine_handle<promise_type> continuation) noexcept
      : continuation_(continuation) {}

  auto operator co_await() const noexcept {
    struct awaitable {
      void await_suspend(
          std_coroutine::coroutine_handle<> continuation) noexcept {
        promise_type& promise = this->continuation_.promise();
        QLJS_ASSERT(!promise.return_value_.has_value());
        QLJS_ASSERT(!promise.continuation_);
        promise.continuation_ = continuation;
      }

      bool await_ready() const noexcept {
        QLJS_ASSERT(this->continuation_);
        promise_type& promise = this->continuation_.promise();
        return promise.return_value_.has_value();
      }

      T&& await_resume() {
        QLJS_ASSERT(this->continuation_);
        promise_type& promise = this->continuation_.promise();
        QLJS_ASSERT(promise.return_value_.has_value());
        return std::move(*promise.return_value_);
      }

      std_coroutine::coroutine_handle<promise_type> continuation_;
    };

    return awaitable{this->continuation_};
  }

 private:
  std_coroutine::coroutine_handle<promise_type> continuation_;
};

template <>
class lsp_task<void> {
 public:
  class promise_type : public lsp_task_promise_type_base {
   public:
    lsp_task get_return_object() {
      return lsp_task(
          std_coroutine::coroutine_handle<promise_type>::from_promise(*this));
    }

    void return_void() { this->did_return_ = true; }

    bool did_return_ = false;
  };

  explicit lsp_task(
      std_coroutine::coroutine_handle<promise_type> continuation) noexcept
      : continuation_(continuation) {}

  auto operator co_await() const noexcept {
    struct awaitable {
      void await_suspend(
          std_coroutine::coroutine_handle<> continuation) noexcept {
        QLJS_ASSERT(this->continuation_);
        promise_type& promise = this->continuation_.promise();
        QLJS_ASSERT(!promise.did_return_);
        QLJS_ASSERT(!promise.continuation_);
        promise.continuation_ = continuation;
      }

      bool await_ready() const noexcept {
        QLJS_ASSERT(this->continuation_);
        promise_type& promise = this->continuation_.promise();
        return promise.did_return_;
      }

      void await_resume() { QLJS_ASSERT(this->continuation_); }

      std_coroutine::coroutine_handle<promise_type> continuation_;
    };

    return awaitable{this->continuation_};
  }

 private:
  std_coroutine::coroutine_handle<promise_type> continuation_;
};

enum {
  TEXT_DOCUMENT_SYNC_KIND_NONE = 0,
  TEXT_DOCUMENT_SYNC_KIND_FULL = 1,
  TEXT_DOCUMENT_SYNC_KIND_INCREMENTAL = 2,
};

class lsp_server_process {
 private:
  class get_message_awaitable;

 public:
  static lsp_server_process spawn(const benchmark_config_server& config);

  // Run an lsp_task<void>-returning function with the server. After, forcefully
  // stop the server.
  template <class Func>
  void run_and_kill(Func&& func) {
    [[maybe_unused]] lsp_task<void> task = [&]() -> lsp_task<void> {
      co_await std::forward<Func>(func)();
      this->message_writer_.flush();  // FIXME(strager): Might deadlock.
      // Some servers don't implement 'exit', so terminate the server manually.
      bool exited = this->wait_for_exit_for(
          std::chrono::milliseconds(300));  // FIXME(strager): Might deadlock.
      if (!exited) {
        this->kill();

        // HACK(strager): Flow doesn't ever close our reader (its writer), even
        // if we kill the process. (Flow forks its own processes which we can't
        // directly kill.) It does reliably crash if we close our writer (its
        // reader), though, so let's force a crash.
        this->stop_future_writes();

        this->wait_for_exit();
      }
    }();
    this->event_loop_.run();
  }

  // Forcefully stop the server.
  void kill();

  // HACK(strager): Close the client->server pipe to make Flow's LSP server
  // stop.
  void stop_future_writes();

  // Wait for the server to stop on its own. Should be called to clean up
  // OS resources.
  void wait_for_exit();

  // Like wait_for_exit, but stops waiting after the given duration. Returns
  // true if the process exited, or false if the given duration elapsed.
  bool wait_for_exit_for(std::chrono::milliseconds timeout);

  lsp_task<void> initialize_lsp_async();
  lsp_task<void> shut_down_lsp();

  void handle_misc_message(::boost::json::object& message);

  // Receives the next message sent by the server. Returns an awaitable yielding
  // a ::boost::json::value.
  get_message_awaitable get_message_async() {
    return get_message_awaitable(this);
  }

  // Returns just the array of diagnostics.
  // Respects diagnosticsMessagesToIgnore.
  lsp_task<::boost::json::array> wait_for_diagnostics_async(
      std::int64_t document_version);
  lsp_task<::boost::json::array>
  wait_for_diagnostics_after_incremental_change_async(
      std::int64_t document_version);
  lsp_task<::boost::json::array> wait_for_diagnostics_ignoring_async(
      std::int64_t document_version, std::int64_t messages_to_ignore);
  lsp_task<::boost::json::array> wait_for_diagnostics_async(
      string8_view document_uri, std::int64_t document_version);
  template <class ParamsPredicate>
  lsp_task<::boost::json::array> wait_for_diagnostics_async(ParamsPredicate&&);
  template <class ParamsPredicate>
  lsp_task<::boost::json::array> wait_for_diagnostics_ignoring_async(
      ParamsPredicate&&, std::int64_t messages_to_ignore);

  // Returns the entire notification object.
  // Respects diagnosticsMessagesToIgnore.
  lsp_task<::boost::json::object> wait_for_diagnostics_notification_async();
  template <class ParamsPredicate>
  lsp_task<::boost::json::object> wait_for_diagnostics_notification_async(
      ParamsPredicate&&);
  template <class ParamsPredicate>
  lsp_task<::boost::json::object> wait_for_diagnostics_notification_async(
      ParamsPredicate&&, std::int64_t messages_to_ignore);

  // Returns the entire notification object.
  // Does not respect diagnosticsMessagesToIgnore; returns the first matching
  // message.
  lsp_task<::boost::json::object>
  wait_for_first_diagnostics_notification_async();
  template <class ParamsPredicate>
  lsp_task<::boost::json::object> wait_for_first_diagnostics_notification_async(
      ParamsPredicate&&);

  void send_message(byte_buffer&& message);

  std::filesystem::path file_to_path(string8_view path);
  string8 file_to_uri(string8_view path);

  void create_file_on_disk_if_needed(string8_view path);

 private:
  class get_message_awaitable {
   public:
    explicit get_message_awaitable(lsp_server_process* process)
        : process_(process) {}

    bool await_ready() { return false; }

    void await_suspend(std_coroutine::coroutine_handle<> continuation) {
      auto& message_parser = this->process_->message_parser_;
      message_parser.continuation_ = continuation;
      message_parser.out_message_content_ = &this->message_content_;
    }

    ::boost::json::value await_resume();

   private:
    string8_view message_content_;
    lsp_server_process* process_;
  };

  struct continuing_lsp_message_parser
      : public lsp_message_parser<continuing_lsp_message_parser> {
    void message_parsed(string8_view message_content);

    std_coroutine::coroutine_handle<> continuation_;
    string8_view* out_message_content_ = nullptr;
  };

  class lsp_event_loop : public event_loop<lsp_event_loop> {
   public:
    explicit lsp_event_loop(lsp_server_process* process) : process_(process) {}

    platform_file_ref get_readable_pipe() const {
      return this->process_->reader_.ref();
    }

    void append(string8_view data) {
      this->process_->message_parser_.append(data);
    }

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
    std::optional<posix_fd_file_ref> get_pipe_write_fd() {
      if (!this->process_->writer_.valid()) {
        return std::nullopt;
      }
      return this->process_->message_writer_.get_event_fd();
    }
#endif

#if QLJS_HAVE_KQUEUE
    void on_pipe_write_event(const struct ::kevent& event) {
      this->process_->message_writer_.on_poll_event(event);
    }
#elif QLJS_HAVE_POLL
    void on_pipe_write_event(const ::pollfd& event) {
      this->process_->message_writer_.on_poll_event(event);
    }
#endif

#if QLJS_HAVE_KQUEUE
    void on_fs_changed_kevent(const struct ::kevent&) {}

    void on_fs_changed_kevents() {}
#endif

#if QLJS_HAVE_INOTIFY
    std::optional<posix_fd_file_ref> get_inotify_fd() { return std::nullopt; }

    void on_fs_changed_event(const ::pollfd&) {}
#endif

#if defined(_WIN32)
    void on_fs_changed_event(::OVERLAPPED*, ::DWORD, ::DWORD) {}
#endif

   private:
    lsp_server_process* process_;
  };

  explicit lsp_server_process(std::filesystem::path server_root,
                              const benchmark_config_server& config,
                              ::pid_t pid, platform_file reader,
                              platform_file writer)
      : server_root_(server_root),
        initialization_options_json_(config.initialization_options_json),
        workspace_configuration_json_(config.workspace_configuration_json),
        diagnostics_messages_to_ignore_(config.diagnostics_messages_to_ignore),
        diagnostics_messages_to_ignore_after_incremental_change_(
            config.diagnostics_messages_to_ignore_after_incremental_change),
        need_files_on_disk_(config.need_files_on_disk),
        pid_(pid),
        reader_(std::move(reader)),
        writer_(std::move(writer)) {}

  std::int64_t new_message_id() { return this->next_message_id_++; }

  std::filesystem::path server_root_;
  std::string initialization_options_json_;
  std::string workspace_configuration_json_;
  std::int64_t diagnostics_messages_to_ignore_;
  std::int64_t diagnostics_messages_to_ignore_after_incremental_change_;
  bool need_files_on_disk_;

  ::pid_t pid_;
  platform_file reader_;  // server -> client
  platform_file writer_;  // client -> server

  continuing_lsp_message_parser message_parser_;
  lsp_pipe_writer message_writer_ = lsp_pipe_writer(this->writer_.ref());
  lsp_event_loop event_loop_ = lsp_event_loop(this);

  std::int64_t next_message_id_ = 1;
  std::int64_t text_document_sync_kind_ = TEXT_DOCUMENT_SYNC_KIND_NONE;
};
}

#endif

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
