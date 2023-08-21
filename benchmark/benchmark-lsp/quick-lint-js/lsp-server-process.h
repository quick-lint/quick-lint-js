// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <chrono>
#include <coroutine>
#include <cstdint>
#include <filesystem>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/io/event-loop2.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/lsp/lsp-message-parser.h>
#include <quick-lint-js/lsp/lsp-pipe-writer.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/memory-resource.h>
#include <simdjson.h>
#include <type_traits>
#include <unistd.h>
#include <utility>

#if QLJS_HAVE_KQUEUE
#include <sys/event.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

#if QLJS_HAVE_POLL
#include <poll.h>
#endif

namespace quick_lint_js {
namespace std_coroutine = std;

Byte_Buffer make_text_document_did_open_notification(String8_View uri,
                                                     std::int64_t version,
                                                     String8_View text);
Byte_Buffer make_text_document_did_fully_change_notification(
    String8_View uri, std::int64_t version, String8_View text);
Byte_Buffer make_text_document_did_close_notification(String8_View uri);

class LSP_Task_Promise_Type_Base {
 public:
  std_coroutine::suspend_never initial_suspend() { return {}; }

  auto final_suspend() noexcept { return Final_Suspend_Awaitable(); }

  std_coroutine::coroutine_handle<> continuation_ = {};

 private:
  struct Final_Suspend_Awaitable {
    bool await_ready() const noexcept { return false; }

    template <class Promise>
    std_coroutine::coroutine_handle<> await_suspend(
        std_coroutine::coroutine_handle<Promise> continuation) noexcept {
      auto cont = continuation.promise().continuation_;
      return cont ? cont : std_coroutine::noop_coroutine();
    }

    void await_resume() noexcept {}
  };

  void unhandled_exception() {}
};

template <class T>
class LSP_Task {
 public:
  class Promise_Type : public LSP_Task_Promise_Type_Base {
   public:
    LSP_Task get_return_object() {
      return LSP_Task(
          std_coroutine::coroutine_handle<Promise_Type>::from_promise(*this));
    }

    template <class U>
    void return_value(U&& value) {
      QLJS_ASSERT(!this->return_value_.has_value());
      this->return_value_.emplace(std::forward<U>(value));
    }

    std::optional<T> return_value_;
  };

  using promise_type = Promise_Type;

  explicit LSP_Task(
      std_coroutine::coroutine_handle<Promise_Type> continuation) noexcept
      : continuation_(continuation) {}

  auto operator co_await() const noexcept {
    struct Awaitable {
      void await_suspend(
          std_coroutine::coroutine_handle<> continuation) noexcept {
        Promise_Type& promise = this->continuation_.promise();
        QLJS_ASSERT(!promise.return_value_.has_value());
        QLJS_ASSERT(!promise.continuation_);
        promise.continuation_ = continuation;
      }

      bool await_ready() const noexcept {
        QLJS_ASSERT(this->continuation_);
        Promise_Type& promise = this->continuation_.promise();
        return promise.return_value_.has_value();
      }

      T&& await_resume() {
        QLJS_ASSERT(this->continuation_);
        Promise_Type& promise = this->continuation_.promise();
        QLJS_ASSERT(promise.return_value_.has_value());
        return std::move(*promise.return_value_);
      }

      std_coroutine::coroutine_handle<Promise_Type> continuation_;
    };

    return Awaitable{this->continuation_};
  }

 private:
  std_coroutine::coroutine_handle<Promise_Type> continuation_;
};

template <>
class LSP_Task<void> {
 public:
  class Promise_Type : public LSP_Task_Promise_Type_Base {
   public:
    LSP_Task get_return_object() {
      return LSP_Task(
          std_coroutine::coroutine_handle<Promise_Type>::from_promise(*this));
    }

    void return_void() { this->did_return_ = true; }

    bool did_return_ = false;
  };

  using promise_type = Promise_Type;

  explicit LSP_Task(
      std_coroutine::coroutine_handle<Promise_Type> continuation) noexcept
      : continuation_(continuation) {}

  auto operator co_await() const noexcept {
    struct Awaitable {
      void await_suspend(
          std_coroutine::coroutine_handle<> continuation) noexcept {
        QLJS_ASSERT(this->continuation_);
        Promise_Type& promise = this->continuation_.promise();
        QLJS_ASSERT(!promise.did_return_);
        QLJS_ASSERT(!promise.continuation_);
        promise.continuation_ = continuation;
      }

      bool await_ready() const noexcept {
        QLJS_ASSERT(this->continuation_);
        Promise_Type& promise = this->continuation_.promise();
        return promise.did_return_;
      }

      void await_resume() { QLJS_ASSERT(this->continuation_); }

      std_coroutine::coroutine_handle<Promise_Type> continuation_;
    };

    return Awaitable{this->continuation_};
  }

 private:
  std_coroutine::coroutine_handle<Promise_Type> continuation_;
};

enum {
  TEXT_DOCUMENT_SYNC_KIND_NONE = 0,
  TEXT_DOCUMENT_SYNC_KIND_FULL = 1,
  TEXT_DOCUMENT_SYNC_KIND_INCREMENTAL = 2,
};

class LSP_Server_Process : private Event_Loop_Pipe_Read_Delegate
#if QLJS_EVENT_LOOP2_PIPE_WRITE
    ,
                           private Event_Loop_Pipe_Write_Delegate
#endif
{
 private:
  class Get_Message_Awaitable;

 public:
  static LSP_Server_Process spawn(const Benchmark_Config_Server& config);

  // Run an LSP_Task<void>-returning function with the server. After, forcefully
  // stop the server.
  template <class Func>
  void run_and_kill(Func&& func) {
    Event_Loop event_loop;

    [[maybe_unused]] LSP_Task<void> task = [&]() -> LSP_Task<void> {
      co_await std::forward<Func>(func)();
      this->message_writer_.flush();  // FIXME(strager): Might deadlock.
      // Some servers don't implement 'exit', so terminate the server manually.
      bool exited = this->wait_for_exit_for(
          std::chrono::milliseconds(300));  // FIXME(strager): Might deadlock.
      if (!exited) {
        this->kill();
        this->wait_for_exit();

        // HACK(strager): Flow and Rome don't ever close our reader (its
        // writer), even if we kill the process. (They fork their own processes
        // which we can't directly kill.) Work around this by forcefully
        // stopping our event loop.
        event_loop.un_keep_alive();
      }
    }();

    event_loop.keep_alive();
    event_loop.register_pipe_read(this->reader_.ref(), this);
#if QLJS_EVENT_LOOP2_PIPE_WRITE
    event_loop.register_pipe_write(this->message_writer_.get_pipe_fd(), this);
    this->enable_or_disable_writer_events_as_needed(&event_loop);
#endif
    event_loop.run();
  }

  // Forcefully stop the server.
  void kill();

  // Wait for the server to stop on its own. Should be called to clean up
  // OS resources.
  void wait_for_exit();

  // Like wait_for_exit, but stops waiting after the given duration. Returns
  // true if the process exited, or false if the given duration elapsed.
  bool wait_for_exit_for(std::chrono::milliseconds timeout);

  LSP_Task<void> initialize_lsp_async();
  LSP_Task<void> shut_down_lsp();

  void handle_misc_message(::simdjson::dom::object& message);

  // Receives the next message sent by the server. Returns an awaitable yielding
  // a ::simdjson::dom::element. The returned value is invalidated on the next
  // call to get_message_async. See NOTE[LSP_Server_Process-message] for
  // details.
  Get_Message_Awaitable get_message_async() {
    return Get_Message_Awaitable(this);
  }

  // Returns just the array of diagnostics.
  // Respects diagnosticsMessagesToIgnore.
  LSP_Task<::simdjson::dom::array> wait_for_diagnostics_async(
      std::int64_t document_version);
  LSP_Task<::simdjson::dom::array>
  wait_for_diagnostics_after_incremental_change_async(
      std::int64_t document_version);
  LSP_Task<::simdjson::dom::array> wait_for_diagnostics_ignoring_async(
      std::int64_t document_version, std::int64_t messages_to_ignore);
  LSP_Task<::simdjson::dom::array> wait_for_diagnostics_async(
      String8_View document_uri, std::int64_t document_version);
  template <class Params_Predicate>
  LSP_Task<::simdjson::dom::array> wait_for_diagnostics_async(
      Params_Predicate&&);
  template <class Params_Predicate>
  LSP_Task<::simdjson::dom::array> wait_for_diagnostics_ignoring_async(
      Params_Predicate&&, std::int64_t messages_to_ignore);

  // Returns the entire notification object.
  // Respects diagnosticsMessagesToIgnore.
  LSP_Task<::simdjson::dom::object> wait_for_diagnostics_notification_async();
  template <class Params_Predicate>
  LSP_Task<::simdjson::dom::object> wait_for_diagnostics_notification_async(
      Params_Predicate&&);
  template <class Params_Predicate>
  LSP_Task<::simdjson::dom::object> wait_for_diagnostics_notification_async(
      Params_Predicate&&, std::int64_t messages_to_ignore);

  // Returns the entire notification object.
  // Does not respect diagnosticsMessagesToIgnore; returns the first matching
  // message.
  LSP_Task<::simdjson::dom::object>
  wait_for_first_diagnostics_notification_async();
  template <class Params_Predicate>
  LSP_Task<::simdjson::dom::object>
  wait_for_first_diagnostics_notification_async(Params_Predicate&&);

  void send_message(Byte_Buffer&& message);

  std::filesystem::path file_to_path(String8_View path);
  String8 file_to_uri(String8_View path);

  void create_file_on_disk_if_needed(String8_View path);

 private:
  class Get_Message_Awaitable {
   public:
    explicit Get_Message_Awaitable(LSP_Server_Process* process)
        : process_(process) {}

    bool await_ready() { return false; }

    void await_suspend(std_coroutine::coroutine_handle<> continuation) {
      auto& message_parser = this->process_->message_parser_;
      message_parser.continuation_ = continuation;
      message_parser.out_message_content_ = &this->message_content_;
    }

    ::simdjson::dom::element await_resume();

   private:
    String8_View message_content_;
    LSP_Server_Process* process_;
  };

  struct Continuing_LSP_Message_Parser
      : public LSP_Message_Parser<Continuing_LSP_Message_Parser> {
    void message_parsed(String8_View message_content);

    std_coroutine::coroutine_handle<> continuation_;
    String8_View* out_message_content_ = nullptr;
  };

  void on_pipe_read_data(Event_Loop_Base* event_loop, Platform_File_Ref,
                         String8_View data) override {
    this->message_parser_.append(data);
#if QLJS_EVENT_LOOP2_PIPE_WRITE
    this->enable_or_disable_writer_events_as_needed(event_loop);
#endif
  }
  void on_pipe_read_end(Event_Loop_Base* event_loop,
                        Platform_File_Ref) override {
    event_loop->un_keep_alive();
  }
  void on_pipe_read_error(Event_Loop_Base* event_loop, Platform_File_Ref,
                          Platform_File_IO_Error) override {
    event_loop->un_keep_alive();
  }

#if QLJS_EVENT_LOOP2_PIPE_WRITE
  void on_pipe_write_ready(Event_Loop_Base* event_loop,
                           Platform_File_Ref) override {
    this->message_writer_.on_pipe_write_ready();
    this->enable_or_disable_writer_events_as_needed(event_loop);
  }
  void on_pipe_write_end(Event_Loop_Base*, Platform_File_Ref) override {
    this->message_writer_.on_pipe_write_end();
  }

  // We have the same problem enabling and disabling the
  // Non_Blocking_Pipe_Writer that the LSP server has. See
  // HACK[Non_Blocking_Pipe_Writer-enable-disable] for details.
  void enable_or_disable_writer_events_as_needed(Event_Loop_Base* event_loop) {
    bool should_be_enabled = this->message_writer_.get_event_fd().has_value();
    if (should_be_enabled) {
      event_loop->enable_pipe_write(this->message_writer_.get_pipe_fd());
    } else {
      event_loop->disable_pipe_write(this->message_writer_.get_pipe_fd());
    }
  }
#endif

  explicit LSP_Server_Process(std::filesystem::path server_root,
                              const Benchmark_Config_Server& config,
                              ::pid_t pid, Platform_File reader,
                              Platform_File writer)
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
  Platform_File reader_;  // server -> client
  Platform_File writer_;  // client -> server

  Continuing_LSP_Message_Parser message_parser_;
  LSP_Pipe_Writer message_writer_ = LSP_Pipe_Writer(this->writer_.ref());

  // This holds the most recently parsed message (if any).
  //
  // NOTE[LSP_Server_Process-message]: get_message_async calls
  // json_parser_.parse(). This means that get_message_async's returned
  // ::simdjson::dom::element's lifetime ends when the next call to
  // get_message_async begins.
  //
  // Reusing a parser minimizes malloc/free traffic during the timed benchmark.
  ::simdjson::dom::parser json_parser_;

  std::int64_t next_message_id_ = 1;
  std::int64_t text_document_sync_kind_ = TEXT_DOCUMENT_SYNC_KIND_NONE;
};
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
