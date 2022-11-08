// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DEBUG_DEBUG_SERVER_H
#define QUICK_LINT_JS_DEBUG_DEBUG_SERVER_H

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <atomic>
#include <memory>
#include <mongoose.h>
#include <optional>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/port/thread.h>
#include <string>

namespace quick_lint_js {
class byte_buffer;
class trace_flusher_websocket_backend;

struct debug_server_io_error {
  std::string error_message;

  std::string to_string() const { return this->error_message; }
};

// Internally, debug_server manages a thread. The thread is called the 'server
// thread'. All other threads are called 'other threads'.
class debug_server {
 private:
  struct create_tag {};

 public:
  static std::shared_ptr<debug_server> create();
  static std::vector<std::shared_ptr<debug_server>> instances();

  explicit debug_server(create_tag);

  ~debug_server();

  // Example address: "http://localhost:1234"
  //
  // Not thread safe.
  //
  // Precondition: Server thread is not running.
  void set_listen_address(std::string_view address);

  // start_server_thread can be called at most once.
  void start_server_thread();
  void stop_server_thread();

  // Precondition: start_server_thread was called.
  result<void, debug_server_io_error> wait_for_server_start();

  // Precondition: wait_for_server_start was called and it succeeded.
  std::string url() const;
  std::string url(std::string_view path) const;
  std::string websocket_url(std::string_view path) const;

  void debug_probe_publish_vector_profile();

 private:
  // Run on any thread:
  void wake_up_server_thread();
  void wake_up_server_thread(std::unique_lock<mutex> &);

  // Run on the server thread:
  void run_on_current_thread();
  void begin_closing_all_connections(::mg_mgr *);
  void http_server_callback(::mg_connection *c, int ev, void *ev_data) noexcept;
  void wakeup_pipe_callback(::mg_connection *c, int ev, void *ev_data) noexcept;

  struct init_data {
    // HACK(strager): Clang 11 with libstdc++ 12 requires a user-declared (not
    // default) constructor. Otherwise, std::is_constructible_v<init_data>
    // returns false, causing std::optional<init_data>::emplace() to not
    // compile.
    explicit init_data() {}

    std::string actual_listen_address;
    int wakeup_pipe = -1;
  };

  mutable mutex mutex_;

  // Protected by mutex_.
  std::string requested_listen_address_ = "http://localhost:0";

  // Protected by mutex_. Written to by the server thread. Read by other
  // threads.
  std::optional<init_data> init_data_;
  std::string init_error_;

  // Signalled by the server thread after init_data_ or init_error_ is set.
  mutable condition_variable initialized_;

  // Written to by other threads. Read by the server thread.
  std::atomic<bool> stop_server_thread_{false};
  std::atomic<bool> need_publish_vector_profile_{false};

  // Used by other threads only:
  thread server_thread_;
  bool did_wait_for_server_start_ = false;

  // Used by server thread only:
  // Each backend is associated with one WebSocket connection.
  std::vector<std::unique_ptr<trace_flusher_websocket_backend>>
      tracer_backends_;
#if QLJS_FEATURE_VECTOR_PROFILING
  vector_max_size_histogram_by_owner max_size_histogram_;
#endif

  friend class trace_flusher_websocket_backend;
};
}

#endif

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
