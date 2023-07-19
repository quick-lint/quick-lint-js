// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DEBUG_DEBUG_SERVER_H
#define QUICK_LINT_JS_DEBUG_DEBUG_SERVER_H

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <atomic>
#include <memory>
#include <mongoose.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/synchronized.h>
#include <string>

namespace quick_lint_js {
class Byte_Buffer;
class Trace_Flusher_WebSocket_Backend;

struct Debug_Server_IO_Error {
  std::string error_message;

  std::string to_string() const { return this->error_message; }
};

// Internally, debug_server manages a thread. The thread is called the 'server
// thread'. All other threads are called 'other threads'.
class Debug_Server {
 private:
  struct Create_Tag {};

 public:
  static std::shared_ptr<Debug_Server> create();
  static std::vector<std::shared_ptr<Debug_Server>> instances();

  explicit Debug_Server(Create_Tag);

  ~Debug_Server();

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
  Result<void, Debug_Server_IO_Error> wait_for_server_start();

  // Precondition: wait_for_server_start was called and it succeeded.
  std::string url();
  std::string url(std::string_view path);
  std::string websocket_url(std::string_view path);
  std::uint16_t tcp_port_number();

  void debug_probe_publish_lsp_documents();
  void debug_probe_publish_vector_profile();

 private:
  struct Shared_State;

  // Run on any thread:
  void wake_up_server_thread();
  void wake_up_server_thread(Lock_Ptr<Shared_State> &);

  // Appends the host:port part of a URL to the given string.
  void get_host_and_port(std::string &out);

  // Run on the server thread:
  void run_on_current_thread();
  void begin_closing_all_connections(::mg_mgr *);
  void http_server_callback(::mg_connection *c, int ev, void *ev_data) noexcept;
  void wakeup_pipe_callback(::mg_connection *c, int ev, void *ev_data) noexcept;
  void publish_lsp_documents_if_needed();

  struct Shared_State {
    std::string requested_listen_address = "http://localhost:0";

    // Written to by the server thread. Read by other threads.
    ::mg_addr actual_listen_address;
    int wakeup_pipe = -1;
    bool initialized = false;  // When true, actual_listen_address and
                               // wakeup_pipe are valid.
    std::string init_error;

    std::uint16_t port_number() const;
  };

  Synchronized<Shared_State> state_;

  // Signalled by the server thread after init_data_ or init_error_ is set.
  mutable Condition_Variable initialized_;

  // Written to by other threads. Read by the server thread.
  std::atomic<bool> stop_server_thread_{false};
  std::atomic<bool> need_publish_lsp_documents_{false};
  std::atomic<bool> need_publish_vector_profile_{false};

  // Used by other threads only:
  Thread server_thread_;
  bool did_wait_for_server_start_ = false;

  // Used by server thread only:
  // Each backend is associated with one WebSocket connection.
  std::vector<std::unique_ptr<Trace_Flusher_WebSocket_Backend>>
      tracer_backends_;
#if QLJS_FEATURE_VECTOR_PROFILING
  Vector_Max_Size_Histogram_By_Owner max_size_histogram_;
#endif

  friend class Trace_Flusher_WebSocket_Backend;
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
