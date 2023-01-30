// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_IO_EVENT_LOOP_WIN32_H
#define QUICK_LINT_JS_IO_EVENT_LOOP_WIN32_H

#include <quick-lint-js/port/have.h>

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#elif QLJS_HAVE_WINDOWS_H

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/event-loop-base.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/windows.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
windows_handle_file create_io_completion_port() noexcept;

// An event loop using Win32's I/O completion ports. See event_loop_base for
// details.
template <class Derived>
class windows_event_loop : public event_loop_base<Derived> {
 public:
  enum completion_key : ULONG_PTR {
    completion_key_invalid = 0,
    completion_key_stop,
    completion_key_fs_changed,
  };

  explicit windows_event_loop()
      : io_completion_port_(create_io_completion_port()) {}

  windows_handle_file_ref io_completion_port() noexcept {
    return this->io_completion_port_.ref();
  }

  void run() {
    static_assert(!QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING);

    this->read_pipe_thread_ = thread([this] { this->run_read_pipe_thread(); });
    for (;;) {
      DWORD number_of_bytes_transferred;
      ULONG_PTR completion_key = completion_key_invalid;
      OVERLAPPED* overlapped;
      BOOL ok = ::GetQueuedCompletionStatus(
          /*CompletionPort=*/this->io_completion_port_.get(),
          /*lpNumberOfBytesTransferred=*/&number_of_bytes_transferred,
          /*lpCompletionKey=*/&completion_key, /*lpOverlapped=*/&overlapped,
          /*dwMilliseconds=*/INFINITE);
      DWORD error = ok ? 0 : ::GetLastError();
      if (!overlapped) {
        switch (error) {
        case WAIT_TIMEOUT:
          QLJS_UNREACHABLE();
          break;

        default:
          QLJS_UNIMPLEMENTED();
          break;
        }
      }

      switch (completion_key) {
      case completion_key_invalid:
        QLJS_UNREACHABLE();
        break;

      case completion_key_stop:
        QLJS_ASSERT(ok);
        this->read_pipe_thread_.join();
        return;

      case completion_key_fs_changed:
        this->derived().on_fs_changed_event(overlapped,
                                            number_of_bytes_transferred, error);
        break;

      default:
        QLJS_UNREACHABLE();
      }
    }
  }

 private:
  void run_read_pipe_thread() {
    while (!this->read_from_pipe()) {
      // Loop.
    }

    BOOL ok = ::PostQueuedCompletionStatus(
        /*CompletionPort=*/this->io_completion_port_.get(),
        /*dwNumberOfBytesTransferred=*/0,
        /*dwCompletionKey=*/completion_key_stop,
        /*lpOverlapped=*/reinterpret_cast<OVERLAPPED*>(1));
    if (!ok) {
      QLJS_UNIMPLEMENTED();
    }
  }

  windows_handle_file io_completion_port_;
  thread read_pipe_thread_;
};

inline windows_handle_file create_io_completion_port() noexcept {
  windows_handle_file iocp(::CreateIoCompletionPort(
      /*FileHandle=*/INVALID_HANDLE_VALUE,
      /*ExistingCompletionPort=*/nullptr, /*CompletionKey=*/0,
      /*NumberOfConcurrentThreads=*/1));
  if (!iocp.valid()) {
    QLJS_UNIMPLEMENTED();
  }
  return iocp;
}
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
