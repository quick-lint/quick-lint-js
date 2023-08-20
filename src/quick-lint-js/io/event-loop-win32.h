// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

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
Windows_Handle_File create_io_completion_port();

// An event loop using Win32's I/O completion ports. See Event_Loop_Base for
// details.
template <class Derived>
class Windows_Event_Loop : public Event_Loop_Base<Derived> {
 public:
  enum Completion_Key : ULONG_PTR {
    completion_key_invalid = 0,
    completion_key_stop,
    completion_key_fs_changed,
  };

  explicit Windows_Event_Loop()
      : io_completion_port_(create_io_completion_port()) {}

  Windows_Handle_File_Ref io_completion_port() {
    return this->io_completion_port_.ref();
  }

  void run() {
    static_assert(!QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING);

    this->read_pipe_thread_ = Thread([this] { this->run_read_pipe_thread(); });
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

  Windows_Handle_File io_completion_port_;
  Thread read_pipe_thread_;
};

inline Windows_Handle_File create_io_completion_port() {
  Windows_Handle_File iocp(::CreateIoCompletionPort(
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
