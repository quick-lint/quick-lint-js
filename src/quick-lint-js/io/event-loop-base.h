// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/narrow-cast.h>

#if QLJS_HAVE_KQUEUE
#include <sys/event.h>
#endif

#if QLJS_HAVE_POLL
#include <poll.h>
#endif

#if defined(_WIN32)
#define QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING 0
#else
#define QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING 1
#endif

namespace quick_lint_js {
#if QLJS_HAVE_CXX_CONCEPTS
template <class Delegate>
concept event_loop_delegate = requires(Delegate d, const Delegate cd,
#if QLJS_HAVE_KQUEUE
                                       const struct ::kevent kqueue_event,
#endif
#if QLJS_HAVE_POLL
                                       const ::pollfd poll_event,
#endif
                                       String8_View data) {
  {cd.get_readable_pipe()};
  {d.append(data)};

#if QLJS_HAVE_POLL && !QLJS_HAVE_KQUEUE
  {d.get_pipe_write_fd()};
  {d.on_pipe_write_event(poll_event)};
#endif

#if QLJS_HAVE_INOTIFY
  {d.get_inotify_fd()};
  {d.on_fs_changed_event(poll_event)};
#endif

#if QLJS_HAVE_KQUEUE
  {d.on_fs_changed_kevent(kqueue_event)};
  {d.on_fs_changed_kevents()};
#endif
};
#endif

// An event_loop implements I/O concurrency on a single thread.
//
// An event_loop manages the following types of I/O:
//
// * a readable pipe
// * a writable pipe
//
// event_loop uses the CRTP pattern. Inherit from event_loop<your_class>.
// your_class must satisfy the event_loop_delegate concept.
//
// event_loop will never call non-const member functions in parallel.
template <class Derived>
class Event_Loop_Base {
 protected:
  // Returns true when the pipe has closed. Returns false if the pipe might
  // still have data available (now or in the future).
  bool read_from_pipe() {
    std::array<Char8, 65536> buffer;
    std::optional<Platform_File_Ref> pipe =
        this->const_derived().get_readable_pipe();
    if (!pipe.has_value()) {
      return true;
    }
#if QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING
    QLJS_SLOW_ASSERT(pipe->is_pipe_non_blocking());
#else
    QLJS_SLOW_ASSERT(!pipe->is_pipe_non_blocking());
#endif
    File_Read_Result read_result = pipe->read(buffer.data(), buffer.size());
    if (!read_result.ok()) {
#if QLJS_HAVE_UNISTD_H
      if (read_result.error().is_would_block_try_again_error()) {
#if QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING
        return false;
#else
        QLJS_UNREACHABLE();
#endif
      }
#endif
      QLJS_UNIMPLEMENTED();
      return true;
    }
    if (read_result.at_end_of_file()) {
      return true;
    } else {
      QLJS_ASSERT(read_result.bytes_read() != 0);
      std::lock_guard<Mutex> lock(this->user_code_mutex_);
      this->derived().append(String8_View(
          buffer.data(), narrow_cast<std::size_t>(read_result.bytes_read())));
      return false;
    }
  }

#if QLJS_HAVE_CXX_CONCEPTS
  event_loop_delegate
#endif
      auto&
      derived() {
    return *static_cast<Derived*>(this);
  }

  const
#if QLJS_HAVE_CXX_CONCEPTS
      event_loop_delegate
#endif
      auto&
      const_derived() const {
    return *static_cast<const Derived*>(this);
  }

 protected:
  // Acquire user_code_mutex_ when calling non-const member functions of
  // Derived.
  // TODO(strager): Only use a lock on Windows. Avoid the lock on POSIX
  // platforms where we only have one thread.
  Mutex user_code_mutex_;
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
