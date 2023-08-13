// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_IO_EVENT_LOOP_POLL_H
#define QUICK_LINT_JS_IO_EVENT_LOOP_POLL_H

#include <quick-lint-js/port/have.h>

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#elif QLJS_HAVE_POLL

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <poll.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/fixed-vector.h>
#include <quick-lint-js/io/event-loop-base.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
// An event loop using POSIX poll(). See Event_Loop_Base for details.
template <class Derived>
class Poll_Event_Loop : public Event_Loop_Base<Derived> {
 public:
  void run() {
    for (;;) {
      // TODO(strager): Only call read() if poll() tells us that data is
      // available.
      bool done = this->read_from_pipe();
      if (done) {
        break;
      }

      static_assert(QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING);
      std::optional<Platform_File_Ref> pipe =
          this->const_derived().get_readable_pipe();
      if (!pipe.has_value()) {
        break;
      }
      QLJS_SLOW_ASSERT(pipe->is_pipe_non_blocking());

      Fixed_Vector<::pollfd, 3> pollfds;

      Fixed_Vector_Size read_pipe_index = pollfds.size();
      pollfds.push_back(
          ::pollfd{.fd = pipe->get(), .events = POLLIN, .revents = 0});

      std::optional<Fixed_Vector_Size> write_pipe_index;
      if (std::optional<POSIX_FD_File_Ref> fd =
              this->derived().get_pipe_write_fd()) {
        write_pipe_index = pollfds.size();
        pollfds.push_back(::pollfd{
            .fd = fd->get(),
            .events = POLLOUT,
            .revents = 0,
        });
      }

#if QLJS_HAVE_INOTIFY
      std::optional<Fixed_Vector_Size> inotify_index;
      if (std::optional<POSIX_FD_File_Ref> fd =
              this->derived().get_inotify_fd()) {
        inotify_index = pollfds.size();
        pollfds.push_back(::pollfd{
            .fd = fd->get(),
            .events = POLLIN,
            .revents = 0,
        });
      }
#endif

      QLJS_ASSERT(pollfds.size() > 0);
      int rc = ::poll(pollfds.data(), narrow_cast<::nfds_t>(pollfds.size()),
                      /*timeout=*/-1);
      if (rc == -1) {
        QLJS_UNIMPLEMENTED();
      }
      QLJS_ASSERT(rc > 0);

      const ::pollfd& read_pipe_event = pollfds[read_pipe_index];
      if (read_pipe_event.revents & POLLIN) {
        continue;
      }
      if (read_pipe_event.revents & POLLERR) {
        QLJS_UNIMPLEMENTED();
      }

      if (write_pipe_index.has_value()) {
        const ::pollfd& write_pipe_event = pollfds[*write_pipe_index];
        if (write_pipe_event.revents != 0) {
          this->derived().on_pipe_write_event(write_pipe_event);
        }
      }

#if QLJS_HAVE_INOTIFY
      if (inotify_index.has_value()) {
        const ::pollfd& inotify_event = pollfds[*inotify_index];
        if (inotify_event.revents != 0) {
          this->derived().on_fs_changed_event(inotify_event);
        }
      }
#endif
    }
  }
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
