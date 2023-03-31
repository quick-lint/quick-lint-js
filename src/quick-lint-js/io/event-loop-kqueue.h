// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_IO_EVENT_LOOP_KQUEUE_H
#define QUICK_LINT_JS_IO_EVENT_LOOP_KQUEUE_H

#include <quick-lint-js/port/have.h>

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#elif QLJS_HAVE_KQUEUE

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
#include <quick-lint-js/util/narrow-cast.h>
#include <sys/event.h>

namespace quick_lint_js {
// An event loop using BSD kqueue(). See event_loop_base for details.
template <class Derived>
class kqueue_event_loop : public event_loop_base<Derived> {
 public:
  enum event_udata : std::uintptr_t {
    event_udata_readable_pipe,
    event_udata_pipe_write,
    event_udata_fs_changed,
  };

  explicit kqueue_event_loop() : kqueue_fd_(::kqueue()) {
    QLJS_ASSERT(this->kqueue_fd_.valid());
  }

  posix_fd_file_ref kqueue_fd() noexcept { return this->kqueue_fd_.ref(); }

  void run() {
    {
      static_assert(QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING);
      platform_file_ref pipe = this->const_derived().get_readable_pipe();
      QLJS_SLOW_ASSERT(pipe.is_pipe_non_blocking());

      std::array<struct ::kevent, 2> changes;
      std::size_t change_count = 0;

      EV_SET(&changes[change_count++], pipe.get(), EVFILT_READ, EV_ADD, 0, 0,
             reinterpret_cast<void*>(event_udata_readable_pipe));

      if (std::optional<posix_fd_file_ref> fd =
              this->derived().get_pipe_write_fd()) {
        EV_SET(&changes[change_count++], fd->get(), EVFILT_WRITE, EV_ADD, 0, 0,
               reinterpret_cast<void*>(event_udata_pipe_write));
      }

      QLJS_ASSERT(change_count > 0);
      QLJS_ASSERT(change_count <= changes.size());
      ::timespec timeout = {.tv_sec = 0, .tv_nsec = 0};
      int rc = ::kevent(this->kqueue_fd_.get(),
                        /*changelist=*/changes.data(),
                        /*nchanges=*/narrow_cast<int>(change_count),
                        /*eventlist=*/nullptr, /*nevents=*/0,
                        /*timeout=*/&timeout);
      if (rc == -1) {
        QLJS_UNIMPLEMENTED();
      }
      QLJS_ASSERT(rc == 0);
    }

    for (;;) {
      std::array<struct ::kevent, 10> events;
      int rc = ::kevent(this->kqueue_fd_.get(),
                        /*changelist=*/nullptr, /*nchanges=*/0,
                        /*eventlist=*/events.data(),
                        /*nevents=*/narrow_cast<int>(events.size()),
                        /*timeout=*/nullptr);
      if (rc == -1) {
        if (errno == EINTR) {
          continue;
        }
        QLJS_UNIMPLEMENTED();
      }
      QLJS_ASSERT(rc > 0);

      bool fs_changed = false;
      for (int i = 0; i < rc; ++i) {
        struct ::kevent& event = events[narrow_cast<std::size_t>(i)];
        switch (reinterpret_cast<std::uintptr_t>(event.udata)) {
        case event_udata_readable_pipe: {
          QLJS_ASSERT(event.filter == EVFILT_READ);
          bool done = this->read_from_pipe();
          if (done) {
            return;
          }
          break;
        }

        case event_udata_pipe_write:
          QLJS_ASSERT(event.filter == EVFILT_WRITE);
          this->derived().on_pipe_write_event(event);
          break;

        case event_udata_fs_changed:
          this->derived().on_fs_changed_kevent(event);
          fs_changed = true;
          break;

        default:
          QLJS_UNREACHABLE();
          break;
        }
      }
      if (fs_changed) {
        this->derived().on_fs_changed_kevents();
      }
    }
  }

 private:
  posix_fd_file kqueue_fd_;
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
