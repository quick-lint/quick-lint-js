// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/io/event-loop2.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/util/synchronized.h>
#include <vector>

#if QLJS_HAVE_POLL
#include <sys/poll.h>
#endif

#if QLJS_HAVE_POLL
namespace quick_lint_js {
enum class Event_Loop2_Poll::Registered_Event_Kind {
  // Event_Loop2_Pipe_Read_Delegate
  pipe_read,
  // Event_Loop2_Pipe_Write_Delegate
  pipe_write,
  // Event_Loop2_Custom_Poll_Delegate
  custom_poll,
};

struct Event_Loop2_Poll::Registered_Event {
  explicit Registered_Event(Registered_Event_Kind kind, void* delegate,
                            short poll_events)
      : kind(kind), delegate(delegate), poll_events(poll_events) {}

  // These don't change thus do not need to be protected by a lock.
  const Registered_Event_Kind kind;
  void* const delegate;
  const short poll_events;

  // Writers: Protected by Event_Loop2_Poll::Impl::state's lock.
  // Readers: Lock-free.
  std::atomic<bool> enabled = true;
};

// Mutable state which is across multiple threads thus requires synchronization.
struct Event_Loop2_Poll::Shared_State {
  Stable_Hash_Map<Platform_File_Ref, Registered_Event> registered_events;
};

struct Event_Loop2_Poll::Impl {
  // NOTE[Event_Loop2_Poll-control_pipe]: control_pipe is used to interrupt the
  // event loop. Writing a byte to control_pipe.writer will cause the reader to
  // recognize a change in state.
  //
  // control_pipe.writer can be used by any thread.
  //
  // NOTE[Event_Loop2_Poll-control_pipe-non-blocking]: control_pipe.writer is
  // non-blocking. If a write fails with EAGAIN/EWOULDBLOCK, that's okay;
  // someone else signalled the event loop and the event loop hasn't been
  // interrupted yet. The event loop will eventually interrupt.
  Pipe_FDs control_pipe = make_pipe();

  Synchronized<Shared_State> state;
};

Event_Loop2_Poll::Event_Loop2_Poll() : impl_(new Impl()) {
  this->impl_->control_pipe.writer.set_pipe_non_blocking();
}

Event_Loop2_Poll::~Event_Loop2_Poll() { delete this->impl_; }

void Event_Loop2_Poll::run() {
  // events[i] corresponds to event_registered_events[i].
  std::vector<::pollfd> events;
  std::vector<const Registered_Event*> event_registered_events;

  while (!this->is_stop_requested()) {
    events.clear();
    event_registered_events.clear();
    events.push_back(::pollfd{
        .fd = this->impl_->control_pipe.reader.get(),
        .events = POLLIN,
        .revents = 0,
    });
    event_registered_events.push_back(nullptr);
    {
      Lock_Ptr<Shared_State> state = this->impl_->state.lock();
      for (const auto& [fd, registered_event] : state->registered_events) {
        if (registered_event.enabled) {
          events.push_back(::pollfd{
              .fd = fd.get(),
              .events = registered_event.poll_events,
              .revents = 0,
          });
          event_registered_events.push_back(&registered_event);
        }
      }
    }
    QLJS_ASSERT(events.size() == event_registered_events.size());

    QLJS_ASSERT(!events.empty());
    int rc = ::poll(events.data(), narrow_cast<::nfds_t>(events.size()),
                    /*timeout=*/-1);
    if (rc == -1) {
      QLJS_UNIMPLEMENTED();
    }
    if (rc == -1) {
      if (errno == EINTR) {
        continue;
      }
      QLJS_UNIMPLEMENTED();
    }
    QLJS_ASSERT(rc > 0);

    for (std::size_t i = 0; i < events.size(); ++i) {
      const ::pollfd& event = events[i];
      if (event.revents == 0) {
        continue;
      }

      const Registered_Event* r = event_registered_events[i];
      POSIX_FD_File_Ref fd(event.fd);
      if (r == nullptr) {
        // We were notified of changes by notify_via_control_pipe. See
        // NOTE[Event_Loop2_Poll-control_pipe].
        QLJS_ASSERT(fd == this->impl_->control_pipe.reader.ref());
        if (this->is_stop_requested()) {
          // Stop the event loop. Do not process other events.
          return;
        }
        // Process remaining events. Changes to Shared_State::registered_events
        // will take effect when we next call ::poll.
      } else if (r->enabled) {
        switch (r->kind) {
        case Registered_Event_Kind::pipe_read: {
          Event_Loop2_Pipe_Read_Delegate* delegate =
              static_cast<Event_Loop2_Pipe_Read_Delegate*>(r->delegate);
          QLJS_ASSERT(event.events == POLLIN);
          QLJS_ASSERT(event.revents & (POLLIN | POLLERR | POLLHUP));
          if (event.revents & POLLERR) {
            // We don't have an error code. Call read_from_pipe because it will
            // probably give us a more useful error.
          }
          if (event.revents & POLLHUP) {
            // Call read_from_pipe which will detect the end-of-file condition
            // for us.
          }
          std::array<Char8, 65536> buffer;
          QLJS_SLOW_ASSERT(fd.is_pipe_non_blocking());
          File_Read_Result read_result = fd.read(buffer.data(), buffer.size());
          this->handle_read_from_pipe_result(
              read_result, Span<const Char8>(buffer), fd, delegate);
          // FIXME(strager): On Read_From_Pipe_Result::end, do we need to
          // disable r to prevent more events?
          break;
        }

        case Registered_Event_Kind::pipe_write: {
          Event_Loop2_Pipe_Write_Delegate* delegate =
              static_cast<Event_Loop2_Pipe_Write_Delegate*>(r->delegate);
          QLJS_ASSERT(event.events == POLLOUT);
          QLJS_ASSERT(event.revents & (POLLOUT | POLLERR | POLLHUP));
          if (event.revents & (POLLHUP | POLLERR)) {
            // On Linux, we receive POLLERR if the reader has closed. On
            // macOS, we receive POLLHUP if the reader has closed.
            delegate->on_pipe_write_end(this, fd);
          } else {
            delegate->on_pipe_write_ready(this, fd);
          }
          break;
        }

        case Registered_Event_Kind::custom_poll:
          static_cast<Event_Loop2_Custom_Poll_Delegate*>(r->delegate)
              ->on_custom_poll_event(this, fd, event.revents);
          break;
        }
      }

      if (this->is_stop_requested()) {
        // If a stop was requested by a callback, avoid calling other callbacks.
        // This is not strictly necessary.
        return;
      }

      // Optimization: Stop the loop early if there will be no revents.
      rc -= 1;
      if (rc == 0) {
        break;
      }
    }
  }
}

void Event_Loop2_Poll::register_pipe_read(
    Platform_File_Ref pipe, Event_Loop2_Pipe_Read_Delegate* delegate) {
  QLJS_ASSERT(pipe.is_pipe_non_blocking());
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto [_it, inserted] = state->registered_events.try_emplace(
      pipe, Registered_Event_Kind::pipe_read, delegate, POLLIN);
  QLJS_ASSERT(inserted);

  this->notify_via_control_pipe();
}

void Event_Loop2_Poll::register_pipe_write(
    Platform_File_Ref pipe, Event_Loop2_Pipe_Write_Delegate* delegate) {
  QLJS_ASSERT(pipe.is_pipe_non_blocking());
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto [_it, inserted] = state->registered_events.try_emplace(
      pipe, Registered_Event_Kind::pipe_write, delegate, POLLOUT);
  QLJS_ASSERT(inserted);

  this->notify_via_control_pipe();
}

void Event_Loop2_Poll::disable_pipe_write(Platform_File_Ref pipe) {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto it = state->registered_events.find(pipe);
  QLJS_ASSERT(it != state->registered_events.end());
  Registered_Event& r = it->second;
  if (!r.enabled) {
    // disable_pipe_write does nothing if already disabled.
    return;
  }

  r.enabled = false;

  this->notify_via_control_pipe();
}

void Event_Loop2_Poll::enable_pipe_write(Platform_File_Ref pipe) {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto it = state->registered_events.find(pipe);
  QLJS_ASSERT(it != state->registered_events.end());
  Registered_Event& r = it->second;
  if (r.enabled) {
    // enable_pipe_write does nothing if already enabled.
    return;
  }

  r.enabled = true;

  this->notify_via_control_pipe();
}

void Event_Loop2_Poll::register_custom_poll(
    Platform_File_Ref file, short events,
    Event_Loop2_Custom_Poll_Delegate* delegate) {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto [_it, inserted] = state->registered_events.try_emplace(
      file, Registered_Event_Kind::custom_poll, delegate, events);
  QLJS_ASSERT(inserted);

  this->notify_via_control_pipe();
}

void Event_Loop2_Poll::request_stop() { this->notify_via_control_pipe(); }

void Event_Loop2_Poll::notify_via_control_pipe() {
  char signal = 0;  // Arbitrary.
  Result<std::size_t, POSIX_File_IO_Error> write_result =
      this->impl_->control_pipe.writer.write(&signal, 1);
  if (!write_result.ok()) {
    if (write_result.error().is_would_block_try_again_error()) {
      // Someone else already notified the event loop. This is fine. See
      // NOTE[Event_Loop2_Poll-control_pipe-non-blocking].
      return;
    } else {
      QLJS_UNIMPLEMENTED();
    }
  }
  QLJS_ASSERT(*write_result == 1);
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
