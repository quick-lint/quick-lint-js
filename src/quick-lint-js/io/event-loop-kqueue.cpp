// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/fixed-vector.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/io/event-loop.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/kqueue.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/synchronized.h>
#include <vector>

#if QLJS_HAVE_KQUEUE
namespace quick_lint_js {
namespace {
POSIX_FD_File_Ref get_fd(struct ::kevent& event) {
  return POSIX_FD_File_Ref(narrow_cast<int>(event.ident));
}

// Returns -1 on failure.
[[nodiscard]] int kqueue_add_changes(POSIX_FD_File_Ref kqueue_fd,
                                     Span<const struct ::kevent> changes) {
  return ::kevent(kqueue_fd.get(),
                  /*changelist=*/changes.data(),
                  /*nchanges=*/narrow_cast<int>(changes.size()),
                  /*eventlist=*/nullptr,
                  /*nevents=*/0,
                  /*timeout=*/nullptr);
}
}

enum class Event_Loop_Kqueue::Registered_Event_Kind {
  // Event_Loop_Pipe_Read_Delegate
  pipe_read,
  // Event_Loop_Pipe_Write_Delegate
  pipe_write,
  // Event_Loop_Custom_Kqueue_Delegate
  custom_kqueue,
};

struct Event_Loop_Kqueue::Registered_Event {
  // These don't change thus do not need to be protected by a lock.
  const Registered_Event_Kind kind;
  void* const delegate;

  // If false, the call to kevent with EV_ADD failed or hasn't happened yet.
  // Protected by Event_Loop_Kqueue::Impl::state's lock.
  bool registered_with_kqueue = false;

  // Protected by Event_Loop_Kqueue::Impl::state's lock.
  bool enabled = true;
};

// Mutable state which is across multiple threads thus requires synchronization.
struct Event_Loop_Kqueue::Shared_State {
  Monotonic_Allocator memory{"Event_Loop_Kqueue"};

  // For most events, udata is a pointer to a Registered_Event in
  // registered_events.
  //
  // Custom kqueue registrations, which are not associated with file
  // descriptors, are stored in this->memory. See
  // NOTE[Event_Loop_Kqueue-custom-kqueue].
  //
  // TODO(strager): Use this->memory for registered_events.
  Stable_Hash_Map<Platform_File_Ref, Registered_Event> registered_events;

  // List of on_pipe_write_end calls which need to be made. See
  // NOTE[Event_Loop_Kqueue-FreeBSD-EPIPE].
  Linked_Vector<Platform_File_Ref> pending_pipe_write_ends{&this->memory};
};

struct Event_Loop_Kqueue::Impl {
  // kqueue_fd can be used by any thread.
  POSIX_FD_File kqueue_fd{::kqueue()};

  Synchronized<Shared_State> state;
};

Event_Loop_Kqueue::Event_Loop_Kqueue() : impl_(new Impl()) {
  QLJS_ALWAYS_ASSERT(this->impl_->kqueue_fd.valid());

  // Allocate resources for the control timer. See
  // NOTE[Event_Loop_Kqueue-control].
  Fixed_Vector<struct ::kevent, 1> changes;
  EV_SET(&changes.emplace_back(), this->control_kqueue_timer_ident,
         EVFILT_TIMER, EV_ADD | EV_DISABLE, 0, 0, nullptr);
  int rc = kqueue_add_changes(this->kqueue_fd(),
                              Span<const struct ::kevent>(changes));
  QLJS_ALWAYS_ASSERT(rc == 0);
}

Event_Loop_Kqueue::~Event_Loop_Kqueue() { delete this->impl_; }

POSIX_FD_File_Ref Event_Loop_Kqueue::kqueue_fd() const {
  return this->impl_->kqueue_fd.ref();
}

void Event_Loop_Kqueue::run() {
  while (!this->is_stop_requested()) {
    this->handle_pending_custom_events();

    Fixed_Vector<struct ::kevent, 10> events;
    events.resize(events.capacity());

    int rc = ::kevent(this->impl_->kqueue_fd.get(),
                      /*changelist=*/nullptr,
                      /*nchanges=*/0,
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
    events.resize(narrow_cast<Fixed_Vector_Size>(rc));

    // NOTE[Event_Loop_Kqueue-custom-kqueue-batch]: Put events with the same
    // Registration together so we can batch custom events together.
    //
    // Also, put our control event first so it is processed as soon as possible.
    // See NOTE[Event_Loop_Kqueue-control].
    sort(events, [](struct ::kevent& a, struct ::kevent& b) -> bool {
      return std::less<>()(a.udata, b.udata);
    });

    for (Fixed_Vector_Size i = 0; i < events.size();) {
      struct ::kevent& event = events[i];

      if (event.udata == nullptr) {
        // Event loop stop was requested. See NOTE[Event_Loop_Kqueue-control].
        QLJS_ASSERT(event.filter == EVFILT_TIMER);
        QLJS_ASSERT(event.ident == this->control_kqueue_timer_ident);
        if (!this->is_stop_requested()) {
          // Somebody called this->keep_alive(), or we were signalled to check
          // pending_pipe_write_ends. Don't stop the event loop; ignore this
          // event.
          i += 1;
          continue;
        }
        // Stop the event loop. Do not process other events.
        return;
      }

      Registered_Event* r = static_cast<Registered_Event*>(event.udata);
      switch (r->kind) {
      case Registered_Event_Kind::pipe_read: {
        QLJS_ASSERT(event.filter == EVFILT_READ);
        POSIX_FD_File_Ref pipe = get_fd(event);
        std::array<Char8, 65536> buffer;
        QLJS_SLOW_ASSERT(pipe.is_pipe_non_blocking());
        File_Read_Result read_result = pipe.read(buffer.data(), buffer.size());
        this->handle_read_from_pipe_result(
            read_result, Span<const Char8>(buffer), pipe,
            static_cast<Event_Loop_Pipe_Read_Delegate*>(r->delegate));
        // FIXME(strager): On Read_From_Pipe_Result::end, do we need to
        // EV_DISABLE to prevent more events?
        i += 1;
        break;
      }

      case Registered_Event_Kind::pipe_write:
        QLJS_ASSERT(event.filter == EVFILT_WRITE);
        if ((event.flags & EV_EOF) == EV_EOF) {
          static_cast<Event_Loop_Pipe_Write_Delegate*>(r->delegate)
              ->on_pipe_write_end(this, get_fd(event));
        } else {
          static_cast<Event_Loop_Pipe_Write_Delegate*>(r->delegate)
              ->on_pipe_write_ready(this, get_fd(event));
        }
        i += 1;
        break;

      case Registered_Event_Kind::custom_kqueue: {
        // Instead of calling on_custom_kqueue_events once per event, group
        // events together and call on_custom_kqueue_events only once per turn
        // of the event loop.
        //
        // The list of events was sorted on udata (see
        // NOTE[Event_Loop_Kqueue-custom-kqueue-batch]), so all events with the
        // same udata should be adjacent in the events array.
        struct ::kevent* batch_end = std::find_if(
            &events[i], events.end(),
            [&](struct ::kevent& e) -> bool { return e.udata != event.udata; });
        Span<struct ::kevent> batch(&events[i], batch_end);
        QLJS_ASSERT(!batch.empty());
        static_cast<Event_Loop_Custom_Kqueue_Delegate*>(r->delegate)
            ->on_custom_kqueue_events(this, batch);

        i += batch.size();
        break;
      }
      }

      if (this->is_stop_requested()) {
        // If a stop was requested by a callback, avoid calling other callbacks.
        // This is not strictly necessary.
        return;
      }
    }
  }
}

void Event_Loop_Kqueue::register_pipe_read(
    Platform_File_Ref pipe, Event_Loop_Pipe_Read_Delegate* delegate) {
  QLJS_ASSERT(pipe.is_pipe_non_blocking());
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto [it, inserted] = state->registered_events.try_emplace(
      pipe, Registered_Event{
                .kind = Registered_Event_Kind::pipe_read,
                .delegate = delegate,
            });
  QLJS_ASSERT(inserted);
  Registered_Event& r = it->second;

  Fixed_Vector<struct ::kevent, 1> changes;
  EV_SET(&changes.emplace_back(), pipe.get(), EVFILT_READ, EV_ADD, 0, 0, &r);
  int rc = kqueue_add_changes(this->kqueue_fd(),
                              Span<const struct ::kevent>(changes));
  QLJS_ALWAYS_ASSERT(rc == 0);
  r.registered_with_kqueue = true;
}

void Event_Loop_Kqueue::register_pipe_write(
    Platform_File_Ref pipe, Event_Loop_Pipe_Write_Delegate* delegate) {
  QLJS_ASSERT(pipe.is_pipe_non_blocking());
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto [it, inserted] = state->registered_events.try_emplace(
      pipe, Registered_Event{
                .kind = Registered_Event_Kind::pipe_write,
                .delegate = delegate,
            });
  QLJS_ASSERT(inserted);
  Registered_Event& r = it->second;

  Fixed_Vector<struct ::kevent, 1> changes;
  EV_SET(&changes.emplace_back(), pipe.get(), EVFILT_WRITE, EV_ADD, 0, 0, &r);
  int rc = kqueue_add_changes(this->kqueue_fd(),
                              Span<const struct ::kevent>(changes));
  if (rc == -1) {
    if (errno == EPIPE) {
      // NOTE[Event_Loop_Kqueue-FreeBSD-EPIPE]: If the other end of a pipe is
      // closed, then instead of adding the pipe to the kqueue and immediately
      // enqueueing an event, FreeBSD fails to add the pipe to the kqueue at
      // all. Detect this and schedule a callback for on_pipe_write_end.
      state->pending_pipe_write_ends.emplace_back(pipe);
      this->notify_via_control();
    } else {
      QLJS_UNIMPLEMENTED();
    }
  } else {
    QLJS_ASSERT(rc == 0);
    r.registered_with_kqueue = true;
  }
}

void Event_Loop_Kqueue::disable_pipe_write(Platform_File_Ref pipe) {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto it = state->registered_events.find(pipe);
  QLJS_ASSERT(it != state->registered_events.end());
  Registered_Event& r = it->second;
  if (!r.enabled) {
    // disable_pipe_write does nothing if already disabled.
    return;
  }

  if (r.registered_with_kqueue) {
    Fixed_Vector<struct ::kevent, 1> changes;
    EV_SET(&changes.emplace_back(), pipe.get(), EVFILT_WRITE,
           EV_ADD | EV_DISABLE, 0, 0, &r);
    int rc = kqueue_add_changes(this->kqueue_fd(),
                                Span<const struct ::kevent>(changes));
    QLJS_ALWAYS_ASSERT(rc == 0);
  } else {
    // Initial registration failed with EPIPE (see
    // NOTE[Event_Loop_Kqueue-FreeBSD-EPIPE]).
    // Event_Loop_Kqueue::handle_pending_custom_events will check r.enabled, so
    // we don't need to do anything special here.
  }

  r.enabled = false;
}

void Event_Loop_Kqueue::enable_pipe_write(Platform_File_Ref pipe) {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto it = state->registered_events.find(pipe);
  QLJS_ASSERT(it != state->registered_events.end());
  Registered_Event& r = it->second;
  if (r.enabled) {
    // enable_pipe_write does nothing if already enabled.
    return;
  }

  if (r.registered_with_kqueue) {
    Fixed_Vector<struct ::kevent, 1> changes;
    EV_SET(&changes.emplace_back(), pipe.get(), EVFILT_WRITE,
           EV_ADD | EV_ENABLE, 0, 0, &r);
    int rc = kqueue_add_changes(this->kqueue_fd(),
                                Span<const struct ::kevent>(changes));
    QLJS_ALWAYS_ASSERT(rc == 0);
  } else {
    // Initial registration failed with EPIPE (see
    // NOTE[Event_Loop_Kqueue-FreeBSD-EPIPE]). If on_pipe_write_end wasn't
    // called already, the pipe should be in pending_pipe_write_ends. Tell the
    // event loop to look in pending_pipe_write_ends and call on_pipe_write_end
    // if necessary.
    this->notify_via_control();
  }

  r.enabled = true;
}

Event_Loop_Kqueue::Kqueue_Udata Event_Loop_Kqueue::register_custom_kqueue(
    Event_Loop_Custom_Kqueue_Delegate* delegate) {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  // NOTE[Event_Loop_Kqueue-custom-kqueue]:
  Registered_Event* r =
      state->memory.new_object<Registered_Event>(Registered_Event{
          .kind = Registered_Event_Kind::custom_kqueue,
          .delegate = delegate,
      });
  return r;
}

void Event_Loop_Kqueue::handle_pending_custom_events() {
  Shared_State* state = this->impl_->state.get_without_lock_unsafe();
  std::unique_lock<Mutex> lock(*this->impl_->state.get_mutex_unsafe());

  Linked_Vector<Platform_File_Ref> new_pending_pipe_write_ends(&state->memory);

  while (!state->pending_pipe_write_ends.empty()) {
    if (this->is_stop_requested()) {
      // If a stop was requested by a callback or another thread, avoid calling
      // more callbacks. This is not strictly necessary.
      break;
    }

    Platform_File_Ref pipe = state->pending_pipe_write_ends.back();
    state->pending_pipe_write_ends.pop_back();

    auto it = state->registered_events.find(pipe);
    if (it == state->registered_events.end()) {
      QLJS_DEBUG_LOG(
          "Event_Loop_Kqueue: on_pipe_write_end was queued for fd=%d but it is "
          "not registered\n",
          pipe.get());
    } else {
      Registered_Event& r = it->second;
      QLJS_ASSERT(r.kind == Registered_Event_Kind::pipe_write);
      Event_Loop_Pipe_Write_Delegate* delegate =
          static_cast<Event_Loop_Pipe_Write_Delegate*>(r.delegate);

      if (r.enabled) {
        lock.unlock();
        delegate->on_pipe_write_end(this, pipe);
        lock.lock();
      } else {
        // The pipe write might get enabled later, so allow a future turn of the
        // event loop to process this pipe.
        new_pending_pipe_write_ends.emplace_back(pipe);
      }
    }
  }

  QLJS_ASSERT(lock.owns_lock());
  new_pending_pipe_write_ends.for_each([&](Platform_File_Ref pipe) {
    state->pending_pipe_write_ends.emplace_back(pipe);
  });
}

void Event_Loop_Kqueue::request_stop() { this->notify_via_control(); }

void Event_Loop_Kqueue::notify_via_control() {
  // NOTE[Event_Loop_Kqueue-control]: To interrupt the event loop, we create a
  // timer (EVFILT_TIMER). The timer has a unique ID which will be caught by our
  // loop handling returned events. When the event loop sees this user event, it
  // checks for various conditions, such as a requested stop of the event loop.
  //
  // To guarantee that creating the timer succeeds, we create the timer when we
  // create the event loop and only *enable* it when requesting a stop of the
  // event loop. Enabling an existing timer should always succeed.
  //
  // udata is set to nullptr so it sorts first (see
  // NOTE[Event_Loop_Kqueue-custom-kqueue-batch]).
  //
  // NOTE(strager): We use an EVFILT_TIMER instead of an EVFILT_USER for
  // portability. OpenBSD does not support EVFILT_USER.
  //
  // TODO(strager): Write a test which guarantees that the stop event is
  // processed before other events.
  Fixed_Vector<struct ::kevent, 1> changes;
  EV_SET(&changes.emplace_back(), this->control_kqueue_timer_ident,
         EVFILT_TIMER, EV_ADD | EV_ENABLE | EV_ONESHOT, NOTE_TRIGGER, 0,
         nullptr);
  int rc = kqueue_add_changes(this->kqueue_fd(),
                              Span<const struct ::kevent>(changes));
  QLJS_ALWAYS_ASSERT(rc == 0);
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
