// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/fixed-vector.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/io/event-loop.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/thread-name.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/synchronized.h>
#include <vector>

#if defined(_WIN32)
#include <quick-lint-js/port/windows.h>
#endif

#if defined(_WIN32)
namespace quick_lint_js {
namespace {
// NOTE[Event_Loop_Windows-control]: control_event_loop_completion_key is used
// to interrupt the event loop's main thread. Signalling it with
// ::PostQueuedCompletionStatus (with control_event_loop_overlapped as the
// ::OVERLAPPED) causes the ::GetQueuedCompletionStatus loop to recognize a
// change in state.
Event_Loop_Windows::Windows_Completion_Key control_event_loop_completion_key =
    0;
Event_Loop_Windows::Windows_Completion_Key invalid_completion_key =
    static_cast<Event_Loop_Windows::Windows_Completion_Key>(-1);
::OVERLAPPED* control_event_loop_overlapped =
    reinterpret_cast<::OVERLAPPED*>(1);
}

struct Event_Loop_Windows::Registered_Custom {
  Event_Loop_Custom_Windows_IO_Completion_Delegate* delegate;
};

// Spawns a thread which continuously calls ::ReadFile (blocking) on a pipe.
//
// The algorithm of this class has some features proven and disproven using a
// theorem prover. See NOTE[Event_Loop_Windows-Registered_Pipe_Read-proof].
class Event_Loop_Windows::Registered_Pipe_Read {
 public:
  explicit Registered_Pipe_Read(Windows_Handle_File_Ref pipe,
                                Event_Loop_Windows* loop,
                                Event_Loop_Pipe_Read_Delegate* delegate)
      : pipe_(pipe), loop_(loop), delegate_(delegate) {
    QLJS_ASSERT(!pipe.is_pipe_non_blocking());
  }

  // Precondition: Shared_State::registered_pipe_reads lock is held.
  ~Registered_Pipe_Read() { QLJS_ASSERT(!this->started()); }

  // Precondition: Shared_State::registered_pipe_reads lock is held.
  // Precondition: !this->started()
  void start() {
    this->thread_.start([this]() -> void { this->thread_routine(); });
  }

  // Precondition: Shared_State::registered_pipe_reads lock is held.
  // Precondition: this->started()
  // Precondition: this->stop_requested()
  // Postcondition: !this->started()
  void stop_and_wait() {
    // Whoever created this Registered_Pipe_Read must have called start().
    QLJS_ASSERT(this->thread_.joinable());

    // The caller should have guaranteed that the event loop is in the stopping
    // state.
    QLJS_ASSERT(this->is_stop_requested());
    // this->is_stop_requested() causes thread_routine to exit its loop.

    // Cancel a blocking ::ReadFile call. There are three states that the reader
    // thread might be in at this very moment:
    //
    // * ::ReadFile hasn't been called yet. We use ::TerminateThread or set
    //   this->stop_requested_ to interrupt the thread.
    // * ::ReadFile is blocking. We can use ::TerminateThread or ::CancelIoEx to
    //   interrupt the call.
    // * ::ReadFile returned. We use ::TerminateThread or set
    //   this->stop_requested_ to interrupt the thread. This can lead to data
    //   loss; see NOTE[Event_Loop_Windows-Registered_Pipe_Read-data-loss].
    //
    // Because we cannot atomically determine the state of the thread, we use
    // ::TerminateThread to handle all cases.
    //
    // See NOTE[Event_Loop_Windows-Registered_Pipe_Read-terminate] for a proof
    // that ::TerminateThread will not interrupt a user callback.
    if (this->allow_terminate_) {
      this->thread_.terminate();
    }

    this->thread_.join();
  }

  // Returns true if this->start() was called but this->stop_and_wait() was
  // not called.
  //
  // Precondition: Shared_State::registered_pipe_reads lock is held.
  bool started() const { return this->thread_.joinable(); }

  bool is_stop_requested() const { return this->loop_->is_stop_requested(); }

 private:
  void thread_routine() {
    {
      Memory_Output_Stream thread_name;
      thread_name.append_literal(
          u8"Event_Loop_Windows::Registered_Pipe_Read pipe="_sv);
      thread_name.append_fixed_hexadecimal_integer(
          reinterpret_cast<std::uintptr_t>(this->pipe_.get()), 16);
      thread_name.flush();
      set_current_thread_name(thread_name.get_flushed_string8().c_str(),
                              /*short_name=*/u8"EventLoopPipe");
    }

    QLJS_SLOW_ASSERT(!this->pipe_.is_pipe_non_blocking());
    while (!this->is_stop_requested()) {
      std::array<Char8, 65536> buffer;

      this->allow_terminate_ = true;
      if (this->is_stop_requested()) {
        break;
      }
      File_Read_Result read_result =
          this->pipe_.read(buffer.data(), buffer.size());
      this->allow_terminate_ = false;

      if (this->is_stop_requested()) {
        break;
      }

      switch (this->loop_->handle_read_from_pipe_result(
          read_result, Span<const Char8>(buffer), this->pipe_,
          this->delegate_)) {
      case Read_From_Pipe_Result::data:
        continue;
      case Read_From_Pipe_Result::end:
        goto end;
      case Read_From_Pipe_Result::error:
        // FIXME(strager): Should we stop or should we loop?
        continue;
      case Read_From_Pipe_Result::aborted:
        goto aborted;
      }
    }

  end:
  aborted:;
    // TODO(strager): We should signal to the event loop that this thread is
    // done/dying so that the event loop can clean up memory (e.g. join the
    // thread).
  }

  const Windows_Handle_File_Ref pipe_;
  Event_Loop_Windows* const loop_;
  Event_Loop_Pipe_Read_Delegate* const delegate_;

  Thread thread_;
  std::atomic<bool> allow_terminate_ = false;
};

// Mutable state which is across multiple threads thus requires
// synchronization.
struct Event_Loop_Windows::Shared_State {
  Monotonic_Allocator memory{"Event_Loop_Windows"};

  Stable_Hash_Map<Platform_File_Ref, Registered_Pipe_Read>
      registered_pipe_reads;
};

struct Event_Loop_Windows::Impl {
  // io_completion_port can be used by any thread.
  Windows_Handle_File io_completion_port{create_io_completion_port()};

  Synchronized<Shared_State> state;
};

Event_Loop_Windows::Event_Loop_Windows() : impl_(new Impl()) {}

Event_Loop_Windows::~Event_Loop_Windows() { delete this->impl_; }

Windows_Handle_File_Ref Event_Loop_Windows::windows_io_completion_port() const {
  return this->impl_->io_completion_port.ref();
}

void Event_Loop_Windows::run() {
  this->start_new_registered_pipe_reads();
  while (!this->is_stop_requested()) {
    ::DWORD number_of_bytes_transferred = 0;
    ::ULONG_PTR completion_key = invalid_completion_key;
    ::OVERLAPPED* overlapped = nullptr;
    ::BOOL ok = ::GetQueuedCompletionStatus(
        /*CompletionPort=*/this->impl_->io_completion_port.get(),
        /*lpNumberOfBytesTransferred=*/&number_of_bytes_transferred,
        /*lpCompletionKey=*/&completion_key, /*lpOverlapped=*/&overlapped,
        /*dwMilliseconds=*/INFINITE);

    ::DWORD error = ok ? 0 : ::GetLastError();
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
    QLJS_ASSERT(completion_key != invalid_completion_key);

    if (completion_key == control_event_loop_completion_key) {
      // Event loop stop or update was requested. See
      // NOTE[Event_Loop_Windows-control].
      QLJS_ASSERT(overlapped == control_event_loop_overlapped);
      QLJS_ASSERT(error == 0);
      if (this->is_stop_requested()) {
        // Stop the event loop. Do not process other events.
        break;
      }
      this->start_new_registered_pipe_reads();
      continue;
    }

    Registered_Custom* r = reinterpret_cast<Registered_Custom*>(completion_key);
    if (ok) {
      r->delegate->on_custom_windows_io_completion(
          this, completion_key, number_of_bytes_transferred, overlapped);
    } else {
      r->delegate->on_custom_windows_io_completion_error(
          this, completion_key, error, number_of_bytes_transferred, overlapped);
    }
  }

  this->stop_registered_pipe_reads();
}

void Event_Loop_Windows::register_pipe_read(
    Platform_File_Ref pipe, Event_Loop_Pipe_Read_Delegate* delegate) {
  QLJS_ASSERT(!pipe.is_pipe_non_blocking());
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  auto [_it, inserted] =
      state->registered_pipe_reads.try_emplace(pipe, pipe, this, delegate);
  QLJS_ASSERT(inserted);

  // Cause the event loop to call this->start_new_registered_pipe_reads.
  this->notify_via_control();
}

Event_Loop_Windows::Windows_Completion_Key
Event_Loop_Windows::register_custom_windows_io_completion(
    Event_Loop_Custom_Windows_IO_Completion_Delegate* delegate) {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();

  Registered_Custom* r =
      state->memory.new_object<Registered_Custom>(Registered_Custom{
          .delegate = delegate,
      });
  Windows_Completion_Key completion_key =
      reinterpret_cast<Windows_Completion_Key>(r);
  QLJS_ASSERT(completion_key != control_event_loop_completion_key);
  return completion_key;
}

void Event_Loop_Windows::request_stop() { this->notify_via_control(); }

void Event_Loop_Windows::notify_via_control() {
  // See NOTE[Event_Loop_Windows-control].
  ::BOOL ok = ::PostQueuedCompletionStatus(
      /*CompletionPort=*/this->impl_->io_completion_port.get(),
      /*dwNumberOfBytesTransferred=*/0,
      /*dwCompletionKey=*/control_event_loop_completion_key,
      /*lpOverlapped=*/control_event_loop_overlapped);
  if (!ok) {
    QLJS_UNIMPLEMENTED();
  }
}

void Event_Loop_Windows::start_new_registered_pipe_reads() {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();
  for (auto& [pipe, registered_pipe_read] : state->registered_pipe_reads) {
    if (this->is_stop_requested()) {
      return;
    }
    if (!registered_pipe_read.started()) {
      registered_pipe_read.start();
    }
  }
}

void Event_Loop_Windows::stop_registered_pipe_reads() {
  Lock_Ptr<Shared_State> state = this->impl_->state.lock();
  // TODO(strager): To improve performance, stop all threads, then join all
  // threads.
  for (auto& [pipe, registered_pipe_read] : state->registered_pipe_reads) {
    // FIXME(strager): I think this can deadlock. While stopping, a callback
    // might call register_custom_windows_io_completion (for example),
    // re-acquiring the lock. Acquisition will block that thread, preventing
    // stop_and_wait from returning.
    registered_pipe_read.stop_and_wait();
  }
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
