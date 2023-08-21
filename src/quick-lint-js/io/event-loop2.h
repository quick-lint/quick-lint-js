// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No event loops on the web.
#else

#include <atomic>
#include <cstdint>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/warning.h>

#if QLJS_HAVE_KQUEUE
struct kevent;
#endif

#if defined(_WIN32)
#include <quick-lint-js/port/windows.h>
#endif

#if defined(_WIN32)
#define QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING 0
#else
#define QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING 1
#endif

#if defined(_WIN32)
// Pipe_Writer manages a background thread.
//
// TODO(strager): Does it make sense to merge Pipe_Writer into the event loop
// classes to unify the APIs?
#define QLJS_EVENT_LOOP2_PIPE_WRITE 0
#else
#define QLJS_EVENT_LOOP2_PIPE_WRITE 1
#endif

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wnon-virtual-dtor")
QLJS_WARNING_IGNORE_GCC("-Wnon-virtual-dtor")

namespace quick_lint_js {
class Event_Loop_Base;

// See Event_Loop_Base::register_pipe_read.
class Event_Loop_Pipe_Read_Delegate {
 public:
  virtual void on_pipe_read_data(Event_Loop_Base*, Platform_File_Ref,
                                 String8_View data) = 0;
  virtual void on_pipe_read_end(Event_Loop_Base*, Platform_File_Ref) = 0;
  virtual void on_pipe_read_error(Event_Loop_Base*, Platform_File_Ref,
                                  Platform_File_IO_Error) = 0;
};

// See Event_Loop_Base::register_pipe_write.
struct Event_Loop_Pipe_Write_Delegate {
 public:
  virtual void on_pipe_write_ready(Event_Loop_Base*, Platform_File_Ref) = 0;
  virtual void on_pipe_write_end(Event_Loop_Base*, Platform_File_Ref) = 0;
};

#if QLJS_HAVE_KQUEUE
// See Event_Loop_Kqueue::register_custom_kqueue.
struct Event_Loop_Custom_Kqueue_Delegate {
  virtual void on_custom_kqueue_events(Event_Loop_Base*,
                                       Span<struct ::kevent> events) = 0;
};
#endif

#if QLJS_HAVE_POLL
// See Event_Loop_Poll::register_custom_poll.
struct Event_Loop_Custom_Poll_Delegate {
  virtual void on_custom_poll_event(Event_Loop_Base*, POSIX_FD_File_Ref,
                                    short revents) = 0;
};
#endif

#if defined(_WIN32)
// See Event_Loop_Windows::register_custom_windows_io_completion.
struct Event_Loop_Custom_Windows_IO_Completion_Delegate {
  virtual void on_custom_windows_io_completion(
      Event_Loop_Base*, ::ULONG_PTR completion_key,
      ::DWORD number_of_bytes_transferred, ::OVERLAPPED* overlapped) = 0;
  virtual void on_custom_windows_io_completion_error(
      Event_Loop_Base*, ::ULONG_PTR completion_key, ::DWORD error,
      ::DWORD number_of_bytes_transferred, ::OVERLAPPED* overlapped) = 0;
};
#endif

// Derived classes of Event_Loop_Base implement callback-based I/O concurrency.
//
// NOTE[Event_Loop-thread-safety]: Event loop implementations are thread-safe.
// Their methods can be called concurrently from any thread. Exception: run()
// can only be called from one thread.
//
// NOTE[Event_Loop-delegate-thread-safety]: An event loop implementation may
// spawn one or more background threads. For a given registration (e.g. for one
// register_pipe_read call), all delegate function calls are serialized, but
// calls might happen on different threads. For different registrations (e.g.
// two register_pipe_read calls, or a register_pipe_read call and a
// register_pipe_write call), delegate functions may be called concurrently from
// multiple threads.
//
// NOTE[Event_Loop-run-state]: There are run three states for an event loop:
// * stopped
// * running
// * stopping
//
// The event loop starts in a stopped state. Legal state transitions:
// * keep_alive():                   stopped  -> stopped
// * un_keep_alive():                stopped  -> stopped
// * run(), ref count =0:            stopped  -> stopped
// * enter run(), ref count >0:      stopped  -> running
// * keep_alive():                   running  -> running
// * un_keep_alive(), ref count >1:  running  -> running
// * un_keep_alive(), ref count =1:  running  -> stopping
// * exit run(), ref count =0:       stopping -> stopped
// Note that there is no legal transition for stopping -> running or for
// running -> stopped.
class Event_Loop_Base {
 public:
  explicit Event_Loop_Base();

  Event_Loop_Base(const Event_Loop_Base&) = delete;
  Event_Loop_Base& operator=(const Event_Loop_Base&) = delete;

  ~Event_Loop_Base() = default;

  // Increment a reference count. This must be called at least once if you want
  // the event loop to do anything useful.
  //
  // Precondition: The event loop is in the stopped state or in the running
  //               state. The event loop must not be in the stopping state. See
  //               NOTE[Event_Loop-run-state].
  void keep_alive();

  // Decrement a reference count. When the reference count reaches zero, the
  // event loop is in the stopping state.
  //
  // NOTE[Event_Loop-stop]: When the event loop is in the stopping state,
  // registered delegate functions might still be called. (This is because
  // another thread might be calling a callback.) However, if un_keep_alive is
  // called during the execution of a registration's callback and causes the
  // event loop to stop, then it is guaranteed that no more callbacks for that
  // registration will be called. (This is because calls of a registration's
  // delegate functions are serialized. See
  // NOTE[Event_Loop-delegate-thread-safety].)
  //
  // After all delegate callbacks are called during the stopping state, run()
  // returns and the event loop enters the stopped state.
  void un_keep_alive();

  // For tests only.
  void stop_event_loop_testing_only();

  // This function can be called at most once. Therefore, this function is not
  // re-entrant or thread-safe.
  //
  // Precondition: The event loop is in the stopped state.
  // Postcondition: The event loop is in the stopped state.
  virtual void run() = 0;

  // Register callbacks for the read end of an anonymous pipe.
  //
  // When data is written on the other end of pipe, the event loop calls
  // Event_Loop_Pipe_Read_Delegate::on_pipe_read_data with (potentially only
  // part of) the data that was written. The event loop performs the read on
  // your behalf.
  //
  // When the other end of pipe is closed, the event loop calls
  // Event_Loop_Pipe_Read_Delegate::on_pipe_read_end.
  //
  // Precondition: pipe is the read end of an anonymous OS pipe (not a Win32
  //               named pipe or a POSIX FIFO).
  // Precondition: pipe was not previously registered.
  // Precondition: If QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING, then pipe is
  //               non-blocking.
  // Precondition: If !QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING, then pipe is
  //               blocking.
  virtual void register_pipe_read(Platform_File_Ref pipe,
                                  Event_Loop_Pipe_Read_Delegate* delegate) = 0;

#if QLJS_EVENT_LOOP2_PIPE_WRITE
  // Register callbacks for the write end of an anonymous pipe.
  //
  // When at least one byte can be written to pipe, the event loop calls
  // Event_Loop_Pipe_Write_Delegate::on_pipe_write_ready. The event loop does
  // not perform any writes on your behalf. If on_pipe_write_ready would do
  // nothing, you should call this->disable_pipe_write(pipe).
  //
  // Calls to on_pipe_write_ready are level-triggered. Unless disabled by
  // disable_pipe_write, if no data is written to pipe, the event loop will
  // repeatedly call on_pipe_write_ready.
  //
  // When the other end of pipe is closed, the event loop calls
  // Event_Loop_Pipe_Write_Delegate::on_pipe_write_end.
  //
  // register_pipe_write is meant to be combined with Pipe_Writer.
  //
  // TODO(strager): Merge Pipe_Writer into these event loop classes. This would
  // remove some #if-s in the interface, simplifying callers. It would also fix
  // HACK[Non_Blocking_Pipe_Writer-enable-disable].
  //
  // Precondition: pipe is the read end of an anonymous OS pipe (not a Win32
  //               named pipe or a POSIX FIFO).
  // Precondition: pipe was not previously registered.
  // Precondition: pipe is non-blocking.
  virtual void register_pipe_write(Platform_File_Ref pipe,
                                   Event_Loop_Pipe_Write_Delegate*) = 0;

  // Prevent Event_Loop_Pipe_Write_Delegate::on_pipe_write_ready from being
  // called. To reduce wasted CPU resources, you should call disable_pipe_write
  // if on_pipe_write_ready would do nothing anyway.
  //
  // Event_Loop_Pipe_Write_Delegate::on_pipe_write_end still might be called if
  // the registration is disabled.
  //
  // If the pipe's registration was already disabled, this call does nothing.
  //
  // Precondition: pipe was previously registered with register_pipe_write.
  virtual void disable_pipe_write(Platform_File_Ref pipe) = 0;

  // Undo the effect of this->disable_pipe_write(pipe).
  //
  // Event_Loop_Pipe_Write_Delegate::on_pipe_write_ready can be called again
  // after enable_pipe_write returns.
  //
  // If the pipe's registration was already enabled, this call does nothing.
  //
  // Precondition: pipe was previously registered with register_pipe_write.
  virtual void enable_pipe_write(Platform_File_Ref pipe) = 0;
#endif

 protected:
  virtual void request_stop() = 0;

  bool is_stop_requested() const;

  enum class Read_From_Pipe_Result {
    // One or more bytes of data was read from the pipe.
    data,
    // End-of-file was reached because the write end of the pipe closed.
    end,
    // Some error occurred. (I've never seen this happen.)
    error,
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
    // The non-blocking read call returned EAGAIN/EWOULDBLOCK.
    no_data,
#else
    // The blocking read call was aborted by a different thread.
    aborted,
#endif
  };

  // When the other end of pipe is closed, calls delegate->on_pipe_read_end then
  // returns Read_From_Pipe_Result::end.
  //
  // If EAGAIN or EWOULDBLOCK occurred while reading, returns
  // Read_From_Pipe_Result::no_data. pipe might still have data available in the
  // future.
  //
  // If data was read, calls delegate->on_pipe_read_data and returns
  // Read_From_Pipe_Result::data. pipe might still have data available (now or
  // in the future).
  //
  // When an error occurred (except for EAGAIN or EWOULDBLOCK), calls
  // delegate->on_pipe_read_error and returns Read_From_Pipe_Result::error. pipe
  // might still have data available (now or in the future).
  //
  // This function potentially blocks if
  // !QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING.
  //
  // Precondition: if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING, then pipe is
  //               non-blocking.
  // Precondition: if !QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING, then pipe is
  //               blocking.
  Read_From_Pipe_Result handle_read_from_pipe_result(
      const File_Read_Result& read_result, Span<const Char8> buffer,
      Platform_File_Ref pipe, Event_Loop_Pipe_Read_Delegate* delegate);

 private:
  std::atomic<int> alive_count_ = 0;
};

#if QLJS_HAVE_KQUEUE
// Event loop implementation based on kqueue (BSDs, macOS).
class Event_Loop_Kqueue final : public Event_Loop_Base {
 private:
  // EVFILT_USER ident used to stop the event loop. See
  // NOTE[Event_Loop_Kqueue-stop].
  static constexpr std::uintptr_t stop_kqueue_user_ident = 0;

 public:
  using Kqueue_Udata = void*;

  // EVFILT_USER ident which is reserved for Event_Loop_Kqueue.
  static constexpr int reserved_kqueue_user_ident = stop_kqueue_user_ident;

  explicit Event_Loop_Kqueue();
  ~Event_Loop_Kqueue();

  POSIX_FD_File_Ref kqueue_fd() const;

  void run() override;

  void register_pipe_read(Platform_File_Ref pipe,
                          Event_Loop_Pipe_Read_Delegate* delegate) override;

  void register_pipe_write(Platform_File_Ref pipe,
                           Event_Loop_Pipe_Write_Delegate* delegate) override;
  void disable_pipe_write(Platform_File_Ref pipe) override;
  void enable_pipe_write(Platform_File_Ref pipe) override;

  // Create a unique value for ::kevent::udata which can be used to register
  // events manually with ::kevent.
  //
  // Example of adding a one-second timer to the event loop:
  //
  //   struct Timer_Delegate : public Event_Loop_Custom_Kqueue_Delegate {
  //     void on_custom_kqueue_events(Event_Loop_Base*,
  //                                  Span<struct ::kevent>) override {
  //       std::println("timer fired!");
  //     }
  //   };
  //   Event_Loop_Kqueue::Kqueue_Udata timer_udata =
  //       loop.register_custom_kqueue(new Timer_Delegate());
  //   struct ::kevent change;
  //   std::uintptr_t timer_id = 0;  // Arbitrary.
  //   EV_SET(change, timer_id, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
  //          NOTE_CRITICAL, 1000, timer_udata);
  //   ::kevent(loop.kqueue_fd(), &change, 1, nullptr, 0, nullptr);
  //
  // When an event occurs, the events received by one call to ::kqueue are
  // batched together and delegate->on_custom_kqueue_events is called once with
  // all of the events. This is so that Configuration_Loader can update its
  // caches once if there is a lot of filesystem activity instead of updating
  // once per individual changed directory. See
  // NOTE[Event_Loop_Kqueue-custom-kqueue-batch] for implementation details.
  //
  // Restrictions:
  //
  // * EVFILT_USER with ident==Event_Loop_Kqueue::reserved_kqueue_user_ident is
  //   reserved and must not be used by a custom event.
  // * Any file descriptor used with register_pipe_read or register_pipe_write
  //   must not be added to the kqueue as a custom event.
  Kqueue_Udata register_custom_kqueue(
      Event_Loop_Custom_Kqueue_Delegate* delegate);

 private:
  enum class Registered_Event_Kind;
  struct Impl;
  struct Registered_Event;
  struct Shared_State;

  void request_stop() override;

  Impl* impl_;
};
#endif

#if QLJS_HAVE_POLL
// Event loop implementation based on the poll syscall (POSIX platforms
// including Linux).
class Event_Loop_Poll final : public Event_Loop_Base {
 public:
  explicit Event_Loop_Poll();
  ~Event_Loop_Poll();

  void run() override;

  void register_pipe_read(Platform_File_Ref pipe,
                          Event_Loop_Pipe_Read_Delegate* delegate) override;

  void register_pipe_write(Platform_File_Ref pipe,
                           Event_Loop_Pipe_Write_Delegate* delegate) override;
  void disable_pipe_write(Platform_File_Ref pipe) override;
  void enable_pipe_write(Platform_File_Ref pipe) override;

  // Register a custom file descriptor with ::poll.
  //
  // When an event is received by ::poll, delegate->on_custom_poll_event is
  // called with file and ::pollfd::revents.
  //
  // Preconditions: pipe must not have been used with register_pipe_read or
  //                register_pipe_write.
  // Preconditions: events must be a valid value in ::pollfd::events for file
  //                according to the ::poll function.
  void register_custom_poll(POSIX_FD_File_Ref file, short events,
                            Event_Loop_Custom_Poll_Delegate* delegate);

 private:
  enum class Registered_Event_Kind;
  struct Impl;
  struct Registered_Event;
  struct Shared_State;

  void request_stop() override;

  void notify_via_control_pipe();

  Impl* impl_;
};
#endif

#if defined(_WIN32)
// Event loop implementation based on Windows I/O Completion Ports and
// background threads.
class Event_Loop_Windows final : public Event_Loop_Base {
 public:
  using Windows_Completion_Key = ::ULONG_PTR;

  explicit Event_Loop_Windows();
  ~Event_Loop_Windows();

  Windows_Handle_File_Ref windows_io_completion_port() const;

  void run() override;

  void register_pipe_read(Platform_File_Ref pipe,
                          Event_Loop_Pipe_Read_Delegate* delegate) override;

  // Create a unique value for lpCompletionKey which can be used to register
  // events with ::CreateIoCompletionPort or ::PostQueuedCompletionStatus.
  //
  // Example of queueing an overlapped write on the event loop:
  //
  //   struct Write_Delegate :
  //       public Event_Loop_Custom_Windows_IO_Completion_Delegate {
  //     void on_custom_windows_io_completion(
  //         Event_Loop_Base*, Windows_Completion_Key,
  //         ::DWORD number_of_bytes_transferred,
  //         ::OVERLAPPED* overlapped) override {
  //       std::println("WriteFile successfully wrote {} bytes",
  //                    number_of_bytes_transferred);
  //     }
  //
  //     void on_custom_windows_io_completion_error(
  //         Event_Loop_Base*, Windows_Completion_Key, ::DWORD error,
  //         ::DWORD, ::OVERLAPPED*) override {
  //       std::println("error occurred: {}", error);
  //     }
  //   };
  //   Event_Loop_Windows::Windows_Completion_Key completion_key =
  //       loop.register_custom_windows_io_completion(new Write_Delegate());
  //
  //   ::HANDLE file = ...;
  //   ::HANDLE file_completion_port = ::CreateIoCompletionPort(
  //       file, loop.io_completion_port(), completion_key, 0);
  //
  //   // Start an asynchronous (overlapped) WriteFile.
  //   // Note that data_to_write and overlapped must be live for the duration
  //   // of the overlapped I/O.
  //   std::vector<char> *data_to_write = new std::vector<char>(...);
  //   ::OVERLAPPED overlapped = new ::OVERLAPPED();
  //   overlapped.hEvent = nullptr;
  //   ::WriteFile(file, data_to_write->data(), data_to_write->size(), nullptr,
  //               overlapped);
  //
  // When an event occurs, then delegate->on_custom_windows_io_completion is
  // called if the event is a success, else
  // delegate->on_custom_windows_io_completion_error is called if the event is
  // an error.
  Windows_Completion_Key register_custom_windows_io_completion(
      Event_Loop_Custom_Windows_IO_Completion_Delegate* delegate);

 private:
  class Registered_Pipe_Read;
  enum class Registered_Event_Kind;
  struct Impl;
  struct Registered_Custom;
  struct Shared_State;

  void request_stop() override;
  void notify_via_control();

  void start_new_registered_pipe_reads();
  void stop_registered_pipe_reads();

  Impl* impl_;
};
#endif

#if defined(_WIN32)
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
#endif

// The best Event_Loop_Base implementation for the current platform.
using Event_Loop =
#if QLJS_HAVE_KQUEUE
    Event_Loop_Kqueue
#elif QLJS_HAVE_POLL
    Event_Loop_Poll
#elif defined(_WIN32)
    Event_Loop_Windows
#else
#error "Unknown platform"
#endif
    ;
}

QLJS_WARNING_POP

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
