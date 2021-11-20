// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_EVENT_LOOP_H
#define QUICK_LINT_JS_EVENT_LOOP_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>
#include <thread>

#if QLJS_HAVE_KQUEUE
#include <sys/event.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <Windows.h>
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
                                       string8_view data) {
  {cd.get_readable_pipe()};
  {d.append(data)};

#if QLJS_HAVE_POLL
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

#if defined(_WIN32)
windows_handle_file create_io_completion_port() noexcept;
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
class event_loop_base {
 protected:
  // Returns true when the pipe has closed. Returns false if the pipe might
  // still have data available (now or in the future).
  bool read_from_pipe() {
    // TODO(strager): Pick buffer size intelligently.
    std::array<char8, 1024> buffer;
    platform_file_ref pipe = this->const_derived().get_readable_pipe();
#if QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING
    QLJS_SLOW_ASSERT(pipe.is_pipe_non_blocking());
#else
    QLJS_SLOW_ASSERT(!pipe.is_pipe_non_blocking());
#endif
    file_read_result read_result = pipe.read(buffer.data(), buffer.size());
    if (!read_result.ok()) {
#if QLJS_HAVE_UNISTD_H
      if (read_result.error().error == EAGAIN) {
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
      std::lock_guard<std::mutex> lock(this->user_code_mutex_);
      this->derived().append(string8_view(
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
  std::mutex user_code_mutex_;
};

#if QLJS_HAVE_KQUEUE
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
#endif

#if QLJS_HAVE_POLL
// An event loop using POSIX poll(). See event_loop_base for details.
template <class Derived>
class poll_event_loop : public event_loop_base<Derived> {
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
      platform_file_ref pipe = this->const_derived().get_readable_pipe();
      QLJS_SLOW_ASSERT(pipe.is_pipe_non_blocking());

      std::array< ::pollfd, 3> pollfds;
      std::size_t pollfd_count = 0;

      std::size_t read_pipe_index = pollfd_count++;
      pollfds[read_pipe_index] =
          ::pollfd{.fd = pipe.get(), .events = POLLIN, .revents = 0};

      std::optional<std::size_t> write_pipe_index;
      if (std::optional<posix_fd_file_ref> fd =
              this->derived().get_pipe_write_fd()) {
        write_pipe_index = pollfd_count++;
        pollfds[*write_pipe_index] = ::pollfd{
            .fd = fd->get(),
            .events = POLLOUT,
            .revents = 0,
        };
      }

#if QLJS_HAVE_INOTIFY
      std::optional<std::size_t> inotify_index;
      if (std::optional<posix_fd_file_ref> fd =
              this->derived().get_inotify_fd()) {
        inotify_index = pollfd_count++;
        pollfds[*inotify_index] = ::pollfd{
            .fd = fd->get(),
            .events = POLLIN,
            .revents = 0,
        };
      }
#endif

      QLJS_ASSERT(pollfd_count > 0);
      QLJS_ASSERT(pollfd_count <= pollfds.size());
      int rc = ::poll(pollfds.data(), narrow_cast< ::nfds_t>(pollfd_count),
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
#endif

#if defined(_WIN32)
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

    this->read_pipe_thread_ =
        std::thread([this] { this->run_read_pipe_thread(); });
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
  std::thread read_pipe_thread_;
};
#endif

template <class Derived>
using event_loop =
#if QLJS_HAVE_KQUEUE
    kqueue_event_loop
#elif QLJS_HAVE_POLL
    poll_event_loop
#elif defined(_WIN32)
    windows_event_loop
#else
#error "Unknown platform"
#endif
    <Derived>;

#if defined(_WIN32)
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
#endif
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
