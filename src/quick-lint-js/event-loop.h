// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_EVENT_LOOP_H
#define QUICK_LINT_JS_EVENT_LOOP_H

#include <array>
#include <cstddef>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>
#include <thread>

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
#if QLJS_HAVE_POLL
                                       const ::pollfd poll_event,
#endif
                                       string8_view data) {
  {cd.get_readable_pipe()};
  {d.append(data)};

  {d.on_filesystem_change()};

#if QLJS_HAVE_POLL
  {d.get_pipe_write_pollfd()};
  {d.on_pipe_write_event(poll_event)};
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
    QLJS_ASSERT(pipe.is_pipe_non_blocking());
#else
    QLJS_ASSERT(!pipe.is_pipe_non_blocking());
#endif
    file_read_result read_result = pipe.read(buffer.data(), buffer.size());
    if (read_result.at_end_of_file) {
      return true;
    } else if (read_result.error_message.has_value()) {
#if QLJS_HAVE_UNISTD_H
      if (errno == EAGAIN) {
#if QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING
        return false;
#else
        QLJS_UNREACHABLE();
#endif
      }
#endif
      QLJS_UNIMPLEMENTED();
    } else {
      QLJS_ASSERT(read_result.bytes_read != 0);
      std::lock_guard<std::mutex> lock(this->user_code_mutex_);
      this->derived().append(string8_view(
          buffer.data(), narrow_cast<std::size_t>(read_result.bytes_read)));
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
      QLJS_ASSERT(pipe.is_pipe_non_blocking());

      std::array<::pollfd, 2> pollfds;
      std::size_t pollfd_count = 0;

      std::size_t read_pipe_index = pollfd_count;
      pollfds[pollfd_count++] =
          ::pollfd{.fd = pipe.get(), .events = POLLIN, .revents = 0};

      std::size_t write_pipe_index = pollfd_count;
      if (std::optional<::pollfd> event =
              this->derived().get_pipe_write_pollfd()) {
        pollfds[pollfd_count++] = *event;
      }

      QLJS_ASSERT(pollfd_count > 0);
      QLJS_ASSERT(pollfd_count <= pollfds.size());
      int rc = ::poll(pollfds.data(), narrow_cast<::nfds_t>(pollfd_count),
                      /*timeout=*/1000);
      if (rc == -1) {
        QLJS_UNIMPLEMENTED();
      }
      // TODO(strager): Watch the filesystem with inotify/queue instead of
      // polling the file system every second.
      QLJS_ASSERT(rc >= 0);

      if (rc == 0) {
        // Timed out.
        this->derived().on_filesystem_change();
      }

      const ::pollfd& read_pipe_event = pollfds[read_pipe_index];
      if (read_pipe_event.revents & POLLIN) {
        continue;
      }
      if (read_pipe_event.revents & POLLERR) {
        QLJS_UNIMPLEMENTED();
      }

      if (pollfd_count > 1) {
        const ::pollfd& write_pipe_event = pollfds[write_pipe_index];
        if (write_pipe_event.revents != 0) {
          this->derived().on_pipe_write_event(write_pipe_event);
        }
      }
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
  };

  explicit windows_event_loop()
      : io_completion_port_(create_io_completion_port()) {}

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
          /*dwMilliseconds=*/1000);
      if (!overlapped) {
        DWORD error = ::GetLastError();
        switch (error) {
        case WAIT_TIMEOUT: {
          std::lock_guard<std::mutex> lock(this->user_code_mutex_);
          this->derived().on_filesystem_change();
          continue;
        }

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
#if QLJS_HAVE_POLL
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
