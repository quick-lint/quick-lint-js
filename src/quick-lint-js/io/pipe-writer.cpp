// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <algorithm>
#include <array>
#include <condition_variable>
#include <mutex>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/io/pipe-writer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread-name.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/integer.h>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#include <unistd.h>
#endif

namespace quick_lint_js {
#if QLJS_PIPE_WRITER_SEPARATE_THREAD
Background_Thread_Pipe_Writer::Background_Thread_Pipe_Writer(
    Platform_File_Ref pipe)
    : pipe_(pipe) {
  QLJS_ASSERT(!this->pipe_.is_pipe_non_blocking());
  this->flushing_thread_ = Thread([this] { this->run_flushing_thread(); });
}

Background_Thread_Pipe_Writer::~Background_Thread_Pipe_Writer() {
  this->stop_ = true;
  this->data_is_pending_.notify_one();
  this->flushing_thread_.join();
}

void Background_Thread_Pipe_Writer::flush() {
  std::unique_lock<Mutex> lock(this->mutex_);
  QLJS_ASSERT(!this->stop_);
  this->data_is_flushed_.wait(
      lock, [this] { return !this->writing_ && this->pending_.empty(); });
}

void Background_Thread_Pipe_Writer::write(Byte_Buffer&& data) {
  std::unique_lock<Mutex> lock(this->mutex_);
  QLJS_ASSERT(!this->stop_);
  this->pending_.append(std::move(data));
  this->data_is_pending_.notify_one();
}

void Background_Thread_Pipe_Writer::write_all_now_blocking(
    Byte_Buffer_IOVec& data) {
  while (data.iovec_count() != 0) {
#if QLJS_HAVE_WRITEV
    ::ssize_t raw_bytes_written =
        ::writev(this->pipe_.get(), data.iovec(), data.iovec_count());
    if (raw_bytes_written < 0) {
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = narrow_cast<std::size_t>(raw_bytes_written);
#else
    const Byte_Buffer_Chunk& chunk = data.iovec()[0];
    QLJS_ASSERT(chunk.size != 0);  // Writing can hang if given size 0.
    auto write_result = this->pipe_.write(chunk.data, chunk.size);
    if (!write_result.ok()) {
#if QLJS_HAVE_UNISTD_H
      QLJS_ASSERT(!write_result.error().is_would_block_try_again_error());
#endif
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = *write_result;
#endif
    QLJS_ASSERT(bytes_written != 0);
    data.remove_front(bytes_written);
  }
}

void Background_Thread_Pipe_Writer::run_flushing_thread() {
  {
    Memory_Output_Stream thread_name;
    thread_name.append_literal(u8"Background_Thread_Pipe_Writer pipe="_sv);
#if defined(_WIN32)
    thread_name.append_fixed_hexadecimal_integer(
        reinterpret_cast<std::uintptr_t>(this->pipe_.get()), 16);
#else
    thread_name.append_decimal_integer(this->pipe_.get());
#endif
    thread_name.flush();
    set_current_thread_name(thread_name.get_flushed_string8().c_str(),
                            /*short_name=*/u8"pipewrite");
  }

  std::unique_lock<Mutex> lock(this->mutex_);
  for (;;) {
    this->data_is_pending_.wait(
        lock, [this] { return this->stop_ || !this->pending_.empty(); });
    if (this->stop_) {
      break;
    }
    QLJS_ASSERT(!this->pending_.empty());

    {
      Byte_Buffer_IOVec to_write = std::move(this->pending_);
      this->writing_ = true;
      lock.unlock();
      this->write_all_now_blocking(to_write);
      // Destroy to_write.
    }
    lock.lock();
    this->writing_ = false;
    if (this->pending_.empty()) {
      this->data_is_flushed_.notify_one();
    }
  }
}
#endif

#if !QLJS_PIPE_WRITER_SEPARATE_THREAD
Non_Blocking_Pipe_Writer::Non_Blocking_Pipe_Writer(Platform_File_Ref pipe)
    : pipe_(pipe) {
  QLJS_ASSERT(this->pipe_.is_pipe_non_blocking());
}

void Non_Blocking_Pipe_Writer::flush() {
#if QLJS_HAVE_POLL
  while (this->has_pending_data()) {
    ::pollfd event = {
        .fd = this->get_pipe_fd().get(),
        .events = POLLOUT,
        .revents = 0,
    };
    int rc = ::poll(&event, 1, /*timeout=*/-1);
    if (rc == -1) {
      QLJS_UNIMPLEMENTED();
    }
    this->on_poll_event(event);
  }
#else
#error "Unsupported platform"
#endif
}

bool Non_Blocking_Pipe_Writer::has_pending_data() const {
  return !this->pending_.empty();
}

Platform_File_Ref Non_Blocking_Pipe_Writer::get_pipe_fd() {
  return this->pipe_;
}

#if QLJS_HAVE_KQUEUE
void Non_Blocking_Pipe_Writer::on_poll_event(const struct ::kevent& event) {
  QLJS_ASSERT(narrow_cast<int>(event.ident) != this->pipe_.get());
  if (event.flags & EV_ERROR) {
    QLJS_UNIMPLEMENTED();
  }
  if (event.flags & EV_EOF) {
    QLJS_UNIMPLEMENTED();
  }
  this->write_as_much_as_possible_now_non_blocking(this->pending_);
}
#endif

#if QLJS_HAVE_POLL
void Non_Blocking_Pipe_Writer::on_poll_event(const ::pollfd& fd) {
  QLJS_ASSERT(fd.revents != 0);
  if (fd.revents & POLLERR) {
    QLJS_UNIMPLEMENTED();
  }
  if (fd.revents & POLLOUT) {
    this->write_as_much_as_possible_now_non_blocking(this->pending_);
  }
}
#endif

void Non_Blocking_Pipe_Writer::on_pipe_write_ready() {
  this->write_as_much_as_possible_now_non_blocking(this->pending_);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wmissing-noreturn")
QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=noreturn")

void Non_Blocking_Pipe_Writer::on_pipe_write_end() { QLJS_UNIMPLEMENTED(); }

QLJS_WARNING_POP

void Non_Blocking_Pipe_Writer::write(Byte_Buffer&& data) {
  this->pending_.append(std::move(data));
  this->write_as_much_as_possible_now_non_blocking(this->pending_);
}

void Non_Blocking_Pipe_Writer::write_as_much_as_possible_now_non_blocking(
    Byte_Buffer_IOVec& data) {
  QLJS_ASSERT(this->pipe_.is_pipe_non_blocking());
  while (data.iovec_count() != 0) {
#if QLJS_HAVE_WRITEV
    ::ssize_t raw_bytes_written =
        ::writev(this->pipe_.get(), data.iovec(), data.iovec_count());
    if (raw_bytes_written < 0) {
      POSIX_File_IO_Error error{.error = errno};
      if (error.is_would_block_try_again_error()) {
        break;
      }
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = narrow_cast<std::size_t>(raw_bytes_written);
#else
    const Byte_Buffer_Chunk& chunk = data.iovec()[0];
    QLJS_ASSERT(chunk.size != 0);  // Writing can hang if given size 0.
    auto write_result = this->pipe_.write(chunk.data, chunk.size);
    if (!write_result.ok()) {
#if QLJS_HAVE_UNISTD_H
      if (write_result.error().is_would_block_try_again_error()) {
        break;
      }
#endif
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = *write_result;
#if QLJS_HAVE_WINDOWS_H
    if (bytes_written == 0) {
      break;
    }
#endif
#endif
    QLJS_ASSERT(bytes_written != 0);
    data.remove_front(bytes_written);
  }
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
