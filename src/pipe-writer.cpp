// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <condition_variable>
#include <mutex>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/pipe-writer.h>
#include <thread>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#include <unistd.h>
#endif

namespace quick_lint_js {
#if QLJS_PIPE_WRITER_SEPARATE_THREAD
background_thread_pipe_writer::background_thread_pipe_writer(
    platform_file_ref pipe)
    : pipe_(pipe) {
  QLJS_ASSERT(!this->pipe_.is_pipe_non_blocking());
  this->flushing_thread_ = std::thread([this] { this->run_flushing_thread(); });
}

background_thread_pipe_writer::~background_thread_pipe_writer() {
  this->stop_ = true;
  this->data_is_pending_.notify_one();
  this->flushing_thread_.join();
}

void background_thread_pipe_writer::flush() {
  std::unique_lock<std::mutex> lock(this->mutex_);
  QLJS_ASSERT(!this->stop_);
  this->data_is_flushed_.wait(
      lock, [this] { return !this->writing_ && this->pending_.empty(); });
}

void background_thread_pipe_writer::write(byte_buffer&& data) {
  std::unique_lock<std::mutex> lock(this->mutex_);
  QLJS_ASSERT(!this->stop_);
  this->pending_.append(std::move(data));
  this->data_is_pending_.notify_one();
}

void background_thread_pipe_writer::write_all_now_blocking(
    byte_buffer_iovec& data) {
  while (data.iovec_count() != 0) {
#if QLJS_HAVE_WRITEV
    ::ssize_t raw_bytes_written =
        ::writev(this->pipe_.get(), data.iovec(), data.iovec_count());
    if (raw_bytes_written < 0) {
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = narrow_cast<std::size_t>(raw_bytes_written);
#else
    const byte_buffer_chunk& chunk = data.iovec()[0];
    QLJS_ASSERT(chunk.size != 0);  // Writing can hang if given size 0.
    std::optional<int> raw_bytes_written =
        this->pipe_.write(chunk.data, narrow_cast<int>(chunk.size));
    if (!raw_bytes_written.has_value()) {
#if defined(QLJS_HAVE_UNISTD_H)
      QLJS_ASSERT(errno != EAGAIN);
#endif
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = narrow_cast<std::size_t>(*raw_bytes_written);
#endif
    QLJS_ASSERT(bytes_written != 0);
    data.remove_front(bytes_written);
  }
}

void background_thread_pipe_writer::run_flushing_thread() {
  std::unique_lock<std::mutex> lock(this->mutex_);
  for (;;) {
    this->data_is_pending_.wait(
        lock, [this] { return this->stop_ || !this->pending_.empty(); });
    if (this->stop_) {
      break;
    }
    QLJS_ASSERT(!this->pending_.empty());

    {
      byte_buffer_iovec to_write = std::move(this->pending_);
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
non_blocking_pipe_writer::non_blocking_pipe_writer(platform_file_ref pipe)
    : pipe_(pipe) {
  QLJS_ASSERT(this->pipe_.is_pipe_non_blocking());
}

void non_blocking_pipe_writer::flush() {
#if QLJS_HAVE_POLL
  while (std::optional<::pollfd> event = this->get_pollfd()) {
    int rc = ::poll(&*event, 1, /*timeout=*/-1);
    if (rc == -1) {
      QLJS_UNIMPLEMENTED();
    }
    this->on_poll_event(*event);
  }
#else
#error "Unsupported platform"
#endif
}

#if QLJS_HAVE_POLL
std::optional<::pollfd> non_blocking_pipe_writer::get_pollfd() noexcept {
  if (this->pending_.empty()) {
    return std::nullopt;
  } else {
    return ::pollfd{
        .fd = this->pipe_.get(),
        .events = POLLOUT,
        .revents = 0,
    };
  }
}

void non_blocking_pipe_writer::on_poll_event(const ::pollfd& fd) {
  QLJS_ASSERT(fd.revents != 0);
  if (fd.revents & POLLERR) {
    QLJS_UNIMPLEMENTED();
  }
  if (fd.revents & POLLOUT) {
    this->write_as_much_as_possible_now_non_blocking(this->pending_);
  }
}
#endif

void non_blocking_pipe_writer::write(byte_buffer&& data) {
  this->pending_.append(std::move(data));
  this->write_as_much_as_possible_now_non_blocking(this->pending_);
}

void non_blocking_pipe_writer::write_as_much_as_possible_now_non_blocking(
    byte_buffer_iovec& data) {
  QLJS_ASSERT(this->pipe_.is_pipe_non_blocking());
  while (data.iovec_count() != 0) {
#if QLJS_HAVE_WRITEV
    ::ssize_t raw_bytes_written =
        ::writev(this->pipe_.get(), data.iovec(), data.iovec_count());
    if (raw_bytes_written < 0) {
      if (errno == EAGAIN) {
        break;
      }
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = narrow_cast<std::size_t>(raw_bytes_written);
#else
    const byte_buffer_chunk& chunk = data.iovec()[0];
    QLJS_ASSERT(chunk.size != 0);  // Writing can hang if given size 0.
    std::optional<int> raw_bytes_written =
        this->pipe_.write(chunk.data, narrow_cast<int>(chunk.size));
    if (!raw_bytes_written.has_value()) {
#if QLJS_HAVE_UNISTD_H
      if (errno == EAGAIN) {
        break;
      }
#endif
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = narrow_cast<std::size_t>(*raw_bytes_written);
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
