// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <array>
#include <condition_variable>
#include <mutex>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/lsp-pipe-writer.h>
#include <quick-lint-js/narrow-cast.h>
#include <thread>

namespace quick_lint_js {
lsp_pipe_writer::lsp_pipe_writer(platform_file_ref pipe) : pipe_(pipe) {
  QLJS_ASSERT(this->pipe_.is_pipe_non_blocking());
}

lsp_pipe_writer::~lsp_pipe_writer() {
  if (this->flushing_thread_.joinable()) {
    this->stop_ = true;
    this->data_is_pending_.notify_one();
    this->flushing_thread_.join();
  }
}

void lsp_pipe_writer::send_message(const byte_buffer& message) {
  this->write(u8"Content-Length: ");
  this->write_integer(message.size());
  this->write(u8"\r\n\r\n");

  // TODO(strager): Don't copy. Write all the chunks with writev if possible.
  string8 message_string;
  message_string.resize(message.size());
  message.copy_to(message_string.data());
  this->write(message_string);
}

void lsp_pipe_writer::flush() {
  std::unique_lock<std::mutex> lock(this->mutex_);
  QLJS_ASSERT(!this->stop_);
  this->data_is_flushed_.wait(lock, [this] { return this->pending_.empty(); });
}

template <class T>
void lsp_pipe_writer::write_integer(T value) {
  std::array<char8, integer_string_length<T>> buffer;
  char8* end = quick_lint_js::write_integer(value, buffer.data());
  this->write(string8_view(buffer.data(),
                           narrow_cast<std::size_t>(end - buffer.data())));
}

void lsp_pipe_writer::write(string8_view message) {
  std::unique_lock<std::mutex> lock(this->mutex_);
  QLJS_ASSERT(!this->stop_);
  if (this->pending_.empty()) {
    // The current thread has control over this->pipe_.
    string8_view unwritten = this->write_as_much_as_possible_now(message);
    if (!unwritten.empty()) {
      // TODO(strager): Avoid copying.
      this->pending_.append_copy(unwritten);
      lock.unlock();
      this->start_flushing_thread_if_needed();
    }
  } else {
    // The flushing thread has control over this->pipe_.
    this->pending_.append_copy(message);
  }
}

string8_view lsp_pipe_writer::write_as_much_as_possible_now(
    string8_view message) {
  while (!message.empty()) {
    std::optional<int> bytes_written =
        this->pipe_.write(message.data(), narrow_cast<int>(message.size()));
    if (!bytes_written.has_value()) {
#if defined(QLJS_HAVE_UNISTD_H)
      if (errno == EAGAIN) {
        break;
      }
#endif
      QLJS_UNIMPLEMENTED();
    }
#if QLJS_HAVE_WINDOWS_H
    if (*bytes_written == 0) {
      break;
    }
#endif
    message = message.substr(narrow_cast<std::size_t>(*bytes_written));
  }
  return message;
}

void lsp_pipe_writer::start_flushing_thread_if_needed() {
  if (!this->flushing_thread_.joinable()) {
    this->flushing_thread_ =
        std::thread([this] { this->run_flushing_thread(); });
  }
}

void lsp_pipe_writer::run_flushing_thread() {
  std::unique_lock<std::mutex> lock(this->mutex_);
  for (;;) {
    this->data_is_pending_.wait(
        lock, [this] { return this->stop_ || !this->pending_.empty(); });
    if (this->stop_) {
      break;
    }
    QLJS_ASSERT(!this->pending_.empty());

    // TODO(strager): Don't copy. Write all the chunks with writev if possible.
    string8 message_string;
    message_string.resize(this->pending_.size());
    this->pending_.copy_to(message_string.data());
    this->pending_ = byte_buffer();
    string8_view unwritten =
        this->write_as_much_as_possible_now(message_string);
    // TODO(strager): Avoid copying.
    this->pending_.append_copy(unwritten);

    if (this->pending_.empty()) {
      this->data_is_flushed_.notify_one();
    } else {
      lock.unlock();
      this->pipe_.block_until_pipe_is_writeable_or_broken();
      lock.lock();
    }
  }
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
