// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/output-stream.h>
#include <quick-lint-js/warning.h>

namespace quick_lint_js {
namespace {
constexpr int default_buffer_size = 4096;
}

output_stream::output_stream()
    : output_stream(/*buffer_size=*/default_buffer_size) {}

output_stream::output_stream(int buffer_size)
    : buffer_(new char8[narrow_cast<std::size_t>(buffer_size)]),
      buffer_size_(buffer_size) {}

output_stream::~output_stream() = default;

void output_stream::append_copy(string8_view data) {
  char8* out = this->reserve(narrow_cast<int>(data.size()));
  if (out) {
    std::copy(data.begin(), data.end(), out);
  } else {
    this->flush();
    this->flush_impl(data);
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")
void output_stream::append_copy(char8 data) {
  char8* out = this->reserve(1);
  QLJS_ASSERT(out);
  *out = data;
}
QLJS_WARNING_POP

void output_stream::flush() {
  this->flush_impl(string8_view(this->buffer_.get(),
                                narrow_cast<std::size_t>(this->cursor_)));
  this->cursor_ = 0;
}

char8* output_stream::reserve(int byte_count) {
  if (byte_count > this->buffer_size_) {
    return nullptr;
  }
  if (this->cursor_ + byte_count >= this->buffer_size_) {
    this->flush();
  }
  char8* out = this->buffer_.get() + this->cursor_;
  this->cursor_ += byte_count;
  return out;
}

#if !defined(__EMSCRIPTEN__)
file_output_stream::file_output_stream(platform_file_ref file)
    : file_output_stream(file, /*buffer_size=*/default_buffer_size) {}

file_output_stream::file_output_stream(platform_file_ref file, int buffer_size)
    : output_stream(/*buffer_size=*/buffer_size), file_(file) {}

file_output_stream::~file_output_stream() { this->flush(); }

file_output_stream* file_output_stream::get_stdout() {
  static file_output_stream stream(platform_file_ref::get_stdout());
  return &stream;
}

file_output_stream* file_output_stream::get_stderr() {
  static file_output_stream stream(platform_file_ref::get_stderr());
  return &stream;
}

void file_output_stream::flush_impl(string8_view data) {
  int data_size = narrow_cast<int>(data.size());
  std::optional<int> written = this->file_.write(data.data(), data_size);
  if (!written.has_value()) {
    QLJS_UNIMPLEMENTED();
  }
  if (*written != data_size) {
    // TODO(strager): What do we do with partial writes? Currently we only use
    // file_output_stream for TTYs/consoles, blocking pipes, and regular files
    // (stdout/stderr).
    QLJS_UNIMPLEMENTED();
  }
}
#endif

memory_output_stream::memory_output_stream() : output_stream() {}

memory_output_stream::memory_output_stream(int buffer_size)
    : output_stream(/*buffer_size=*/buffer_size) {}

string8 memory_output_stream::get_flushed_string8() const {
  return this->data_;
}

void memory_output_stream::clear() {
  this->flush();
  this->data_.clear();
}

void memory_output_stream::flush_impl(string8_view data) {
  this->data_ += data;
}
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
