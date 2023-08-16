// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/float.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
namespace {
constexpr int default_buffer_size = 4096;
}

Output_Stream::Output_Stream()
    : Output_Stream(/*buffer_size=*/default_buffer_size) {}

Output_Stream::Output_Stream(int buffer_size)
    : buffer_(new Char8[narrow_cast<std::size_t>(buffer_size)]),
      buffer_size_(buffer_size) {
  QLJS_ASSERT(this->buffer_size_ >= this->minimum_buffer_size);
}

Output_Stream::~Output_Stream() = default;

void Output_Stream::append_decimal_float_slow(double value) {
  this->append(max_decimal_float_string_length<double>, [&](Char8* out) -> int {
    Char8* end = write_decimal_float(value, out);
    return narrow_cast<int>(end - out);
  });
}

[[gnu::noinline]] void Output_Stream::append_copy(String8_View data) {
  Char8* out = this->reserve(narrow_cast<int>(data.size()));
  if (out) {
    std::copy(data.begin(), data.end(), out);
  } else {
    this->flush();
    this->flush_impl(data);
  }
}

void Output_Stream::append_copy_small(String8_View data) {
  int data_size = narrow_cast<int>(data.size());
  QLJS_ASSERT(data_size <= this->buffer_size_);
  Char8* out = this->reserve(data_size);
  QLJS_ASSERT(out);
  std::copy(data.begin(), data.end(), out);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")
void Output_Stream::append_copy(Char8 data) {
  Char8* out = this->reserve(1);
  QLJS_ASSERT(out);
  *out = data;
}
QLJS_WARNING_POP

void Output_Stream::flush() {
  this->flush_impl(make_string_view(this->buffer_.get(), this->cursor_));
  this->cursor_ = this->buffer_.get();
}

Char8* Output_Stream::reserve(int byte_count) {
  if (byte_count >= this->minimum_buffer_size &&
      byte_count > this->buffer_size_) {
    return nullptr;
  }
  if (this->buffer_end_ - this->cursor_ < byte_count) {
    this->flush();
  }
  Char8* out = this->cursor_;
  this->cursor_ += byte_count;
  return out;
}

#if !defined(__EMSCRIPTEN__)
File_Output_Stream::File_Output_Stream(Platform_File_Ref file)
    : File_Output_Stream(file, /*buffer_size=*/default_buffer_size) {}

File_Output_Stream::File_Output_Stream(Platform_File_Ref file, int buffer_size)
    : Output_Stream(/*buffer_size=*/buffer_size), file_(file) {}

File_Output_Stream::~File_Output_Stream() { this->flush(); }

File_Output_Stream* File_Output_Stream::get_stdout() {
  static File_Output_Stream stream(Platform_File_Ref::get_stdout());
  return &stream;
}

File_Output_Stream* File_Output_Stream::get_stderr() {
  static File_Output_Stream stream(Platform_File_Ref::get_stderr());
  return &stream;
}

void File_Output_Stream::flush_impl(String8_View data) {
  // TODO(strager): What do we do with partial writes? Currently we only use
  // file_output_stream for TTYs/consoles, blocking pipes, and regular files
  // (stdout/stderr).
  auto write_result = this->file_.write_full(data.data(), data.size());
  if (!write_result.ok()) {
    std::fprintf(
        stderr,
        "fatal: file_output_stream::flush_impl failed to write data: %s\n",
        write_result.error_to_string().c_str());
    std::fflush(stderr);
    QLJS_UNIMPLEMENTED();
  }
}
#endif

Memory_Output_Stream::Memory_Output_Stream() : Output_Stream() {}

Memory_Output_Stream::Memory_Output_Stream(int buffer_size)
    : Output_Stream(/*buffer_size=*/buffer_size) {}

String8 Memory_Output_Stream::get_flushed_string8() const {
  return this->data_;
}

void Memory_Output_Stream::clear() {
  this->flush();
  this->data_.clear();
}

void Memory_Output_Stream::flush_impl(String8_View data) {
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
