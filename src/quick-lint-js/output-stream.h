// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_OUTPUT_STREAM_H
#define QUICK_LINT_JS_OUTPUT_STREAM_H

#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
// output_stream is similar to std::basic_ostream<char8>.
class output_stream {
 public:
  explicit output_stream();
  explicit output_stream(int buffer_size);

  output_stream(output_stream&&) = delete;
  output_stream& operator=(output_stream&&) = delete;

  virtual ~output_stream();

  template <class Func>
  void append(int max_byte_count, Func&& f) {
    char8* data = this->reserve(max_byte_count);
    if (data) {
      int bytes_written = f(data);
      QLJS_ASSERT(bytes_written <= max_byte_count);
      this->cursor_ = this->cursor_ - max_byte_count + bytes_written;
    } else {
      // this->buffer_ is too small.
      this->flush();
      std::unique_ptr<char8[]> temp_buffer(
          new char8[narrow_cast<std::size_t>(max_byte_count)]);
      int bytes_written = f(temp_buffer.get());
      QLJS_ASSERT(bytes_written <= max_byte_count);
      this->flush_impl(string8_view(temp_buffer.get(),
                                    narrow_cast<std::size_t>(bytes_written)));
    }
  }

  template <class T>
  void append_decimal_integer(T value) {
    this->append(integer_string_length<T>, [&](char8* out) -> int {
      char8* end = write_integer<T>(value, out);
      return narrow_cast<int>(end - out);
    });
  }

  void append_copy(string8_view data);
  void append_copy(char8 data);

  // Do not call. Create a string8_view explicitly instead.
  void append_copy(char8* data) = delete;

  void flush();

 protected:
  virtual void flush_impl(string8_view) = 0;

 private:
  // Returns nullptr if byte_count > buffer_size.
  char8* reserve(int byte_count);

  std::unique_ptr<char8[]> buffer_;
  int buffer_size_;
  char8* cursor_ = this->buffer_.get();
  char8* buffer_end_ = this->buffer_.get() + this->buffer_size_;
};

#if defined(__EMSCRIPTEN__)
// No files on the web.
#else
// Non-owning.
class file_output_stream final : public output_stream {
 public:
  explicit file_output_stream(platform_file_ref);
  explicit file_output_stream(platform_file_ref, int buffer_size);

  ~file_output_stream() override;

  static file_output_stream* get_stdout();
  static file_output_stream* get_stderr();

 protected:
  void flush_impl(string8_view) override;

 private:
  platform_file_ref file_;
};
#endif

// Designed for testing only.
class memory_output_stream final : public output_stream {
 public:
  explicit memory_output_stream();
  explicit memory_output_stream(int buffer_size);

  // Return all of the flushed output, excluding unflushed output.
  string8 get_flushed_string8() const;

  // Remove flushed and unflushed output.
  void clear();

 protected:
  void flush_impl(string8_view) override;

 private:
  string8 data_;
};
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
