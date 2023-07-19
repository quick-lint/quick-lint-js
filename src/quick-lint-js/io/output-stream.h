// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_IO_OUTPUT_STREAM_H
#define QUICK_LINT_JS_IO_OUTPUT_STREAM_H

#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
// Output_Stream is similar to std::basic_ostream<Char8>.
class Output_Stream {
 public:
  static inline constexpr int minimum_buffer_size = 8;

  explicit Output_Stream();
  explicit Output_Stream(int buffer_size);

  Output_Stream(Output_Stream&&) = delete;
  Output_Stream& operator=(Output_Stream&&) = delete;

  virtual ~Output_Stream();

  template <class Func>
  void append(int max_byte_count, Func&& f) {
    Char8* data = this->reserve(max_byte_count);
    if (data) {
      int bytes_written = f(data);
      QLJS_ASSERT(bytes_written <= max_byte_count);
      this->cursor_ = this->cursor_ - max_byte_count + bytes_written;
    } else {
      // this->buffer_ is too small.
      this->flush();
      std::unique_ptr<Char8[]> temp_buffer(
          new Char8[narrow_cast<std::size_t>(max_byte_count)]);
      int bytes_written = f(temp_buffer.get());
      QLJS_ASSERT(bytes_written <= max_byte_count);
      this->flush_impl(String8_View(temp_buffer.get(),
                                    narrow_cast<std::size_t>(bytes_written)));
    }
  }

  template <class T>
  void append_decimal_integer(T value) {
    this->append(integer_string_length<T>, [&](Char8* out) -> int {
      Char8* end = write_integer<T>(value, out);
      return narrow_cast<int>(end - out);
    });
  }

  [[gnu::noinline]] void append_copy(String8_View data);
  void append_copy(Char8 data);

  // Do not call. Create a String8_View explicitly instead.
  void append_copy(const Char8* data) = delete;
  void append_copy(Char8* data) = delete;

  // Precondition: data.size() <= buffer_size
  void append_copy_small(String8_View data);

  // Optimize appending small string literals.
  void append_literal(String8_View data) {
    if (data.size() <= minimum_buffer_size) {
      this->append_copy_small(data);
    } else {
      this->append_copy(data);
    }
  }

  void flush();

 protected:
  virtual void flush_impl(String8_View) = 0;

 private:
  // Returns nullptr if byte_count > buffer_size.
  Char8* reserve(int byte_count);

  std::unique_ptr<Char8[]> buffer_;
  int buffer_size_;
  Char8* cursor_ = this->buffer_.get();
  Char8* buffer_end_ = this->buffer_.get() + this->buffer_size_;
};

#if defined(__EMSCRIPTEN__)
// No files on the web.
#else
// Non-owning.
class File_Output_Stream final : public Output_Stream {
 public:
  explicit File_Output_Stream(Platform_File_Ref);
  explicit File_Output_Stream(Platform_File_Ref, int buffer_size);

  ~File_Output_Stream() override;

  static File_Output_Stream* get_stdout();
  static File_Output_Stream* get_stderr();

 protected:
  void flush_impl(String8_View) override;

 private:
  Platform_File_Ref file_;
};
#endif

// Designed for testing only.
class Memory_Output_Stream final : public Output_Stream {
 public:
  explicit Memory_Output_Stream();
  explicit Memory_Output_Stream(int buffer_size);

  // Return all of the flushed output, excluding unflushed output.
  String8 get_flushed_string8() const;

  // Remove flushed and unflushed output.
  void clear();

 protected:
  void flush_impl(String8_View) override;

 private:
  String8 data_;
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
