// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_BYTE_BUFFER_H
#define QUICK_LINT_JS_BYTE_BUFFER_H

#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>
#include <utility>
#include <vector>

namespace quick_lint_js {
class byte_buffer {
 public:
  using size_type = std::size_t;

  static constexpr size_type default_chunk_size = 4096;

  void* append(size_type byte_count);

  template <class Func>
  void append(size_type max_byte_count, Func&& f) {
    this->reserve(max_byte_count);
    size_type bytes_written = f(this->cursor_);
    QLJS_ASSERT(bytes_written <= max_byte_count);
    this->cursor_ += bytes_written;
  }

  template <class T>
  void append_decimal_integer(T value) {
    this->append(integer_string_length<T>, [&](void* out) -> size_type {
      char8* begin = reinterpret_cast<char8*>(out);
      char8* end = write_integer<T>(value, begin);
      return narrow_cast<size_type>(end - begin);
    });
  }

  void append_copy(string8_view data);
  void append_copy(char8 data);

  size_type size() const noexcept;

  bool empty() const noexcept;

  void copy_to(void* raw_out) const;

 private:
  class chunk {
   public:
    explicit chunk() : chunk(default_chunk_size) {}

    explicit chunk(size_type size) : data(new std::byte[size]), size(size) {}

    std::byte* begin() noexcept { return this->data.get(); }

    const std::byte* begin() const noexcept { return this->data.get(); }

    std::byte* end() noexcept { return this->begin() + this->size; }

    std::unique_ptr<std::byte[]> data;
    size_type size;
  };

  void reserve(size_type extra_byte_count);

  size_type bytes_remaining_in_current_chunk() const noexcept;
  size_type bytes_used_in_current_chunk() const noexcept;

  void add_new_chunk(size_type chunk_size);

  std::vector<chunk> chunks_{1};
  std::byte* cursor_ = this->chunks_.back().begin();
  std::byte* current_chunk_end_ = this->chunks_.back().end();
};
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
