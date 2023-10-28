// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <utility>
#include <vector>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WRITEV
using Byte_Buffer_Chunk = ::iovec;
#else
struct Byte_Buffer_Chunk {
  std::byte* data;
  std::size_t size;
};
#endif

class Byte_Buffer_IOVec;

// A Byte_Buffer is a container of Char8 which allows efficient appending.
//
// A Byte_Buffer can be converted into a Byte_Buffer_IOVec for use with the
// POSIX writev(2) syscall.
class Byte_Buffer {
 public:
  using Size_Type = std::size_t;

  static constexpr Size_Type default_chunk_size = 4096;

  explicit Byte_Buffer();

  Byte_Buffer(Byte_Buffer&&);
  Byte_Buffer& operator=(Byte_Buffer&&) = delete;  // Not yet implemented.

  ~Byte_Buffer();

  [[nodiscard]] void* append(Size_Type byte_count);

  template <class Func>
  void append(Size_Type max_byte_count, Func&& f) {
    this->reserve(max_byte_count);
    Size_Type bytes_written = f(this->cursor_);
    QLJS_ASSERT(bytes_written <= max_byte_count);
    this->cursor_ += bytes_written;
  }

  template <class T>
  void append_decimal_integer(T value) {
    this->append(integer_string_length<T>, [&](void* out) -> Size_Type {
      Char8* begin = reinterpret_cast<Char8*>(out);
      Char8* end = write_integer<T>(value, begin);
      return narrow_cast<Size_Type>(end - begin);
    });
  }

  void append_copy(String8_View data);
  void append_copy(Char8 data);

  void prepend_copy(String8_View data);

  // Do not call. Create a String8_View explicitly instead.
  void append_copy(const Char8* data) = delete;
  void append_copy(Char8* data) = delete;
  void prepend_copy(const Char8* data) = delete;
  void prepend_copy(Char8* data) = delete;

  void clear();

  Size_Type size() const;

  bool empty() const;

  void copy_to(void* raw_out) const;

  // For testing.
  String8 to_string8() const;

  // After calling this->to_iovec(), do not call any other member function on
  // this byte_buffer (aside from the destructor).
  Byte_Buffer_IOVec to_iovec() &&;

  template <class Func>
  void enumerate_chunks(Func&& on_chunk) const {
    for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size();
         ++chunk_index) {
      const Byte_Buffer_Chunk* c = &this->chunks_[chunk_index];
      const std::byte* c_begin =
#if QLJS_HAVE_WRITEV
          reinterpret_cast<const std::byte*>(c->iov_base);
#else
          c->data;
#endif
      const std::byte* c_end;
      if (chunk_index == this->chunks_.size() - 1) {
        c_end = this->cursor_;
      } else {
        auto c_size =
#if QLJS_HAVE_WRITEV
            c->iov_len;
#else
            c->size;
#endif
        c_end = c_begin + c_size;
      }
      on_chunk(c_begin, c_end);
    }
  }

 private:
  void reserve(Size_Type extra_byte_count);
  void grow(Size_Type extra_byte_count);
  void update_current_chunk_size();
  void remove_current_chunk_if_empty();

  Size_Type bytes_remaining_in_current_chunk() const;
  Size_Type bytes_used_in_current_chunk() const;

  void add_new_chunk(Size_Type chunk_size);

  static Byte_Buffer_Chunk allocate_chunk(Size_Type size);
  static void delete_chunk(Byte_Buffer_Chunk&&);

  std::vector<Byte_Buffer_Chunk> chunks_;
  std::byte* cursor_;
  std::byte* current_chunk_end_;

  friend class Byte_Buffer_IOVec;
};

// A Byte_Buffer_IOVec is a container designed for the POSIX writev(2) syscall.
//
// Byte_Buffer_IOVec provides access to a list of iovec structures.
// (Byte_Buffer_Chunk is an alias for iovec from <sys/uio.h> on supported
// platforms).
//
// Byte_Buffer_IOVec allows efficiently removing bytes from the beginning of the
// container.
class Byte_Buffer_IOVec {
 public:
  using Size_Type = Byte_Buffer::Size_Type;

  explicit Byte_Buffer_IOVec();
  explicit Byte_Buffer_IOVec(std::vector<Byte_Buffer_Chunk>&&);

  Byte_Buffer_IOVec(Byte_Buffer_IOVec&&);
  Byte_Buffer_IOVec& operator=(Byte_Buffer_IOVec&&) = delete;

  ~Byte_Buffer_IOVec();

  const Byte_Buffer_Chunk* iovec() const;
  int iovec_count() const;

  bool empty() const { return this->iovec_count() == 0; }

  // After calling this->append(bb), do not call any other member function on
  // the given byte_buffer (aside from the destructor).
  void append(Byte_Buffer&&);

  // Remove count bytes from the beginning of this Byte_Buffer_IOVec.
  void remove_front(Size_Type count);

 private:
  std::vector<Byte_Buffer_Chunk> chunks_;
  std::size_t first_chunk_index_;
  Byte_Buffer_Chunk first_chunk_allocation_;
};
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
