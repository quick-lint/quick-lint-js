// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_BYTE_BUFFER_H
#define QUICK_LINT_JS_BYTE_BUFFER_H

#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>
#include <utility>
#include <vector>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_WRITEV
using byte_buffer_chunk = ::iovec;
#else
struct byte_buffer_chunk {
  std::byte* data;
  std::size_t size;
};
#endif

class byte_buffer_iovec;

// A byte_buffer is a container of char8 which allows efficient appending.
//
// A byte_buffer can be converted into a byte_buffer_iovec for use with the
// POSIX writev(2) syscall.
class byte_buffer {
 public:
  using size_type = std::size_t;

  static constexpr size_type default_chunk_size = 4096;

  explicit byte_buffer();

  byte_buffer(byte_buffer&&);
  byte_buffer& operator=(byte_buffer&&) = delete;  // Not yet implemented.

  ~byte_buffer();

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

  void prepend_copy(string8_view data);

  // Do not call. Create a string8_view explicitly instead.
  void append_copy(const char8* data) = delete;
  void append_copy(char8* data) = delete;
  void prepend_copy(const char8* data) = delete;
  void prepend_copy(char8* data) = delete;

  void clear();

  size_type size() const noexcept;

  bool empty() const noexcept;

  void copy_to(void* raw_out) const;

  // After calling this->to_iovec(), do not call any other member function on
  // this byte_buffer (aside from the destructor).
  byte_buffer_iovec to_iovec() &&;

  template <class Func>
  void enumerate_chunks(Func&& on_chunk) const {
    for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size();
         ++chunk_index) {
      const byte_buffer_chunk* c = &this->chunks_[chunk_index];
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
  void reserve(size_type extra_byte_count);
  void grow(size_type extra_byte_count);
  void update_current_chunk_size() noexcept;
  void remove_current_chunk_if_empty();

  size_type bytes_remaining_in_current_chunk() const noexcept;
  size_type bytes_used_in_current_chunk() const noexcept;

  void add_new_chunk(size_type chunk_size);

  static byte_buffer_chunk allocate_chunk();
  static byte_buffer_chunk allocate_chunk(size_type size);
  static void delete_chunk(byte_buffer_chunk&&);

  std::vector<byte_buffer_chunk> chunks_;
  std::byte* cursor_;
  std::byte* current_chunk_end_;

  friend class byte_buffer_iovec;
};

// A byte_buffer_iovec is a container designed for the POSIX writev(2) syscall.
//
// byte_buffer_iovec provides access to a list of iovec structures.
// (byte_buffer_chunk is an alias for iovec from <sys/uio.h> on supported
// platforms).
//
// byte_buffer_iovec allows efficiently removing bytes from the beginning of the
// container.
class byte_buffer_iovec {
 public:
  using size_type = byte_buffer::size_type;

  explicit byte_buffer_iovec();
  explicit byte_buffer_iovec(std::vector<byte_buffer_chunk>&&);

  byte_buffer_iovec(byte_buffer_iovec&&);
  byte_buffer_iovec& operator=(byte_buffer_iovec&&) = delete;

  ~byte_buffer_iovec();

  const byte_buffer_chunk* iovec() const noexcept;
  int iovec_count() const noexcept;

  bool empty() const noexcept { return this->iovec_count() == 0; }

  // After calling this->append(bb), do not call any other member function on
  // the given byte_buffer (aside from the destructor).
  void append(byte_buffer&&);

  // Remove count bytes from the beginning of this byte_buffer_iovec.
  void remove_front(size_type count);

 private:
  std::vector<byte_buffer_chunk> chunks_;
  std::size_t first_chunk_index_;
  byte_buffer_chunk first_chunk_allocation_;
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
