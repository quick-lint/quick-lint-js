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

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <utility>
#include <vector>

namespace quick_lint_js {
void* byte_buffer::append(size_type byte_count) {
  this->reserve(byte_count);
  return std::exchange(this->cursor_, this->cursor_ + byte_count);
}

void byte_buffer::append_copy(string8_view data) {
  static_assert(sizeof(data[0]) == 1);
  void* out = this->append(data.size());
  std::memcpy(out, data.data(), data.size());
}

byte_buffer::size_type byte_buffer::size() const noexcept {
  size_type total_size = 0;
  for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size() - 1;
       ++chunk_index) {
    const chunk& c = this->chunks_[chunk_index];
    total_size += c.size;
  }
  total_size += this->bytes_used_in_current_chunk();
  return total_size;
}

bool byte_buffer::empty() const noexcept {
  if (this->bytes_used_in_current_chunk() > 0) {
    return false;
  }
  for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size() - 1;
       ++chunk_index) {
    const chunk& c = this->chunks_[chunk_index];
    if (c.size > 0) {
      return false;
    }
  }
  return true;
}

void byte_buffer::copy_to(void* raw_out) const {
  std::byte* out = reinterpret_cast<std::byte*>(raw_out);
  for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size();
       ++chunk_index) {
    const chunk* c = &this->chunks_[chunk_index];
    const std::byte* chunk_begin = c->begin();
    const std::byte* chunk_end;
    if (chunk_index == this->chunks_.size() - 1) {
      chunk_end = this->cursor_;
    } else {
      chunk_end = chunk_begin + c->size;
    }
    out = std::copy(chunk_begin, chunk_end, out);
  }
}

void byte_buffer::reserve(size_type extra_byte_count) {
  if (this->bytes_remaining_in_current_chunk() < extra_byte_count) {
    this->chunks_.back().size = this->bytes_used_in_current_chunk();
    this->add_new_chunk(std::max(default_chunk_size, extra_byte_count));
  }
}

byte_buffer::size_type byte_buffer::bytes_remaining_in_current_chunk() const
    noexcept {
  return narrow_cast<size_type>(this->current_chunk_end_ - this->cursor_);
}

byte_buffer::size_type byte_buffer::bytes_used_in_current_chunk() const
    noexcept {
  return narrow_cast<size_type>(this->cursor_ - this->chunks_.back().begin());
}

void byte_buffer::add_new_chunk(size_type chunk_size) {
  chunk& c = this->chunks_.emplace_back(chunk_size);
  this->cursor_ = c.begin();
  this->current_chunk_end_ = c.end();
}
}
