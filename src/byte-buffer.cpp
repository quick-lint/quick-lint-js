// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <utility>
#include <vector>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#endif

namespace quick_lint_js {
namespace {
const std::byte* chunk_begin(const byte_buffer_chunk& c) noexcept {
#if QLJS_HAVE_WRITEV
  return reinterpret_cast<const std::byte*>(c.iov_base);
#else
  return c.data;
#endif
}

std::byte* chunk_begin(byte_buffer_chunk& c) noexcept {
#if QLJS_HAVE_WRITEV
  return reinterpret_cast<std::byte*>(c.iov_base);
#else
  return c.data;
#endif
}

byte_buffer::size_type chunk_size(const byte_buffer_chunk& c) noexcept {
#if QLJS_HAVE_WRITEV
  return c.iov_len;
#else
  return c.size;
#endif
}

byte_buffer::size_type& chunk_size(byte_buffer_chunk& c) noexcept {
#if QLJS_HAVE_WRITEV
  return c.iov_len;
#else
  return c.size;
#endif
}

const std::byte* chunk_end(const byte_buffer_chunk& c) noexcept {
  return chunk_begin(c) + chunk_size(c);
}

std::byte* chunk_end(byte_buffer_chunk& c) noexcept {
  return chunk_begin(c) + chunk_size(c);
}

byte_buffer_chunk make_chunk(const void* data, byte_buffer::size_type size) {
#if QLJS_HAVE_WRITEV
  return byte_buffer_chunk{.iov_base = const_cast<void*>(data),
                           .iov_len = size};
#else
  return byte_buffer_chunk{
      .data = const_cast<std::byte*>(reinterpret_cast<const std::byte*>(data)),
      .size = size};
#endif
}

}

byte_buffer::byte_buffer() { this->add_new_chunk(this->default_chunk_size); }

byte_buffer::byte_buffer(byte_buffer&&) = default;

byte_buffer::~byte_buffer() {
  for (byte_buffer_chunk& c : this->chunks_) {
    this->delete_chunk(std::move(c));
  }
}

void* byte_buffer::append(size_type byte_count) {
  this->reserve(byte_count);
  return std::exchange(this->cursor_, this->cursor_ + byte_count);
}

void byte_buffer::append_copy(string8_view data) {
  static_assert(sizeof(data[0]) == 1);
  void* out = this->append(data.size());
  std::memcpy(out, data.data(), data.size());
}

void byte_buffer::append_copy(char8 data) {
  void* out = this->append(1);
  std::memcpy(out, &data, 1);
}

void byte_buffer::prepend_copy(string8_view data) {
  // TODO(strager): As an optimization, reserve one slot in the vector for
  // prepending. (We expect one prepend per byte_buffer by lsp_pipe_writer.)
  // TODO(strager): If there's space in the current chunk, use it instead of
  // making a new chunk.

  const std::byte* data_bytes = reinterpret_cast<const std::byte*>(data.data());

  byte_buffer_chunk prefix_chunk = this->allocate_chunk(data.size());
  const std::byte* end =
      std::copy_n(data_bytes, data.size(), chunk_begin(prefix_chunk));
  QLJS_ASSERT(end == chunk_end(prefix_chunk));

  this->chunks_.insert(this->chunks_.begin(), std::move(prefix_chunk));
}

void byte_buffer::clear() {
  auto begin = this->chunks_.begin();
  auto end = std::prev(this->chunks_.end());
  for (auto it = begin; it != end; ++it) {
    this->delete_chunk(std::move(*it));
  }
  this->chunks_.erase(begin, end);
  QLJS_ASSERT(this->chunks_.size() == 1);
  this->cursor_ = chunk_begin(this->chunks_.back());
}

byte_buffer::size_type byte_buffer::size() const noexcept {
  size_type total_size = 0;
  for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size() - 1;
       ++chunk_index) {
    const byte_buffer_chunk& c = this->chunks_[chunk_index];
    total_size += chunk_size(c);
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
    const byte_buffer_chunk& c = this->chunks_[chunk_index];
    if (chunk_size(c) > 0) {
      return false;
    }
  }
  return true;
}

void byte_buffer::copy_to(void* raw_out) const {
  std::byte* out = reinterpret_cast<std::byte*>(raw_out);
  for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size();
       ++chunk_index) {
    const byte_buffer_chunk* c = &this->chunks_[chunk_index];
    const std::byte* c_begin = chunk_begin(*c);
    const std::byte* c_end;
    if (chunk_index == this->chunks_.size() - 1) {
      c_end = this->cursor_;
    } else {
      c_end = chunk_end(*c);
    }
    out = std::copy(c_begin, c_end, out);
  }
}

byte_buffer_iovec byte_buffer::to_iovec() && {
  this->update_current_chunk_size();
  this->remove_current_chunk_if_empty();
  return byte_buffer_iovec(std::move(this->chunks_));
}

void byte_buffer::reserve(size_type extra_byte_count) {
  if (this->bytes_remaining_in_current_chunk() < extra_byte_count) {
    this->update_current_chunk_size();
    this->remove_current_chunk_if_empty();
    this->add_new_chunk(std::max(default_chunk_size, extra_byte_count));
  }
}

void byte_buffer::update_current_chunk_size() noexcept {
  QLJS_ASSERT(!this->chunks_.empty());
  chunk_size(this->chunks_.back()) = this->bytes_used_in_current_chunk();
}

void byte_buffer::remove_current_chunk_if_empty() {
  QLJS_ASSERT(!this->chunks_.empty());
  if (chunk_size(this->chunks_.back()) == 0) {
    this->delete_chunk(std::move(this->chunks_.back()));
    this->chunks_.pop_back();
  }
}

byte_buffer::size_type byte_buffer::bytes_remaining_in_current_chunk() const
    noexcept {
  return narrow_cast<size_type>(this->current_chunk_end_ - this->cursor_);
}

byte_buffer::size_type byte_buffer::bytes_used_in_current_chunk() const
    noexcept {
  return narrow_cast<size_type>(this->cursor_ -
                                chunk_begin(this->chunks_.back()));
}

void byte_buffer::add_new_chunk(size_type chunk_size) {
  byte_buffer_chunk& c =
      this->chunks_.emplace_back(this->allocate_chunk(chunk_size));
  this->cursor_ = chunk_begin(c);
  this->current_chunk_end_ = chunk_end(c);
}

byte_buffer_chunk byte_buffer::allocate_chunk() {
  return allocate_chunk(default_chunk_size);
}

byte_buffer_chunk byte_buffer::allocate_chunk(size_type size) {
  // See corresponding deallocation in delete_chunk.
  return make_chunk(new std::byte[size], size);
}

void byte_buffer::delete_chunk(byte_buffer_chunk&& c) {
  // See corresponding allocation in allocate_chunk.
  delete[] chunk_begin(c);
}

byte_buffer_iovec::byte_buffer_iovec(std::vector<byte_buffer_chunk>&& chunks)
    : chunks_(std::move(chunks)), first_chunk_index_(0) {
  if (this->chunks_.empty()) {
    this->first_chunk_allocation_ = make_chunk(nullptr, 0);
  } else {
    this->first_chunk_allocation_ = this->chunks_.front();
  }

  for (const byte_buffer_chunk& c : this->chunks_) {
    QLJS_ASSERT(chunk_size(c) > 0);
  }
}

byte_buffer_iovec::byte_buffer_iovec(byte_buffer_iovec&&) = default;

byte_buffer_iovec::~byte_buffer_iovec() {
  if (this->first_chunk_index_ != this->chunks_.size()) {
    // The first chunk might have been split. Deallocate the original
    // allocation, not the advanced pointer.
    byte_buffer::delete_chunk(std::move(this->first_chunk_allocation_));
    for (std::size_t i = this->first_chunk_index_ + 1; i < this->chunks_.size();
         ++i) {
      byte_buffer::delete_chunk(std::move(this->chunks_[i]));
    }
  }
}

const byte_buffer_chunk* byte_buffer_iovec::iovec() const noexcept {
  return this->chunks_.data() + this->first_chunk_index_;
}

int byte_buffer_iovec::iovec_count() const noexcept {
  return narrow_cast<int>(this->chunks_.size() - this->first_chunk_index_);
}

void byte_buffer_iovec::append(byte_buffer&& other) {
  if (other.empty()) {
    return;
  }

  bool was_empty = this->first_chunk_index_ == this->chunks_.size();

  other.update_current_chunk_size();
  this->chunks_.insert(this->chunks_.end(), other.chunks_.begin(),
                       other.chunks_.end());
  other.chunks_.clear();

  if (was_empty) {
    QLJS_ASSERT(this->first_chunk_index_ < this->chunks_.size());
    this->first_chunk_allocation_ = this->chunks_[this->first_chunk_index_];
  }
}

void byte_buffer_iovec::remove_front(size_type size) {
  while (size != 0) {
    QLJS_ASSERT(this->first_chunk_index_ != this->chunks_.size());

    byte_buffer_chunk& c = this->chunks_[this->first_chunk_index_];
    size_type c_size = chunk_size(c);
    if (size < c_size) {
      // Split this chunk.
      c = make_chunk(
          /*data=*/chunk_begin(c) + size,
          /*size=*/c_size - size);
      size = 0;
    } else {
      // Delete this entire chunk. It may have been split, so delete the
      // original, unsplit chunk.
      byte_buffer::delete_chunk(std::move(this->first_chunk_allocation_));
      size -= c_size;
      ++this->first_chunk_index_;
      if (this->first_chunk_index_ != this->chunks_.size()) {
        this->first_chunk_allocation_ = this->chunks_[this->first_chunk_index_];
      }
    }
  }
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
