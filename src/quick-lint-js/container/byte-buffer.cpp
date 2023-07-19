// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <utility>
#include <vector>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#endif

namespace quick_lint_js {
namespace {
const std::byte* chunk_begin(const Byte_Buffer_Chunk& c) noexcept {
#if QLJS_HAVE_WRITEV
  return reinterpret_cast<const std::byte*>(c.iov_base);
#else
  return c.data;
#endif
}

std::byte* chunk_begin(Byte_Buffer_Chunk& c) noexcept {
#if QLJS_HAVE_WRITEV
  return reinterpret_cast<std::byte*>(c.iov_base);
#else
  return c.data;
#endif
}

Byte_Buffer::Size_Type chunk_size(const Byte_Buffer_Chunk& c) noexcept {
#if QLJS_HAVE_WRITEV
  return c.iov_len;
#else
  return c.size;
#endif
}

Byte_Buffer::Size_Type& chunk_size(Byte_Buffer_Chunk& c) noexcept {
#if QLJS_HAVE_WRITEV
  return c.iov_len;
#else
  return c.size;
#endif
}

std::byte* chunk_end(Byte_Buffer_Chunk& c) noexcept {
  return chunk_begin(c) + chunk_size(c);
}

Byte_Buffer_Chunk make_chunk(const void* data, Byte_Buffer::Size_Type size) {
#if QLJS_HAVE_WRITEV
  return Byte_Buffer_Chunk{.iov_base = const_cast<void*>(data),
                           .iov_len = size};
#else
  return Byte_Buffer_Chunk{
      .data = const_cast<std::byte*>(reinterpret_cast<const std::byte*>(data)),
      .size = size};
#endif
}

}

Byte_Buffer::Byte_Buffer() { this->add_new_chunk(this->default_chunk_size); }

Byte_Buffer::Byte_Buffer(Byte_Buffer&&) = default;

Byte_Buffer::~Byte_Buffer() {
  for (Byte_Buffer_Chunk& c : this->chunks_) {
    this->delete_chunk(std::move(c));
  }
}

void* Byte_Buffer::append(Size_Type byte_count) {
  this->reserve(byte_count);
  return std::exchange(this->cursor_, this->cursor_ + byte_count);
}

void Byte_Buffer::append_copy(const String8& data) {
  return this->append_copy(String8_View(data));
}

void Byte_Buffer::append_copy(String8_View data) {
  static_assert(sizeof(data[0]) == 1);
  void* out = this->append(data.size());
  std::memcpy(out, data.data(), data.size());
}

void Byte_Buffer::append_copy(Char8 data) {
  void* out = this->append(1);
  std::memcpy(out, &data, 1);
}

void Byte_Buffer::prepend_copy(String8_View data) {
  // TODO(strager): As an optimization, reserve one slot in the vector for
  // prepending. (We expect one prepend per byte_buffer by lsp_pipe_writer.)
  // TODO(strager): If there's space in the current chunk, use it instead of
  // making a new chunk.

  const std::byte* data_bytes = reinterpret_cast<const std::byte*>(data.data());

  Byte_Buffer_Chunk prefix_chunk = this->allocate_chunk(data.size());
  const std::byte* end =
      std::copy_n(data_bytes, data.size(), chunk_begin(prefix_chunk));
  QLJS_ASSERT(end == chunk_end(prefix_chunk));

  this->chunks_.insert(this->chunks_.begin(), std::move(prefix_chunk));
}

void Byte_Buffer::clear() {
  auto begin = this->chunks_.begin();
  auto end = std::prev(this->chunks_.end());
  for (auto it = begin; it != end; ++it) {
    this->delete_chunk(std::move(*it));
  }
  this->chunks_.erase(begin, end);
  QLJS_ASSERT(this->chunks_.size() == 1);
  this->cursor_ = chunk_begin(this->chunks_.back());
}

Byte_Buffer::Size_Type Byte_Buffer::size() const noexcept {
  Size_Type total_size = 0;
  for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size() - 1;
       ++chunk_index) {
    const Byte_Buffer_Chunk& c = this->chunks_[chunk_index];
    total_size += chunk_size(c);
  }
  total_size += this->bytes_used_in_current_chunk();
  return total_size;
}

bool Byte_Buffer::empty() const noexcept {
  if (this->bytes_used_in_current_chunk() > 0) {
    return false;
  }
  for (std::size_t chunk_index = 0; chunk_index < this->chunks_.size() - 1;
       ++chunk_index) {
    const Byte_Buffer_Chunk& c = this->chunks_[chunk_index];
    if (chunk_size(c) > 0) {
      return false;
    }
  }
  return true;
}

void Byte_Buffer::copy_to(void* raw_out) const {
  std::byte* out = reinterpret_cast<std::byte*>(raw_out);
  this->enumerate_chunks(
      [&](const std::byte* c_begin, const std::byte* c_end) -> void {
        out = std::copy(c_begin, c_end, out);
      });
}

Byte_Buffer_IOVec Byte_Buffer::to_iovec() && {
  this->update_current_chunk_size();
  this->remove_current_chunk_if_empty();
  return Byte_Buffer_IOVec(std::move(this->chunks_));
}

void Byte_Buffer::reserve(Size_Type extra_byte_count) {
  if (this->bytes_remaining_in_current_chunk() < extra_byte_count) {
    this->grow(extra_byte_count);
  }
}

[[gnu::noinline]] void Byte_Buffer::grow(Size_Type extra_byte_count) {
  this->update_current_chunk_size();
  this->remove_current_chunk_if_empty();
  this->add_new_chunk(std::max(default_chunk_size, extra_byte_count));
}

void Byte_Buffer::update_current_chunk_size() noexcept {
  QLJS_ASSERT(!this->chunks_.empty());
  chunk_size(this->chunks_.back()) = this->bytes_used_in_current_chunk();
}

void Byte_Buffer::remove_current_chunk_if_empty() {
  QLJS_ASSERT(!this->chunks_.empty());
  if (chunk_size(this->chunks_.back()) == 0) {
    this->delete_chunk(std::move(this->chunks_.back()));
    this->chunks_.pop_back();
  }
}

Byte_Buffer::Size_Type Byte_Buffer::bytes_remaining_in_current_chunk() const
    noexcept {
  return narrow_cast<Size_Type>(this->current_chunk_end_ - this->cursor_);
}

Byte_Buffer::Size_Type Byte_Buffer::bytes_used_in_current_chunk() const
    noexcept {
  return narrow_cast<Size_Type>(this->cursor_ -
                                chunk_begin(this->chunks_.back()));
}

void Byte_Buffer::add_new_chunk(Size_Type chunk_size) {
  Byte_Buffer_Chunk& c =
      this->chunks_.emplace_back(this->allocate_chunk(chunk_size));
  this->cursor_ = chunk_begin(c);
  this->current_chunk_end_ = chunk_end(c);
}

Byte_Buffer_Chunk Byte_Buffer::allocate_chunk(Size_Type size) {
  // See corresponding deallocation in delete_chunk.
  return make_chunk(new std::byte[size], size);
}

void Byte_Buffer::delete_chunk(Byte_Buffer_Chunk&& c) {
  // See corresponding allocation in allocate_chunk.
  delete[] chunk_begin(c);
}

Byte_Buffer_IOVec::Byte_Buffer_IOVec()
    : Byte_Buffer_IOVec(std::vector<Byte_Buffer_Chunk>()) {}

Byte_Buffer_IOVec::Byte_Buffer_IOVec(std::vector<Byte_Buffer_Chunk>&& chunks)
    : chunks_(std::move(chunks)), first_chunk_index_(0) {
  if (this->chunks_.empty()) {
    this->first_chunk_allocation_ = make_chunk(nullptr, 0);
  } else {
    this->first_chunk_allocation_ = this->chunks_.front();
  }

  for (const Byte_Buffer_Chunk& c : this->chunks_) {
    QLJS_ASSERT(chunk_size(c) > 0);
  }
}

Byte_Buffer_IOVec::Byte_Buffer_IOVec(Byte_Buffer_IOVec&&) = default;

Byte_Buffer_IOVec::~Byte_Buffer_IOVec() {
  if (this->first_chunk_index_ != this->chunks_.size()) {
    // The first chunk might have been split. Deallocate the original
    // allocation, not the advanced pointer.
    Byte_Buffer::delete_chunk(std::move(this->first_chunk_allocation_));
    for (std::size_t i = this->first_chunk_index_ + 1; i < this->chunks_.size();
         ++i) {
      Byte_Buffer::delete_chunk(std::move(this->chunks_[i]));
    }
  }
}

const Byte_Buffer_Chunk* Byte_Buffer_IOVec::iovec() const noexcept {
  return this->chunks_.data() + this->first_chunk_index_;
}

int Byte_Buffer_IOVec::iovec_count() const noexcept {
  return narrow_cast<int>(this->chunks_.size() - this->first_chunk_index_);
}

void Byte_Buffer_IOVec::append(Byte_Buffer&& other) {
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

void Byte_Buffer_IOVec::remove_front(Size_Type size) {
  while (size != 0) {
    QLJS_ASSERT(this->first_chunk_index_ != this->chunks_.size());

    Byte_Buffer_Chunk& c = this->chunks_[this->first_chunk_index_];
    Size_Type c_size = chunk_size(c);
    if (size < c_size) {
      // Split this chunk.
      c = make_chunk(
          /*data=*/chunk_begin(c) + size,
          /*size=*/c_size - size);
      size = 0;
    } else {
      // Delete this entire chunk. It may have been split, so delete the
      // original, unsplit chunk.
      Byte_Buffer::delete_chunk(std::move(this->first_chunk_allocation_));
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
