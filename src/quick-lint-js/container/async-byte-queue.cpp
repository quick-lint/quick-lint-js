// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/pointer.h>
#include <utility>

namespace quick_lint_js {
Async_Byte_Queue::Async_Byte_Queue()
    : Async_Byte_Queue(new_delete_resource()) {}

Async_Byte_Queue::Async_Byte_Queue(Memory_Resource* memory) : memory_(memory) {}

Async_Byte_Queue::~Async_Byte_Queue() {
  Chunk* c = this->reader_chunk_;
  while (c) {
    Chunk::destroy_header_and_deallocate(this->memory_,
                                         std::exchange(c, c->next));
  }
}

void* Async_Byte_Queue::append(Size_Type byte_count) {
  this->reserve(byte_count);
  return std::exchange(this->writer_cursor_, this->writer_cursor_ + byte_count);
}

void Async_Byte_Queue::append_copy(Char8 data) {
  return this->append_copy(&data, sizeof(data));
}

void Async_Byte_Queue::append_copy(const void* data, Size_Type byte_count) {
  void* out = this->append(byte_count);
  std::memcpy(out, data, byte_count);
}

void Async_Byte_Queue::commit() {
  {
    std::lock_guard<Mutex> lock(this->mutex_);
    this->update_current_chunk_size(lock);
    this->writer_last_chunk_->committed_index =
        this->writer_last_chunk_->data_size;
  }
  this->writer_first_chunk_ = this->writer_last_chunk_;
}

String8 Async_Byte_Queue::take_committed_string8() {
  String8 result;
  this->take_committed(
      [&result](Span<const std::byte> data) -> void {
        result.append(reinterpret_cast<const Char8*>(data.data()),
                      narrow_cast<std::size_t>(data.size()));
      },
      []() {});
  return result;
}

void Async_Byte_Queue::reserve(Size_Type extra_byte_count) {
  if (this->bytes_remaining_in_current_chunk() < extra_byte_count) {
    this->grow(extra_byte_count);
  }
}

void Async_Byte_Queue::reserve_aligned(Size_Type extra_byte_count,
                                       Size_Type alignment) {
  if (!is_aligned(this->writer_cursor_, alignment) ||
      this->bytes_remaining_in_current_chunk() < extra_byte_count) {
    this->grow(extra_byte_count);
    QLJS_ASSERT(is_aligned(this->writer_cursor_, alignment));
  }
}

void Async_Byte_Queue::grow(Size_Type extra_byte_count) {
  {
    std::lock_guard<Mutex> lock(this->mutex_);
    this->update_current_chunk_size(lock);
  }
  this->add_new_chunk(std::max(default_chunk_size, extra_byte_count));
}

void Async_Byte_Queue::add_new_chunk(Size_Type chunk_size) {
  Chunk* new_chunk =
      Chunk::allocate_and_construct_header(this->memory_, chunk_size);
  {
    std::lock_guard<Mutex> lock(this->mutex_);
    this->writer_last_chunk_->next = new_chunk;
  }
  this->writer_last_chunk_ = new_chunk;
  this->writer_cursor_ = new_chunk->flexible_capacity_begin();
  this->writer_chunk_end_ = new_chunk->flexible_capacity_end();
}

void Async_Byte_Queue::update_current_chunk_size(std::lock_guard<Mutex>&) {
  this->writer_last_chunk_->data_size = this->bytes_used_in_current_chunk();
}

Async_Byte_Queue::Size_Type Async_Byte_Queue::bytes_remaining_in_current_chunk()
    const {
  return narrow_cast<Size_Type>(this->writer_chunk_end_ - this->writer_cursor_);
}

Async_Byte_Queue::Size_Type Async_Byte_Queue::bytes_used_in_current_chunk()
    const {
  return narrow_cast<Size_Type>(
      this->writer_cursor_ -
      this->writer_last_chunk_->flexible_capacity_begin());
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
