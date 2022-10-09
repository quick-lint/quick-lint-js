// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/pointer.h>
#include <utility>

namespace quick_lint_js {
async_byte_queue::async_byte_queue()
    : async_byte_queue(new_delete_resource()) {}

async_byte_queue::async_byte_queue(memory_resource* memory) : memory_(memory) {}

async_byte_queue::~async_byte_queue() {
  chunk* c = this->reader_chunk_;
  while (c) {
    chunk::deallocate(this->memory_, std::exchange(c, c->next));
  }
}

void* async_byte_queue::append(size_type byte_count) {
  this->reserve(byte_count);
  return std::exchange(this->writer_cursor_, this->writer_cursor_ + byte_count);
}

void async_byte_queue::append_copy(char8 data) {
  return this->append_copy(&data, sizeof(data));
}

void async_byte_queue::append_copy(const void* data, size_type byte_count) {
  void* out = this->append(byte_count);
  std::memcpy(out, data, byte_count);
}

void async_byte_queue::commit() {
  {
    std::lock_guard<mutex> lock(this->mutex_);
    this->update_current_chunk_size(lock);
    this->writer_last_chunk_->committed_index =
        this->writer_last_chunk_->data_size;
  }
  this->writer_first_chunk_ = this->writer_last_chunk_;
}

string8 async_byte_queue::take_committed_string8() {
  string8 result;
  this->take_committed(
      [&result](const std::byte* data, std::size_t size) -> void {
        result.append(reinterpret_cast<const char8*>(data), size);
      },
      []() {});
  return result;
}

void async_byte_queue::reserve(size_type extra_byte_count) {
  if (this->bytes_remaining_in_current_chunk() < extra_byte_count) {
    this->grow(extra_byte_count);
  }
}

void async_byte_queue::reserve_aligned(size_type extra_byte_count,
                                       size_type alignment) {
  if (!is_aligned(this->writer_cursor_, alignment) ||
      this->bytes_remaining_in_current_chunk() < extra_byte_count) {
    this->grow(extra_byte_count);
    QLJS_ASSERT(is_aligned(this->writer_cursor_, alignment));
  }
}

void async_byte_queue::grow(size_type extra_byte_count) {
  {
    std::lock_guard<mutex> lock(this->mutex_);
    this->update_current_chunk_size(lock);
  }
  this->add_new_chunk(std::max(default_chunk_size, extra_byte_count));
}

void async_byte_queue::add_new_chunk(size_type chunk_size) {
  chunk* new_chunk = chunk::allocate(this->memory_, chunk_size);
  {
    std::lock_guard<mutex> lock(this->mutex_);
    this->writer_last_chunk_->next = new_chunk;
  }
  this->writer_last_chunk_ = new_chunk;
  this->writer_cursor_ = new_chunk->capacity_begin();
  this->writer_chunk_end_ = new_chunk->capacity_end();
}

void async_byte_queue::update_current_chunk_size(
    std::lock_guard<mutex>&) noexcept {
  this->writer_last_chunk_->data_size = this->bytes_used_in_current_chunk();
}

async_byte_queue::size_type async_byte_queue::bytes_remaining_in_current_chunk()
    const noexcept {
  return narrow_cast<size_type>(this->writer_chunk_end_ - this->writer_cursor_);
}

async_byte_queue::size_type async_byte_queue::bytes_used_in_current_chunk()
    const noexcept {
  return narrow_cast<size_type>(this->writer_cursor_ -
                                this->writer_last_chunk_->capacity_begin());
}

async_byte_queue::chunk* async_byte_queue::chunk::allocate(
    memory_resource* memory, size_type data_size) {
  void* c = memory->allocate(chunk::allocation_size(data_size), alignof(chunk));
  return new (c) chunk(data_size);
}

void async_byte_queue::chunk::deallocate(memory_resource* memory, chunk* c) {
  std::size_t byte_size = c->allocation_size();
  c->~chunk();
  memory->deallocate(c, byte_size, alignof(chunk));
}

std::size_t async_byte_queue::chunk::allocation_size(
    size_type capacity) noexcept {
  return sizeof(chunk) + capacity;
}

std::size_t async_byte_queue::chunk::allocation_size() const noexcept {
  return chunk::allocation_size(this->capacity);
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
