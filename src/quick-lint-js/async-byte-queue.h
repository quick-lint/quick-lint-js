// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ASYNC_BYTE_QUEUE_H
#define QUICK_LINT_JS_ASYNC_BYTE_QUEUE_H

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/thread.h>
#include <utility>

namespace quick_lint_js {
// async_byte_queue is like byte_buffer, but allows one thread to write data and
// another thread to read data. async_byte_queue is a Single-Producer
// Single-Consumer (SPSC) queue of bytes.
//
// async_byte_queue is implemented as a linked list of byte arrays called
// chunks.
class async_byte_queue {
 public:
  using size_type = std::size_t;

  static constexpr size_type default_chunk_size = 1024;

  explicit async_byte_queue();

  async_byte_queue(const async_byte_queue&) = delete;
  async_byte_queue& operator=(const async_byte_queue&) = delete;

  ~async_byte_queue();

  // Writer thread only.
  void* append(size_type byte_count);

  // Writer thread only.
  void* append_aligned(size_type byte_count, size_type alignment);

  // Writer thread only.
  void append_copy(char8 data);

  // Writer thread only.
  void commit();

  // Reader thread only.
  string8 take_committed_string8();

  // Reader thread only.
  template <class ChunkFunc, class FinalizeFunc>
  void take_committed(ChunkFunc&& chunk_callback,
                      FinalizeFunc&& finalize_callback);

 private:
  struct alignas(alignof(std::max_align_t)) chunk {
    // data[begin_index] until data[committed_index] contains committed but
    // untaken bytes.
    //
    // NOTE[committed_index-data_size]: The writer updates committed_index after
    // committing, but only for the most recent chunk. If next is not null, then
    // data_size should be read instead of committed_index.
    //
    // The reader updates begin_index after taking. begin_index is exclusive to
    // the reader.
    //
    // committed_index is protected by mutex_.
    size_type begin_index = 0;
    size_type committed_index = 0;

    // Updated by the writer when this chunk has no more space.
    //
    // If this chunk is the most recent chunk, data_size is uninitialized.
    //
    // data_size is protected by mutex_.
    size_type data_size;

    // next is protected by mutex_.
    chunk* next = nullptr;

    // Number of malloc-allocated bytes in the data array.
    size_type capacity;

    std::byte* capacity_begin() noexcept {
      return reinterpret_cast<std::byte*>(this) + sizeof(*this);
    }
    std::byte* capacity_end() noexcept {
      return this->capacity_begin() + this->capacity;
    }

    static chunk* allocate(size_type data_size);

    static void deallocate(chunk*);

   private:
    explicit chunk(size_type capacity) noexcept : capacity(capacity) {}
    ~chunk() = default;
  };

  // Writer thread only:
  void reserve(size_type extra_byte_count);
  void reserve_aligned(size_type extra_byte_count, size_type alignment);
  void grow(size_type extra_byte_count);
  void add_new_chunk(size_type chunk_size);
  void update_current_chunk_size(std::lock_guard<mutex>&) noexcept;
  size_type bytes_remaining_in_current_chunk() const noexcept;
  size_type bytes_used_in_current_chunk() const noexcept;

  // Exclusive to the writer:
  chunk* writer_first_chunk_ = chunk::allocate(default_chunk_size);
  chunk* writer_last_chunk_ = this->writer_first_chunk_;
  std::byte* writer_cursor_ = this->writer_last_chunk_->capacity_begin();
  std::byte* writer_chunk_end_ = this->writer_last_chunk_->capacity_end();

  // Exclusive to the reader:
  chunk* reader_chunk_ = this->writer_first_chunk_;

  mutex mutex_;
};

// Reader thread only.
template <class ChunkFunc, class FinalizeFunc>
void async_byte_queue::take_committed(ChunkFunc&& chunk_callback,
                                      FinalizeFunc&& finalize_callback) {
  chunk* first_chunk = this->reader_chunk_;
  chunk* last_chunk;
  {
    std::unique_lock<mutex> lock(this->mutex_);
    auto call_chunk_callback = [&](const std::byte* data, size_type size) {
      lock.unlock();
      chunk_callback(data, size);
      lock.lock();
    };

    chunk* c = first_chunk;

  take_from_chunks:
    // The last chunk's size is determined specially. See
    // NOTE[committed_index-data_size].
    while (c->next) {
      size_type old_data_size = c->data_size;
      call_chunk_callback(&c->capacity_begin()[c->begin_index],
                          c->data_size - c->begin_index);

      // These shouldn't be changed by the callback or by the writer thread.
      QLJS_ASSERT(c->data_size == old_data_size);
      QLJS_ASSERT(c->next);

      c = c->next;
    }

    size_type old_committed_index = c->committed_index;
    call_chunk_callback(&c->capacity_begin()[c->begin_index],
                        old_committed_index - c->begin_index);
    c->begin_index = old_committed_index;
    // NOTE(strager): Because call_chunk_callback unlocks the mutex,
    // c->committed_index and c->next might change.
    if (c->next || c->committed_index != old_committed_index) {
      goto take_from_chunks;
    }

    last_chunk = c;
  }
  this->reader_chunk_ = last_chunk;

  finalize_callback();
  // NOTE(strager): Normally, reading c->next requires mutex_ to be held.
  // However, we know that the writer cannot modify any chunk except possibly
  // last_chunk, so we know we can't data race with the writer in the
  // following loop.
  for (chunk* c = first_chunk; c != last_chunk;) {
    chunk::deallocate(std::exchange(c, c->next));
  }
}
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
