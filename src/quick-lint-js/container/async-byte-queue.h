// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_ASYNC_BYTE_QUEUE_H
#define QUICK_LINT_JS_CONTAINER_ASYNC_BYTE_QUEUE_H

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/max-align.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <utility>

namespace quick_lint_js {
// async_byte_queue is like byte_buffer, but allows one thread to write data and
// another thread to read data. async_byte_queue is a Single-Producer
// Single-Consumer (SPSC) queue of bytes.
//
// async_byte_queue allows multiple reader threads if they are synchronized
// (e.g. using a mutex).
//
// async_byte_queue allows multiple writer threads if they are synchronized
// (e.g. using a mutex).
//
// async_byte_queue is implemented as a linked list of byte arrays called
// chunks.
class Async_Byte_Queue {
 public:
  using Size_Type = std::size_t;

  static constexpr Size_Type default_chunk_size = 1024;

  explicit Async_Byte_Queue();
  // The given memory resource must be thread-safe. allocate and deallocate are
  // called from multiple threads without synchronization.
  explicit Async_Byte_Queue(Memory_Resource*);

  Async_Byte_Queue(const Async_Byte_Queue&) = delete;
  Async_Byte_Queue& operator=(const Async_Byte_Queue&) = delete;

  ~Async_Byte_Queue();

  // Writer thread only.
  void* append(Size_Type byte_count);

  // Writer thread only.
  template <class Func>
  void append_aligned(Size_Type max_byte_count, Size_Type alignment, Func&& f) {
    this->reserve_aligned(max_byte_count, alignment);
    Size_Type bytes_written = f(this->writer_cursor_);
    QLJS_ASSERT(bytes_written <= max_byte_count);
    this->writer_cursor_ += bytes_written;
  }

  // Writer thread only.
  void append_copy(Char8 data);
  void append_copy(const void* data, Size_Type byte_count);

  // Writer thread only.
  void commit();

  // Reader thread only.
  String8 take_committed_string8();

  // Reader thread only.
  template <class ChunkFunc, class FinalizeFunc>
  void take_committed(ChunkFunc&& chunk_callback,
                      FinalizeFunc&& finalize_callback);

 private:
  struct alignas(alignof(Max_Align_T)) Chunk {
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
    Size_Type begin_index = 0;
    Size_Type committed_index = 0;

    // Updated by the writer when this chunk has no more space.
    //
    // If this chunk is the most recent chunk, data_size is uninitialized.
    //
    // data_size is protected by mutex_.
    Size_Type data_size;

    // next is protected by mutex_.
    Chunk* next = nullptr;

    // Number of malloc-allocated bytes in the data array.
    Size_Type capacity;

    std::byte* capacity_begin() noexcept {
      return reinterpret_cast<std::byte*>(this) + sizeof(*this);
    }
    std::byte* capacity_end() noexcept {
      return this->capacity_begin() + this->capacity;
    }

    static Chunk* allocate(Memory_Resource*, Size_Type data_size);
    static void deallocate(Memory_Resource*, Chunk*);

   private:
    explicit Chunk(Size_Type capacity) noexcept : capacity(capacity) {}
    ~Chunk() = default;

    static std::size_t allocation_size(Size_Type capacity) noexcept;
    std::size_t allocation_size() const noexcept;
  };

  // Writer thread only:
  void reserve(Size_Type extra_byte_count);
  void reserve_aligned(Size_Type extra_byte_count, Size_Type alignment);
  void grow(Size_Type extra_byte_count);
  void add_new_chunk(Size_Type chunk_size);
  void update_current_chunk_size(std::lock_guard<Mutex>&) noexcept;
  Size_Type bytes_remaining_in_current_chunk() const noexcept;
  Size_Type bytes_used_in_current_chunk() const noexcept;

  // Usable by either reader or writer (mutex_ does not need to be held):
  Memory_Resource* memory_;

  // Exclusive to the writer:
  Chunk* writer_first_chunk_ =
      Chunk::allocate(this->memory_, default_chunk_size);
  Chunk* writer_last_chunk_ = this->writer_first_chunk_;
  std::byte* writer_cursor_ = this->writer_last_chunk_->capacity_begin();
  std::byte* writer_chunk_end_ = this->writer_last_chunk_->capacity_end();

  // Exclusive to the reader:
  Chunk* reader_chunk_ = this->writer_first_chunk_;

  Mutex mutex_;
};

// Reader thread only.
template <class ChunkFunc, class FinalizeFunc>
void Async_Byte_Queue::take_committed(ChunkFunc&& chunk_callback,
                                      FinalizeFunc&& finalize_callback) {
  Chunk* first_chunk = this->reader_chunk_;
  Chunk* last_chunk;
  {
    std::unique_lock<Mutex> lock(this->mutex_);
    auto call_chunk_callback = [&](Span<const std::byte> data) {
      lock.unlock();
      chunk_callback(data);
      lock.lock();
    };

    Chunk* c = first_chunk;

  take_from_chunks:
    // The last chunk's size is determined specially. See
    // NOTE[committed_index-data_size].
    while (c->next) {
      Size_Type old_data_size = c->data_size;
      call_chunk_callback(Span<const std::byte>(
          &c->capacity_begin()[c->begin_index],
          narrow_cast<Span_Size>(c->data_size - c->begin_index)));

      // These shouldn't be changed by the callback or by the writer thread.
      QLJS_ASSERT(c->data_size == old_data_size);
      QLJS_ASSERT(c->next);

      c = c->next;
    }

    Size_Type old_committed_index = c->committed_index;
    call_chunk_callback(Span<const std::byte>(
        &c->capacity_begin()[c->begin_index],
        narrow_cast<Span_Size>(old_committed_index - c->begin_index)));
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
  for (Chunk* c = first_chunk; c != last_chunk;) {
    Chunk::deallocate(this->memory_, std::exchange(c, c->next));
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
