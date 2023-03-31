// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_LINKED_VECTOR_H
#define QUICK_LINT_JS_CONTAINER_LINKED_VECTOR_H

#include <cstddef>
#include <memory>
#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/allocator.h>
#include <quick-lint-js/port/math.h>
#include <quick-lint-js/port/memory-resource.h>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
// A linked list of arrays. Optimized for appending then iterating.
//
// Guarantees:
//
// * Items are ordered by insertion (like std::vector and std::deque when only
//   calling emplace_back).
// * Items are never copied or moved when adding or removing different items
//   (like std::deque). Pointer stability.
template <class T>
class linked_vector {
 public:
  static constexpr std::size_t default_chunk_byte_size = 4096;
  static constexpr std::size_t items_per_chunk =
      maximum(1U, (default_chunk_byte_size - sizeof(void*) * 3) / sizeof(T));

  explicit linked_vector(memory_resource* memory) noexcept : memory_(memory) {}

  linked_vector(linked_vector&& other)
      : head_(other.head_), tail_(other.tail_), memory_(other.memory_) {
    other.head_ = nullptr;
    other.tail_ = nullptr;
  }

  linked_vector& operator=(linked_vector&& other) {
    this->clear();
    this->head_ = other.head_;
    this->tail_ = other.tail_;
    this->memory_ = other.memory_;
    other.head_ = nullptr;
    other.tail_ = nullptr;
  }

  ~linked_vector() { this->clear(); }

  template <class... Args>
  T& emplace_back(Args&&... args) {
    chunk* c = this->tail_;
    if (!c || c->item_count == c->capacity) {
      c = this->append_new_chunk_slow();
    }
    T* item = c->data() + c->item_count;
    item = new (item) T(std::forward<Args>(args)...);
    c->item_count += 1;
    return *item;
  }

  void pop_back() {
    QLJS_ASSERT(!this->empty());
    chunk* c = this->tail_;
    T& item = c->item(c->item_count - 1);
    item.~T();
    c->item_count -= 1;
    if (c->item_count == 0) {
      this->remove_tail_chunk_slow();
    }
  }

  void clear() {
    chunk* c = this->head_;
    while (c) {
      chunk* next = c->next;
      std::destroy_n(c->data(), c->item_count);
      delete_object(this->memory_, c);
      c = next;
    }
    this->head_ = nullptr;
    this->tail_ = nullptr;
  }

  bool empty() const noexcept { return this->head_ == nullptr; }

  T& back() noexcept {
    QLJS_ASSERT(!this->empty());
    return this->tail_->item(this->tail_->item_count - 1);
  }

  template <class Func>
  void for_each(Func&& func) const {
    chunk* c = this->head_;
    while (c) {
      std::size_t items_in_chunk = c->item_count;
      for (std::size_t i = 0; i < items_in_chunk; ++i) {
        const T& item = c->item(i);
        func(item);
      }
      c = c->next;
    }
  }

 private:
  struct chunk {
    chunk* prev = nullptr;
    chunk* next = nullptr;
    std::size_t item_count = 0;
    static constexpr std::size_t capacity = items_per_chunk;
    std::byte item_storage[capacity * sizeof(T)];

    T* data() { return reinterpret_cast<T*>(this->item_storage); }

    T& item(std::size_t index) { return *std::launder(this->data() + index); }
  };

  [[gnu::noinline]] chunk* append_new_chunk_slow() {
    chunk* c = new_object<chunk>(this->memory_);
    if (this->head_) {
      this->tail_->next = c;
    } else {
      this->head_ = c;
    }
    c->prev = this->tail_;
    this->tail_ = c;
    return c;
  }

  [[gnu::noinline]] void remove_tail_chunk_slow() {
    chunk* old_tail = this->tail_;
    QLJS_ASSERT(old_tail);
    QLJS_ASSERT(old_tail->item_count == 0);

    chunk* new_tail = old_tail->prev;
    QLJS_ASSERT((new_tail == nullptr) == (this->head_ == this->tail_));
    delete_object<chunk>(this->memory_, old_tail);
    if (new_tail) {
      new_tail->next = nullptr;
      this->tail_ = new_tail;
    } else {
      // We deallocated the only chunk.
      this->head_ = nullptr;
      this->tail_ = nullptr;
    }
  }

  chunk* head_ = nullptr;
  chunk* tail_ = nullptr;
  memory_resource* memory_;
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
