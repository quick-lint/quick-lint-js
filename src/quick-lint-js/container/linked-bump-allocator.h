// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>
#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/flexible-array.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/math.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/pointer.h>
#include <utility>

#if QLJS_HAVE_SANITIZER_ASAN_INTERFACE_H
#include <sanitizer/asan_interface.h>
#endif

#if defined(QLJS_DEBUG) && QLJS_DEBUG
#define QLJS_DEBUG_BUMP_ALLOCATOR 1
#else
#define QLJS_DEBUG_BUMP_ALLOCATOR 0
#endif

namespace quick_lint_js {
// A memory allocator with a few features:
//
// * no per-object free()/delete
// * bulk free() via rewind() (doesn't call destructors)
// * in-place growing of allocations
// * all allocations have the same alignment
//
// Internally, Linked_Bump_Allocator maintains a linked list of chunks.
template <std::size_t alignment>
class Linked_Bump_Allocator : public Memory_Resource {
 private:
  struct Chunk_Header;
  using Chunk = Flexible_Array<char, Chunk_Header>;

 public:
  explicit Linked_Bump_Allocator(const char* debug_owner) {
    static_cast<void>(debug_owner);
  }

  Linked_Bump_Allocator(const Linked_Bump_Allocator&) = delete;
  Linked_Bump_Allocator& operator=(const Linked_Bump_Allocator&) = delete;

  ~Linked_Bump_Allocator() override { this->release(); }

  void release() {
    Chunk* c = this->chunk_;
    while (c) {
      Chunk* previous = c->previous;
      Chunk::destroy_header_and_deallocate(new_delete_resource(), c);
      c = previous;
    }
    this->chunk_ = nullptr;
    this->next_allocation_ = nullptr;
    this->chunk_end_ = nullptr;
  }

  struct Rewind_State {
    // Private to linked_bump_allocator. Do not use.
    Chunk* chunk_;
    char* next_allocation_;
    char* chunk_end_;
  };

  // Calls allocator->rewind() when destructed.
  class Rewind_Guard {
   public:
    explicit Rewind_Guard(Linked_Bump_Allocator* allocator)
        : allocator_(allocator), rewind_(allocator->prepare_for_rewind()) {}

    Rewind_Guard(const Rewind_Guard&) = delete;
    Rewind_Guard& operator=(const Rewind_Guard&) = delete;

    Rewind_Guard(Rewind_Guard&&) = delete;
    Rewind_Guard& operator=(Rewind_Guard&&) = delete;

    ~Rewind_Guard() { this->allocator_->rewind(std::move(this->rewind_)); }

   private:
    Linked_Bump_Allocator* allocator_;
    Rewind_State rewind_;
  };

  Rewind_State prepare_for_rewind() {
    return Rewind_State{
        .chunk_ = this->chunk_,
        .next_allocation_ = this->next_allocation_,
        .chunk_end_ = this->chunk_end_,
    };
  }

  void rewind(Rewind_State&& r) {
    bool allocated_new_chunk = this->chunk_ != r.chunk_;
    if (allocated_new_chunk) {
      // If we rewound to exactly where we were before, we might rewind near the
      // end of a chunk. Allocations would soon need a new chunk.
      //
      // Avoid straddling near the end of a chunk by using a new chunk (which
      // was already allocated).
      //
      // TODO(strager): Should we use the *oldest* chunk or the *newest* chunk?
      // Here we pick the *oldest* chunk.
      Chunk* c = this->chunk_;
      QLJS_ASSERT(c);
      while (c->previous != r.chunk_) {
        Chunk* previous = c->previous;
        Chunk::destroy_header_and_deallocate(new_delete_resource(), c);
        c = previous;
        QLJS_ASSERT(c);
      }
      this->chunk_ = c;
      this->next_allocation_ = c->flexible_capacity_begin();
      this->chunk_end_ = c->flexible_capacity_end();
    } else {
      this->chunk_ = r.chunk_;
      this->next_allocation_ = r.next_allocation_;
      this->chunk_end_ = r.chunk_end_;
    }
    this->did_deallocate_bytes(
        this->next_allocation_,
        narrow_cast<std::size_t>(this->chunk_end_ - this->next_allocation_));
  }

  [[nodiscard]] Rewind_Guard make_rewind_guard() { return Rewind_Guard(this); }

  template <class T, class... Args>
  T* new_object(Args&&... args) {
    static_assert(alignof(T) <= alignment,
                  "T is not allowed by this allocator; this allocator's "
                  "alignment is insufficient for T");
    constexpr std::size_t byte_size = align_up(sizeof(T));
    return new (this->allocate_bytes(byte_size)) T(std::forward<Args>(args)...);
  }

  template <class T>
  [[nodiscard]] T* allocate_uninitialized_array(std::size_t size) {
    static_assert(alignof(T) <= alignment,
                  "T is not allowed by this allocator; this allocator's "
                  "alignment is insufficient for T");
    std::size_t byte_size = this->align_up(size * sizeof(T));
    return reinterpret_cast<T*>(this->allocate_bytes(byte_size));
  }

  template <class T>
  [[nodiscard]] Span<T> allocate_uninitialized_span(std::size_t size) {
    T* items = this->allocate_uninitialized_array<T>(size);
    return Span<T>(items, narrow_cast<Span_Size>(size));
  }

  template <class T>
  [[nodiscard]] Span<T> allocate_span(Span_Size size) {
    return this->allocate_span<T>(narrow_cast<std::size_t>(size));
  }

  template <class T>
  [[nodiscard]] Span<T> allocate_span(std::size_t size) {
    Span<T> items = this->allocate_uninitialized_span<T>(size);
    for (T& item : items) {
      // FIXME(strager): I think we technically need to std::launder after
      // placement new.
      new (&item) T();
    }
    return items;
  }

  template <class T>
  bool try_grow_array_in_place(T* array, std::size_t old_size,
                               std::size_t new_size) {
    this->assert_not_disabled();
    QLJS_ASSERT(new_size > old_size);
    std::size_t old_byte_size = this->align_up(old_size * sizeof(T));
    bool array_is_last_allocation =
        reinterpret_cast<char*>(array) + old_byte_size ==
        this->next_allocation_;
    if (!array_is_last_allocation) {
      // We can't grow because something else was already allocated.
      return false;
    }

    std::size_t extra_bytes = this->align_up((new_size - old_size) * sizeof(T));
    if (extra_bytes > this->remaining_bytes_in_current_chunk()) {
      return false;
    }
    this->did_allocate_bytes(this->next_allocation_, extra_bytes);
    this->next_allocation_ += extra_bytes;
    return true;
  }

  std::size_t remaining_bytes_in_current_chunk() const {
    return narrow_cast<std::size_t>(this->chunk_end_ - this->next_allocation_);
  }

  class Disable_Guard {
   public:
    ~Disable_Guard() {
#if QLJS_DEBUG_BUMP_ALLOCATOR
      this->alloc_->disabled_count_ -= 1;
#endif
    }

   private:
#if QLJS_DEBUG_BUMP_ALLOCATOR
    explicit disable_guard(linked_bump_allocator* alloc) : alloc_(alloc) {
      this->alloc_->disabled_count_ += 1;
    }
#else
    explicit Disable_Guard(Linked_Bump_Allocator*) {}
#endif

#if QLJS_DEBUG_BUMP_ALLOCATOR
    linked_bump_allocator* alloc_;
#endif

    friend class Linked_Bump_Allocator;
  };

  [[nodiscard]] Disable_Guard disable() { return Disable_Guard(this); }

 protected:
  void* do_allocate(std::size_t bytes,
                    std::size_t align) BOOST_NOEXCEPT override {
    QLJS_ASSERT(align <= alignment);
    return this->allocate_bytes(this->align_up(bytes));
  }

  void do_deallocate(void* p, std::size_t bytes,
                     std::size_t align) BOOST_NOEXCEPT override {
    QLJS_ASSERT(align <= alignment);
    this->deallocate_bytes(p, bytes);
  }

  bool do_is_equal(const memory_resource& other) const BOOST_NOEXCEPT override {
    return this == static_cast<const Linked_Bump_Allocator*>(&other);
  }

 private:
  struct alignas(maximum(alignment, alignof(void*))) Chunk_Header {
    explicit Chunk_Header(Chunk* previous) : previous(previous) {}

    Chunk* previous;  // Linked list.
  };

  static inline constexpr std::size_t default_chunk_size = 4096 - sizeof(Chunk);

  static constexpr std::size_t align_up(std::size_t size) {
    return (size + alignment - 1) & ~(alignment - 1);
  }

  [[nodiscard]] void* allocate_bytes(std::size_t size) {
    this->assert_not_disabled();
    QLJS_SLOW_ASSERT(size % alignment == 0);
    if (this->remaining_bytes_in_current_chunk() < size) {
      this->append_chunk(maximum(size, this->default_chunk_size));
      QLJS_ASSERT(this->remaining_bytes_in_current_chunk() >= size);
    }
    void* result = this->next_allocation_;
    this->next_allocation_ += size;
    this->did_allocate_bytes(result, size);
    return result;
  }

  void deallocate_bytes([[maybe_unused]] void* p,
                        [[maybe_unused]] std::size_t size) {
    // TODO(strager): Mark memory as unallocated for Valgrind and ASAN.
  }

  void did_allocate_bytes([[maybe_unused]] void* p,
                          [[maybe_unused]] std::size_t size) {
#if QLJS_HAVE_SANITIZER_ASAN_INTERFACE_H
    ASAN_UNPOISON_MEMORY_REGION(p, size);
#endif
    // TODO(strager): Mark memory as usable for Valgrind.
  }

  void did_deallocate_bytes([[maybe_unused]] void* p,
                            [[maybe_unused]] std::size_t size) {
#if QLJS_HAVE_SANITIZER_ASAN_INTERFACE_H
    ASAN_POISON_MEMORY_REGION(p, size);
#endif
    // TODO(strager): Mark memory as unusable for Valgrind.
  }

  void append_chunk(std::size_t size) {
    this->chunk_ = Chunk::allocate_and_construct_header(new_delete_resource(),
                                                        size, this->chunk_);
    this->next_allocation_ = this->chunk_->flexible_capacity_begin();
    this->chunk_end_ = this->chunk_->flexible_capacity_end();
  }

  void assert_not_disabled() const {
#if QLJS_DEBUG_BUMP_ALLOCATOR
    QLJS_ALWAYS_ASSERT(!this->is_disabled());
#endif
  }

#if QLJS_DEBUG_BUMP_ALLOCATOR
  bool is_disabled() const { return this->disabled_count_ > 0; }
#endif

  Chunk* chunk_ = nullptr;
  char* next_allocation_ = nullptr;
  char* chunk_end_ = nullptr;

#if QLJS_DEBUG_BUMP_ALLOCATOR
  int disabled_count_ = 0;
#endif
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
