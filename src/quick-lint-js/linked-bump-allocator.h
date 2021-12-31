// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LINKED_BUMP_ALLOCATOR_H
#define QUICK_LINT_JS_LINKED_BUMP_ALLOCATOR_H

#include <boost/container/pmr/memory_resource.hpp>
#include <cstddef>
#include <cstdint>
#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/math.h>
#include <quick-lint-js/narrow-cast.h>

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
// Internally, linked_bump_allocator maintains a linked list of chunks.
template <std::size_t Alignment>
class linked_bump_allocator : public boost::container::pmr::memory_resource {
 private:
  struct chunk_header;

 public:
  static inline constexpr std::size_t alignment = Alignment;

  explicit linked_bump_allocator() = default;

  linked_bump_allocator(const linked_bump_allocator&) = delete;
  linked_bump_allocator& operator=(const linked_bump_allocator&) = delete;

  ~linked_bump_allocator() override { this->release(); }

  void release() {
    chunk_header* c = this->chunk_;
    while (c) {
      chunk_header* previous = c->previous;
      chunk_header::delete_chunk(c);
      c = previous;
    }
    this->chunk_ = nullptr;
    this->next_allocation_ = nullptr;
    this->chunk_end_ = nullptr;
  }

  struct rewind_state {
    // Private to linked_bump_allocator. Do not use.
    chunk_header* chunk_;
    char* next_allocation_;
    char* chunk_end_;
  };

  // Calls allocator->rewind() when destructed.
  class rewind_guard {
   public:
    explicit rewind_guard(linked_bump_allocator* allocator)
        : allocator_(allocator), rewind_(allocator->prepare_for_rewind()) {}

    rewind_guard(const rewind_guard&) = delete;
    rewind_guard& operator=(const rewind_guard&) = delete;

    rewind_guard(rewind_guard&&) = delete;
    rewind_guard& operator=(rewind_guard&&) = delete;

    ~rewind_guard() { this->allocator_->rewind(std::move(this->rewind_)); }

   private:
    linked_bump_allocator* allocator_;
    rewind_state rewind_;
  };

  rewind_state prepare_for_rewind() {
    return rewind_state{
        .chunk_ = this->chunk_,
        .next_allocation_ = this->next_allocation_,
        .chunk_end_ = this->chunk_end_,
    };
  }

  void rewind(rewind_state&& r) {
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
      chunk_header* c = this->chunk_;
      QLJS_ASSERT(c);
      while (c->previous != r.chunk_) {
        chunk_header* previous = c->previous;
        chunk_header::delete_chunk(c);
        c = previous;
        QLJS_ASSERT(c);
      }
      this->chunk_ = c;
      this->next_allocation_ = c->data();
      this->chunk_end_ = c->data_end();
    } else {
      this->chunk_ = r.chunk_;
      this->next_allocation_ = r.next_allocation_;
      this->chunk_end_ = r.chunk_end_;
    }
    this->did_deallocate_bytes(
        this->next_allocation_,
        narrow_cast<std::size_t>(this->chunk_end_ - this->next_allocation_));
  }

  [[nodiscard]] rewind_guard make_rewind_guard() noexcept {
    return rewind_guard(this);
  }

  template <class T, class... Args>
  T* new_object(Args&&... args) {
    static_assert(alignof(T) <= Alignment,
                  "T is not allowed by this allocator; this allocator's "
                  "alignment is insufficient for T");
    constexpr std::size_t byte_size = align_up(sizeof(T));
    return new (this->allocate_bytes(byte_size)) T(std::forward<Args>(args)...);
  }

  template <class T>
  [[nodiscard]] T* allocate_uninitialized_array(std::size_t size) {
    static_assert(alignof(T) <= Alignment,
                  "T is not allowed by this allocator; this allocator's "
                  "alignment is insufficient for T");
    std::size_t byte_size = this->align_up(size * sizeof(T));
    return reinterpret_cast<T*>(this->allocate_bytes(byte_size));
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

  // For debugging and testing only.
  std::size_t remaining_bytes_in_current_chunk() const {
    return narrow_cast<std::size_t>(this->chunk_end_ - this->next_allocation_);
  }

  template <class T>
  boost::container::pmr::polymorphic_allocator<T> standard_allocator() {
    return boost::container::pmr::polymorphic_allocator<T>(
        this->memory_resource());
  }

  // TODO(strager): Inline.
  boost::container::pmr::memory_resource* memory_resource() noexcept {
    return this;
  }

  class disable_guard {
   public:
    ~disable_guard() {
#if QLJS_DEBUG_BUMP_ALLOCATOR
      this->alloc_->disabled_count_ -= 1;
#endif
    }

   private:
#if QLJS_DEBUG_BUMP_ALLOCATOR
    explicit disable_guard(linked_bump_allocator* alloc) noexcept
        : alloc_(alloc) {
      this->alloc_->disabled_count_ -= 1;
    }
#else
    explicit disable_guard(linked_bump_allocator*) noexcept {}
#endif

#if QLJS_DEBUG_BUMP_ALLOCATOR
    linked_bump_allocator* alloc_;
#endif

    friend class linked_bump_allocator;
  };

  [[nodiscard]] disable_guard disable() noexcept { return disable_guard(this); }

 protected:
  void* do_allocate(std::size_t bytes, std::size_t align) override {
    QLJS_ASSERT(align <= Alignment);
    return this->allocate_bytes(this->align_up(bytes));
  }

  void do_deallocate(void* p, std::size_t bytes, std::size_t align) override {
    QLJS_ASSERT(align <= Alignment);
    this->deallocate_bytes(p, bytes);
  }

  bool do_is_equal(const boost::container::pmr::memory_resource& other) const
      noexcept override {
    return this == static_cast<const linked_bump_allocator*>(&other);
  }

 private:
  struct alignas(maximum(Alignment, alignof(void*))) chunk_header {
    chunk_header* previous;  // Linked list.
    std::size_t size;        // Size of the data portion in bytes.

    char* data() noexcept {
      return reinterpret_cast<char*>(this) + sizeof(*this);
    }
    char* data_end() noexcept { return this->data() + this->size; }

    static std::size_t allocation_size(std::size_t size) noexcept {
      return sizeof(chunk_header) + size;
    }

    static std::align_val_t allocation_alignment() noexcept {
      return std::align_val_t{maximum(Alignment, alignof(chunk_header))};
    }

    static chunk_header* new_chunk(std::size_t size, chunk_header* previous) {
#if QLJS_HAVE_SIZED_ALIGNED_NEW
      void* chunk =
          ::operator new(allocation_size(size), allocation_alignment());
#else
      void* chunk = ::operator new(allocation_size(size));
      QLJS_ASSERT(is_aligned(chunk));
#endif
      return new (chunk) chunk_header{
          .previous = previous,
          .size = size,
      };
    }

    static void delete_chunk(chunk_header* c) {
      [[maybe_unused]] std::size_t size = c->size;
      c->~chunk_header();
#if QLJS_HAVE_SIZED_ALIGNED_DELETE
      ::operator delete(c, allocation_size(size), allocation_alignment());
#else
      ::operator delete(c);
#endif
    }
  };

  static inline constexpr std::size_t default_chunk_size =
      4096 - sizeof(chunk_header);

  static constexpr std::size_t align_up(std::size_t size) noexcept {
    return (size + Alignment - 1) & ~(Alignment - 1);
  }

#if !QLJS_HAVE_SIZED_ALIGNED_NEW
  static bool is_aligned(void* p) noexcept {
    std::size_t alignment_mask = Alignment - 1;
    return (reinterpret_cast<std::uintptr_t>(p) & alignment_mask) == 0;
  }
#endif

  [[nodiscard]] void* allocate_bytes(std::size_t size) {
    this->assert_not_disabled();
    QLJS_SLOW_ASSERT(size % Alignment == 0);
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
    this->chunk_ = chunk_header::new_chunk(size, this->chunk_);
    this->next_allocation_ = this->chunk_->data();
    this->chunk_end_ = this->chunk_->data_end();
  }

  void assert_not_disabled() const {
#if QLJS_DEBUG_BUMP_ALLOCATOR
    QLJS_ALWAYS_ASSERT(!this->is_disabled());
#endif
  }

#if QLJS_DEBUG_BUMP_ALLOCATOR
  bool is_disabled() const noexcept { return this->disabled_count_ > 0; }
#endif

  chunk_header* chunk_ = nullptr;
  char* next_allocation_ = nullptr;
  char* chunk_end_ = nullptr;

#if QLJS_DEBUG_BUMP_ALLOCATOR
  int disabled_count_ = 0;
#endif
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
