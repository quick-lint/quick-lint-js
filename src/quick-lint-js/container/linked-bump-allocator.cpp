// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/flexible-array.h>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/math.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/math-overflow.h>
#include <quick-lint-js/util/pointer.h>
#include <utility>

#if QLJS_HAVE_SANITIZER_ASAN_INTERFACE_H
#include <sanitizer/asan_interface.h>
#endif

namespace quick_lint_js {
Linked_Bump_Allocator::Linked_Bump_Allocator(const char* debug_owner) {
  static_cast<void>(debug_owner);
}

Linked_Bump_Allocator::~Linked_Bump_Allocator() { this->release(); }

void Linked_Bump_Allocator::release() {
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

void Linked_Bump_Allocator::rewind(Rewind_State&& r) {
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

Linked_Bump_Allocator::Disable_Guard::~Disable_Guard() {
#if QLJS_DEBUG_BUMP_ALLOCATOR
  this->alloc_->disabled_count_ -= 1;
#endif
}

Linked_Bump_Allocator::Disable_Guard::Disable_Guard([
    [maybe_unused]] Linked_Bump_Allocator* alloc)
#if QLJS_DEBUG_BUMP_ALLOCATOR
    : alloc_(alloc)
#endif
{
#if QLJS_DEBUG_BUMP_ALLOCATOR
  this->alloc_->disabled_count_ += 1;
#endif
}

void* Linked_Bump_Allocator::do_allocate(std::size_t bytes, std::size_t align) {
  return this->allocate_bytes(bytes, align);
}

void Linked_Bump_Allocator::do_deallocate(void* p, std::size_t bytes,
                                          [[maybe_unused]] std::size_t align) {
  this->deallocate_bytes(p, bytes);
}

[[nodiscard]] void* Linked_Bump_Allocator::allocate_bytes(std::size_t size,
                                                          std::size_t align) {
  this->assert_not_disabled();
  QLJS_SLOW_ASSERT(size % align == 0);

  std::uintptr_t next_allocation_int =
      reinterpret_cast<std::uintptr_t>(this->next_allocation_);
  // NOTE(strager): align_up might overflow. If it does, the allocation
  // couldn't fit due to alignment alone
  // (alignment_padding > this->remaining_bytes_in_current_chunk()).
  std::uintptr_t result_int = align_up(next_allocation_int, align);
  char* result = reinterpret_cast<char*>(result_int);

  // alignment_padding is how much the pointer moved due to alignment, i.e.
  // how much padding is necessary due to alignment.
  std::uintptr_t alignment_padding = result_int - next_allocation_int;
  bool have_enough_space =
      (alignment_padding + size) <= this->remaining_bytes_in_current_chunk();
  if (!have_enough_space) [[unlikely]] {
      this->append_chunk(maximum(size, this->default_chunk_size), align);
      result = this->next_allocation_;
      QLJS_ASSERT(is_aligned(result, align));
    }

  this->next_allocation_ = result + size;
  this->did_allocate_bytes(result, size);
  return result;
}

void Linked_Bump_Allocator::did_allocate_bytes(
    [[maybe_unused]] void* p, [[maybe_unused]] std::size_t size) {
#if QLJS_HAVE_SANITIZER_ASAN_INTERFACE_H
  ASAN_UNPOISON_MEMORY_REGION(p, size);
#endif
  // TODO(strager): Mark memory as usable for Valgrind.
}

void Linked_Bump_Allocator::did_deallocate_bytes(
    [[maybe_unused]] void* p, [[maybe_unused]] std::size_t size) {
#if QLJS_HAVE_SANITIZER_ASAN_INTERFACE_H
  ASAN_POISON_MEMORY_REGION(p, size);
#endif
  // TODO(strager): Mark memory as unusable for Valgrind.
}

void Linked_Bump_Allocator::append_chunk(std::size_t size, std::size_t align) {
  // Over-alignment is tested by
  // NOTE[Linked_Bump_Allocator-append_chunk-alignment].
  bool is_over_aligned = align > Chunk::capacity_alignment;
  std::size_t allocated_size = size;
  if (is_over_aligned) {
    allocated_size += align;
  }
  this->chunk_ = Chunk::allocate_and_construct_header(
      new_delete_resource(), allocated_size, this->chunk_);
  std::uintptr_t next_allocation_int =
      reinterpret_cast<std::uintptr_t>(this->chunk_->flexible_capacity_begin());
  if (is_over_aligned) {
    next_allocation_int = align_up(next_allocation_int, align);
  }
  this->next_allocation_ = reinterpret_cast<char*>(next_allocation_int);
  this->chunk_end_ = this->chunk_->flexible_capacity_end();
  QLJS_ASSERT(is_aligned(this->next_allocation_, align));
  QLJS_ASSERT(this->remaining_bytes_in_current_chunk() >= size);
}

void Linked_Bump_Allocator::assert_not_disabled() const {
#if QLJS_DEBUG_BUMP_ALLOCATOR
  QLJS_ALWAYS_ASSERT(!this->is_disabled());
#endif
}

#if QLJS_DEBUG_BUMP_ALLOCATOR
bool Linked_Bump_Allocator::is_disabled() const {
  return this->disabled_count_ > 0;
}
#endif
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
