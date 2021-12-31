// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VECTOR_PROFILER_H
#define QUICK_LINT_JS_VECTOR_PROFILER_H

#include <algorithm>
#include <boost/container/pmr/polymorphic_allocator.hpp>
#include <cstddef>
#include <cstdint>
#include <iosfwd>
#include <map>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/force-inline.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace quick_lint_js {
class vector_instrumentation {
 public:
  enum class event {
    append,
    assign,
    clear,
    create,
    destroy,
  };

  struct entry {
    std::uintptr_t object_id;
    const char *owner;
    vector_instrumentation::event event;
    std::uintptr_t data_pointer;
    std::size_t size;
    std::size_t capacity;

    friend std::ostream &operator<<(std::ostream &, const entry &);
  };

#if QLJS_FEATURE_VECTOR_PROFILING
  static vector_instrumentation instance;
#endif

  void clear();
  std::vector<entry> entries() const;

  std::map<std::string, std::map<std::size_t, int>>
  max_size_histogram_by_owner() const;

  struct dump_options {
    int maximum_line_length = (std::numeric_limits<int>::max)();
    int max_adjacent_empty_rows = (std::numeric_limits<int>::max)();
  };

  static void dump_max_size_histogram(
      const std::map<std::string, std::map<std::size_t, int>> &,
      std::ostream &);
  static void dump_max_size_histogram(
      const std::map<std::string, std::map<std::size_t, int>> &, std::ostream &,
      const dump_options &options);

  struct capacity_change_histogram {
    // Number of times an append caused the vector to create its initial
    // capacity.
    std::size_t appends_initial_capacity = 0;
    // Number of times an append used existing capacity.
    std::size_t appends_reusing_capacity = 0;
    // Number of times an append caused capacity to increase, copying old items.
    std::size_t appends_growing_capacity = 0;
  };

  std::map<std::string, capacity_change_histogram>
  capacity_change_histogram_by_owner() const;

  struct dump_capacity_change_options {
    int maximum_line_length = 80;
  };

  static void dump_capacity_change_histogram(
      const std::map<std::string, capacity_change_histogram> &, std::ostream &,
      const dump_capacity_change_options &);

  void add_entry(std::uintptr_t object_id, const char *owner,
                 vector_instrumentation::event event,
                 std::uintptr_t data_pointer, std::size_t size,
                 std::size_t capacity);

  static void register_dump_on_exit_if_requested();

 private:
  std::vector<entry> entries_;
};

#if QLJS_FEATURE_VECTOR_PROFILING
template <class Vector>
class instrumented_vector {
 public:
  using allocator_type = typename Vector::allocator_type;
  using const_iterator = typename Vector::const_iterator;
  using const_pointer = typename Vector::const_pointer;
  using const_reference = typename Vector::const_reference;
  using difference_type = typename Vector::difference_type;
  using iterator = typename Vector::iterator;
  using pointer = typename Vector::pointer;
  using reference = typename Vector::reference;
  using size_type = typename Vector::size_type;
  using value_type = typename Vector::value_type;

  explicit instrumented_vector(const char *debug_owner,
                               const allocator_type &allocator) noexcept
      : data_(allocator), debug_owner_(debug_owner) {
    this->add_instrumentation_entry(vector_instrumentation::event::create);
  }

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")
  explicit instrumented_vector(const char *debug_owner,
                               const allocator_type &allocator,
                               const value_type *begin, const value_type *end)
      : data_(begin, end, allocator), debug_owner_(debug_owner) {
    this->add_instrumentation_entry(vector_instrumentation::event::create);
  }
  QLJS_WARNING_POP

  instrumented_vector(const instrumented_vector &) = delete;
  instrumented_vector &operator=(const instrumented_vector &) = delete;

  instrumented_vector(instrumented_vector &&other)
      : instrumented_vector(other.debug_owner_, std::move(other)) {}

  instrumented_vector(const char *debug_owner, instrumented_vector &&other)
      : data_(std::move(other.data_)), debug_owner_(debug_owner) {
    this->add_instrumentation_entry(vector_instrumentation::event::create);
    other.add_instrumentation_entry(vector_instrumentation::event::clear);
  }

  instrumented_vector &operator=(instrumented_vector &&other) {
    this->data_ = std::move(other.data_);
    this->add_instrumentation_entry(vector_instrumentation::event::assign);
    other.add_instrumentation_entry(vector_instrumentation::event::clear);
    return *this;
  }

  ~instrumented_vector() {
    this->add_instrumentation_entry(vector_instrumentation::event::destroy);
  }

  QLJS_FORCE_INLINE allocator_type get_allocator() const noexcept {
    return this->data_.get_allocator();
  }

  QLJS_FORCE_INLINE value_type *data() noexcept { return this->data_.data(); }
  QLJS_FORCE_INLINE const value_type *data() const noexcept {
    return this->data_.data();
  }

  QLJS_FORCE_INLINE size_type size() const noexcept {
    return this->data_.size();
  }

  QLJS_FORCE_INLINE size_type capacity() const noexcept {
    return this->data_.capacity();
  }

  QLJS_FORCE_INLINE bool empty() const noexcept { return this->data_.empty(); }

  QLJS_FORCE_INLINE value_type &front() noexcept { return this->data_.front(); }

  QLJS_FORCE_INLINE value_type &back() noexcept { return this->data_.back(); }

  QLJS_FORCE_INLINE value_type *begin() noexcept { return this->data(); }
  QLJS_FORCE_INLINE value_type *end() noexcept {
    return this->begin() + this->size();
  }

  QLJS_FORCE_INLINE const value_type *begin() const noexcept {
    return this->data();
  }
  QLJS_FORCE_INLINE const value_type *end() const noexcept {
    return this->begin() + this->size();
  }

  template <class... Args>
  QLJS_FORCE_INLINE value_type &emplace_back(Args &&... args) {
    value_type &result = this->data_.emplace_back(std::forward<Args>(args)...);
    this->add_instrumentation_entry(vector_instrumentation::event::append);
    return result;
  }

  QLJS_FORCE_INLINE void clear() {
    this->data_.clear();
    this->add_instrumentation_entry(vector_instrumentation::event::clear);
  }

  void reserve(std::size_t new_capacity) { this->data_.reserve(new_capacity); }

  // NOTE(strager): This is a non-standard function.
  void release() { this->data_.release(); }

 private:
  QLJS_FORCE_INLINE void add_instrumentation_entry(
      vector_instrumentation::event event) {
    vector_instrumentation::instance.add_entry(
        /*object_id=*/reinterpret_cast<std::uintptr_t>(this),
        /*owner=*/this->debug_owner_,
        /*event=*/event,
        /*object_id=*/reinterpret_cast<std::uintptr_t>(this->data()),
        /*size=*/this->size(),
        /*capacity=*/this->capacity());
  }

  Vector data_;
  const char *debug_owner_;
};
#endif
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
