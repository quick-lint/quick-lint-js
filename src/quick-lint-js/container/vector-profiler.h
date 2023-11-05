// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iosfwd>
#include <map>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/attribute.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/synchronized.h>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace quick_lint_js {
struct Trace_Vector_Max_Size_Histogram_By_Owner_Entry;

// vector_instrumentation is thread-safe.
class Vector_Instrumentation {
 public:
  enum class Event {
    append,
    assign,
    clear,
    create,
    destroy,
    resize,
  };

  struct Entry {
    std::uintptr_t object_id;
    const char *owner;
    Vector_Instrumentation::Event event;
    std::uintptr_t data_pointer;
    std::size_t size;
    std::size_t capacity;

    friend std::ostream &operator<<(std::ostream &, const Entry &);
  };

#if QLJS_FEATURE_VECTOR_PROFILING
  static Vector_Instrumentation instance;
#endif

  void clear();

  // TODO(strager): Delete.
  std::vector<Entry> entries();

  std::vector<Entry> take_entries();

  void add_entry(std::uintptr_t object_id, const char *owner,
                 Vector_Instrumentation::Event event,
                 std::uintptr_t data_pointer, std::size_t size,
                 std::size_t capacity);

#if QLJS_FEATURE_VECTOR_PROFILING
  static void register_dump_on_exit_if_requested();
#endif

 private:
  Synchronized<std::vector<Entry>> entries_;
};

// vector_max_size_histogram_by_owner is *not* thread-safe.
class Vector_Max_Size_Histogram_By_Owner {
 public:
  explicit Vector_Max_Size_Histogram_By_Owner();

  Vector_Max_Size_Histogram_By_Owner(
      const Vector_Max_Size_Histogram_By_Owner &) = delete;
  Vector_Max_Size_Histogram_By_Owner &operator=(
      const Vector_Max_Size_Histogram_By_Owner &) = delete;

  ~Vector_Max_Size_Histogram_By_Owner();

  void add_entries(const std::vector<Vector_Instrumentation::Entry> &);

  Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry> histogram(
      Monotonic_Allocator *memory) const;

  struct Dump_Options {
    int maximum_line_length = (std::numeric_limits<int>::max)();
    int max_adjacent_empty_rows = (std::numeric_limits<int>::max)();
  };

  static void dump(Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>,
                   Output_Stream &);
  static void dump(Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>,
                   Output_Stream &, const Dump_Options &options);

 private:
  Hash_Map<const char *, Hash_Map<std::size_t, int>> histogram_;
  Hash_Map<std::pair<const char *, std::uintptr_t>, std::size_t> object_sizes_;
};

// vector_capacity_change_histogram_by_owner is *not* thread-safe.
class Vector_Capacity_Change_Histogram_By_Owner {
 public:
  struct Capacity_Change_Histogram {
    // Number of times an append caused the vector to create its initial
    // capacity.
    std::size_t appends_initial_capacity = 0;
    // Number of times an append used existing capacity.
    std::size_t appends_reusing_capacity = 0;
    // Number of times an append caused capacity to increase, copying old items.
    std::size_t appends_growing_capacity = 0;
  };

  explicit Vector_Capacity_Change_Histogram_By_Owner();

  Vector_Capacity_Change_Histogram_By_Owner(
      const Vector_Capacity_Change_Histogram_By_Owner &) = delete;
  Vector_Capacity_Change_Histogram_By_Owner &operator=(
      const Vector_Capacity_Change_Histogram_By_Owner &) = delete;

  ~Vector_Capacity_Change_Histogram_By_Owner();

  void add_entries(const std::vector<Vector_Instrumentation::Entry> &);

  std::map<std::string_view, Capacity_Change_Histogram> histogram() const;

  struct Dump_Options {
    int maximum_line_length = 80;
  };

  static void dump(
      const std::map<std::string_view, Capacity_Change_Histogram> &,
      Output_Stream &, const Dump_Options &);

 private:
  struct Vector_Info {
    std::uintptr_t data_pointer;
    std::size_t size;
  };

  std::map<std::string_view, Capacity_Change_Histogram> histogram_;
  std::map<std::uintptr_t, Vector_Info> objects_;
};

#if QLJS_FEATURE_VECTOR_PROFILING
template <class Vector>
class Instrumented_Vector {
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

  explicit Instrumented_Vector(const char *debug_owner,
                               const allocator_type &allocator)
      : data_(allocator), debug_owner_(debug_owner) {
    this->add_instrumentation_entry(Vector_Instrumentation::Event::create);
  }

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")
  explicit Instrumented_Vector(const char *debug_owner,
                               const allocator_type &allocator,
                               const value_type *begin, const value_type *end)
      : data_(begin, end, allocator), debug_owner_(debug_owner) {
    this->add_instrumentation_entry(Vector_Instrumentation::Event::create);
  }
  QLJS_WARNING_POP

  Instrumented_Vector(const Instrumented_Vector &) = delete;
  Instrumented_Vector &operator=(const Instrumented_Vector &) = delete;

  Instrumented_Vector(Instrumented_Vector &&other)
      : Instrumented_Vector(other.debug_owner_, std::move(other)) {}

  Instrumented_Vector(const char *debug_owner, Instrumented_Vector &&other)
      : data_(std::move(other.data_)), debug_owner_(debug_owner) {
    this->add_instrumentation_entry(Vector_Instrumentation::Event::create);
    other.add_instrumentation_entry(Vector_Instrumentation::Event::clear);
  }

  Instrumented_Vector &operator=(Instrumented_Vector &&other) {
    this->data_ = std::move(other.data_);
    this->add_instrumentation_entry(Vector_Instrumentation::Event::assign);
    other.add_instrumentation_entry(Vector_Instrumentation::Event::clear);
    return *this;
  }

  ~Instrumented_Vector() {
    this->add_instrumentation_entry(Vector_Instrumentation::Event::destroy);
  }

  QLJS_FORCE_INLINE allocator_type get_allocator() const {
    return this->data_.get_allocator();
  }

  QLJS_FORCE_INLINE value_type *data() { return this->data_.data(); }
  QLJS_FORCE_INLINE const value_type *data() const {
    return this->data_.data();
  }

  QLJS_FORCE_INLINE size_type size() const { return this->data_.size(); }

  QLJS_FORCE_INLINE size_type capacity() const {
    return this->data_.capacity();
  }

  QLJS_FORCE_INLINE bool empty() const { return this->data_.empty(); }

  QLJS_FORCE_INLINE value_type &front() { return this->data_.front(); }
  QLJS_FORCE_INLINE value_type &back() { return this->data_.back(); }

  QLJS_FORCE_INLINE const value_type &front() const {
    return this->data_.front();
  }
  QLJS_FORCE_INLINE const value_type &back() const {
    return this->data_.back();
  }

  QLJS_FORCE_INLINE value_type &operator[](size_type index) {
    return this->data_[index];
  }

  QLJS_FORCE_INLINE const value_type &operator[](size_type index) const {
    return this->data_[index];
  }

  QLJS_FORCE_INLINE value_type *begin() { return this->data(); }
  QLJS_FORCE_INLINE value_type *end() { return this->begin() + this->size(); }

  QLJS_FORCE_INLINE const value_type *begin() const { return this->data(); }
  QLJS_FORCE_INLINE const value_type *end() const {
    return this->begin() + this->size();
  }

  template <class... Args>
  QLJS_FORCE_INLINE value_type &emplace_back(Args &&... args) {
    this->data_.emplace_back(std::forward<Args>(args)...);
    this->add_instrumentation_entry(Vector_Instrumentation::Event::append);
    return this->data_.back();
  }

  QLJS_FORCE_INLINE value_type &push_back(value_type &&value) {
    this->data_.push_back(std::move(value));
    this->add_instrumentation_entry(Vector_Instrumentation::Event::append);
    return this->data_.back();
  }

  QLJS_FORCE_INLINE value_type &push_back(const value_type &value) {
    this->data_.push_back(value);
    this->add_instrumentation_entry(Vector_Instrumentation::Event::append);
    return this->data_.back();
  }

  QLJS_FORCE_INLINE void pop_back() {
    this->data_.pop_back();
    this->add_instrumentation_entry(Vector_Instrumentation::Event::resize);
  }

  QLJS_FORCE_INLINE void push_front(value_type &&value) {
    this->data_.push_front(std::move(value));
    // TODO(strager): Add instrumentation specific to prepending.
    this->add_instrumentation_entry(Vector_Instrumentation::Event::resize);
  }

  QLJS_FORCE_INLINE void clear() {
    this->data_.clear();
    this->add_instrumentation_entry(Vector_Instrumentation::Event::clear);
  }

  QLJS_FORCE_INLINE void erase(iterator begin, iterator end) {
    this->data_.erase(begin, end);
    // TODO(strager): Add instrumentation specific to erasing.
    this->add_instrumentation_entry(Vector_Instrumentation::Event::resize);
  }

  void reserve(size_type new_capacity) { this->data_.reserve(new_capacity); }

  void resize(size_type new_size) {
    this->data_.resize(new_size);
    this->add_instrumentation_entry(Vector_Instrumentation::Event::resize);
  }

  // NOTE(strager): This is a non-standard function.
  void release() { this->data_.release(); }

  // NOTE(strager): This is a non-standard function.
  Span<value_type> get_and_release() { return this->data_.get_and_release(); }

  // NOTE(strager): This is a non-standard function.
  void append(const value_type *begin, const value_type *end) {
    this->data_.append(begin, end);
    this->add_instrumentation_entry(Vector_Instrumentation::Event::append);
  }

  // NOTE(strager): This is a non-standard function.
  void append(size_type count, value_type value) {
    this->data_.append(count, value);
    this->add_instrumentation_entry(Vector_Instrumentation::Event::append);
  }

  // NOTE(strager): This is a non-standard function.
  Instrumented_Vector &operator+=(value_type value) {
    this->data_ += value;
    this->add_instrumentation_entry(Vector_Instrumentation::Event::append);
    return *this;
  }

  // NOTE(strager): This is a non-standard function.
  Instrumented_Vector &operator+=(std::basic_string_view<value_type> values) {
    this->data_ += values;
    this->add_instrumentation_entry(Vector_Instrumentation::Event::append);
    return *this;
  }

  // NOTE(strager): This is a non-standard function.
  std::basic_string_view<value_type> to_string_view() const {
    return this->data_.to_string_view();
  }

  // NOTE(strager): This is a non-standard function.
  Span<value_type> release_to_span() { return this->data_.release_to_span(); }

  // NOTE(strager): This is a non-standard function.
  std::basic_string_view<value_type> release_to_string_view() {
    return this->data_.release_to_string_view();
  }

  // NOTE(strager): This is a non-standard function.
  explicit operator Span<value_type>() { return Span<value_type>(this->data_); }

  // NOTE(strager): This is a non-standard function.
  explicit operator Span<const value_type>() const noexcept {
    return Span<const value_type>(this->data_);
  }

 private:
  QLJS_FORCE_INLINE void add_instrumentation_entry(
      Vector_Instrumentation::Event event) {
    Vector_Instrumentation::instance.add_entry(
        /*object_id=*/reinterpret_cast<std::uintptr_t>(this),
        /*owner=*/this->debug_owner_,
        /*event=*/event,
        /*data_pointer=*/reinterpret_cast<std::uintptr_t>(this->data()),
        /*size=*/narrow_cast<std::size_t>(this->size()),
        /*capacity=*/narrow_cast<std::size_t>(this->capacity()));
  }

  Vector data_;
  const char *debug_owner_;
};
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
