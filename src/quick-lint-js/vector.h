// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_VECTOR_H
#define QUICK_LINT_JS_VECTOR_H

#include <boost/container/small_vector.hpp>
#include <cstddef>
#include <cstdint>
#include <iosfwd>
#include <map>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/force-inline.h>
#include <quick-lint-js/warning.h>
#include <string>
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

  static void dump_max_size_histogram(
      const std::map<std::string, std::map<std::size_t, int>> &,
      std::ostream &);
  static void dump_max_size_histogram(
      const std::map<std::string, std::map<std::size_t, int>> &, std::ostream &,
      int maximum_line_length);

  void add_entry(std::uintptr_t object_id, const char *owner,
                 vector_instrumentation::event event, std::size_t size,
                 std::size_t capacity);

  static void register_dump_on_exit_if_requested();

 private:
  std::vector<entry> entries_;
};

template <class T, std::size_t InSituCapacity = 0>
class vector {
 public:
  explicit vector(const char *debug_owner [[maybe_unused]]) noexcept
#if QLJS_FEATURE_VECTOR_PROFILING
      : debug_owner_(debug_owner)
#endif
  {
    this->add_instrumentation_entry(vector_instrumentation::event::create);
  }

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")
  explicit vector(const char *debug_owner [[maybe_unused]], const T *begin,
                  const T *end)
      : data_(begin, end)
#if QLJS_FEATURE_VECTOR_PROFILING
        ,
        debug_owner_(debug_owner)
#endif
  {
    this->add_instrumentation_entry(vector_instrumentation::event::create);
  }
  QLJS_WARNING_POP

  vector(const vector &) = delete;
  vector &operator=(const vector &) = delete;

#if QLJS_FEATURE_VECTOR_PROFILING
  vector(vector &&other) : vector(other.debug_owner_, std::move(other)) {}
#else
  vector(vector &&other) = default;
#endif

  vector(const char *debug_owner [[maybe_unused]], vector &&other)
      : data_(std::move(other.data_))
#if QLJS_FEATURE_VECTOR_PROFILING
        ,
        debug_owner_(debug_owner)
#endif
  {
    this->add_instrumentation_entry(vector_instrumentation::event::create);
    other.add_instrumentation_entry(vector_instrumentation::event::clear);
  }

  vector &operator=(vector &&other) {
    this->data_ = std::move(other.data_);
    this->add_instrumentation_entry(vector_instrumentation::event::assign);
    other.add_instrumentation_entry(vector_instrumentation::event::clear);
    return *this;
  }

  ~vector() {
    this->add_instrumentation_entry(vector_instrumentation::event::destroy);
  }

  QLJS_FORCE_INLINE T *data() noexcept { return this->data_.data(); }

  QLJS_FORCE_INLINE std::size_t size() const noexcept {
    return this->data_.size();
  }

  QLJS_FORCE_INLINE std::size_t capacity() const noexcept {
    return this->data_.capacity();
  }

  QLJS_FORCE_INLINE bool empty() const noexcept { return this->data_.empty(); }

  QLJS_FORCE_INLINE T &front() noexcept { return this->data_.front(); }

  QLJS_FORCE_INLINE T &back() noexcept { return this->data_.back(); }

  template <class... Args>
  QLJS_FORCE_INLINE T &emplace_back(Args &&... args) {
    T &result = this->data_.emplace_back(std::forward<Args>(args)...);
    this->add_instrumentation_entry(vector_instrumentation::event::append);
    return result;
  }

  QLJS_FORCE_INLINE void clear() {
    this->data_.clear();
    this->add_instrumentation_entry(vector_instrumentation::event::clear);
  }

 private:
#if QLJS_FEATURE_VECTOR_PROFILING
  QLJS_FORCE_INLINE void add_instrumentation_entry(
      vector_instrumentation::event event) {
    vector_instrumentation::instance.add_entry(
        /*object_id=*/reinterpret_cast<std::uintptr_t>(this),
        /*owner=*/this->debug_owner_,
        /*event=*/event,
        /*size=*/this->size(),
        /*capacity=*/this->capacity());
  }
#else
  QLJS_FORCE_INLINE void add_instrumentation_entry(
      vector_instrumentation::event) {}
#endif

  boost::container::small_vector<T, InSituCapacity> data_;
#if QLJS_FEATURE_VECTOR_PROFILING
  const char *debug_owner_;
#endif
};
}

#endif
