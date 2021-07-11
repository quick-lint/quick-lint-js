// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_RESULT_H
#define QUICK_LINT_JS_RESULT_H

#include <new>
#include <quick-lint-js/assert.h>
#include <string>
#include <utility>

namespace quick_lint_js {
// Like std::variant<T, Error>, but more ergonomic.
template <class T, class Error>
class result {
 public:
  template <class... Args>
  explicit result(Args&&... args)
      : value_(std::forward<Args>(args)...), ok_(true) {}

  // TODO(strager): Allow copying.
  result(const result&) = delete;
  result& operator=(const result&) = delete;

  result(result&& other) noexcept : ok_(other.ok_) {
    if (this->ok_) {
      new (&this->value_) T(std::move(other.value_));
    } else {
      new (&this->error_) Error(std::move(other.error_));
    }
  }

  result& operator=(result&& other) noexcept {
    this->~result();
    new (this) result(std::move(other));
    return *this;
  }

  ~result() {
    if (this->ok_) {
      this->value_.~T();
    } else {
      this->error_.~Error();
    }
  }

  template <class... ErrorArgs>
  static result failure(ErrorArgs&&... error_args) {
    return result(error_tag(), std::forward<ErrorArgs>(error_args)...);
  }

  bool ok() const noexcept { return this->ok_; }

  T& value() noexcept {
    QLJS_ASSERT(this->ok());
    return this->value_;
  }

  const Error& error() const noexcept {
    QLJS_ASSERT(!this->ok());
    return this->error_;
  }

  T& operator*() noexcept { return this->value(); }
  T* operator->() noexcept { return &this->value(); }

 private:
  struct error_tag {};

  template <class... ErrorArgs>
  explicit result(error_tag, ErrorArgs&&... error_args)
      : error_(std::forward<ErrorArgs>(error_args)...), ok_(false) {}

  union {
    T value_;
    Error error_;
  };
  bool ok_;
};

// Like std::variant<T, std::string>, but more ergonomic.
//
// The stored std::string represents an error message.
//
// sloppy_result is named "sloppy" because strings are a poor way to communicate
// errors in general. sloppy_result is mostly intended for tests or for places
// where an error can't be recovered from, not for most production code. If you
// want more type-safe error handling, use boost::leaf::result instead.
template <class T>
using sloppy_result = result<T, std::string>;
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
