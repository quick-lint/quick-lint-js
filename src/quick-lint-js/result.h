// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_RESULT_H
#define QUICK_LINT_JS_RESULT_H

#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/unreachable.h>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>

namespace quick_lint_js {
template <class T, class... Errors>
class result;

// Do not use result_error_tag directly. Call result<>::failure instead.
template <class Error>
struct result_error_tag {};

// Do not use result_base directly. Use result instead.
template <class T, class... Errors>
class result_base {
 private:
  using result = quick_lint_js::result<T, Errors...>;
  using value_type =
      std::conditional_t<std::is_same_v<T, void>, std::monostate, T>;

 public:
  template <class... Args>
  explicit result_base(Args&&... args)
      : data_(std::in_place_index<0>, std::forward<Args>(args)...) {}

  // Private constructor used by failure. Do not call directly.
  template <class Error, class... ErrorArgs>
  explicit result_base(result_error_tag<Error>, ErrorArgs&&... error_args)
      // TODO(strager): If std::is_same_v<Error, value_type>, then
      // std::in_place_type is incorrect.
      : data_(std::in_place_type<Error>,
              std::forward<ErrorArgs>(error_args)...) {}

  // TODO(strager): Allow copying.
  result_base(const result_base&) = delete;
  result_base& operator=(const result_base&) = delete;

  result_base(result_base&&) = default;
  result_base& operator=(result_base&&) = default;

  template <class Error>
  static result failure(Error&& error) {
    return result(result_error_tag<Error>(), std::forward<Error>(error));
  }

  template <class Error, class... ErrorArgs>
  static result failure(ErrorArgs&&... error_args) {
    return result(result_error_tag<Error>(),
                  std::forward<ErrorArgs>(error_args)...);
  }

  bool ok() const noexcept { return this->data_.index() == 0; }

  value_type& value() noexcept {
    QLJS_ASSERT(this->ok());
    return std::get<0>(this->data_);
  }

  template <class Error>
  bool has_error() const noexcept {
    // TODO(strager): If std::is_same_v<Error, value_type>, then
    // std::holds_alternative is incorrect.
    return std::holds_alternative<Error>(this->data_);
  }

  template <class Error>
  const Error& error() const noexcept {
    QLJS_ASSERT(!this->ok());
    QLJS_ASSERT(this->has_error<Error>());
    // TODO(strager): If std::is_same_v<Error, value_type>, then std::get<Error>
    // is incorrect.
    return std::get<Error>(this->data_);
  }

  value_type& operator*() noexcept { return this->value(); }
  value_type* operator->() noexcept { return &this->value(); }

 private:
  std::variant<value_type, Errors...> data_;
};

// Like std::variant<T, Errors...>, but more ergonomic.
//
// Errors must be unique. For example, result<int, char, char> is illegal.
template <class T, class... Errors>
class result : private result_base<T, Errors...> {
 private:
  using base = result_base<T, Errors...>;

 public:
  using base::base;
  using base::error;
  using base::failure;
  using base::has_error;
  using base::ok;
  using base::operator*;
  using base::operator->;
  using base::operator=;
  using base::value;
};

// Like std::variant<std::monostate, Errors...>, but more ergonomic.
//
// Errors must be unique. For example, result<void, char, char> is illegal.
template <class... Errors>
class result<void, Errors...> : private result_base<void, Errors...> {
 private:
  using base = result_base<void, Errors...>;

 public:
  using base::base;
  using base::error;
  using base::has_error;
  using base::ok;
  using base::operator=;
  using base::failure;
};

// Like std::variant<T, Error>, but more ergonomic.
template <class T, class Error>
class result<T, Error> : private result_base<T, Error> {
 private:
  using base = result_base<T, Error>;

 public:
  using base::base;
  using base::ok;
  using base::operator*;
  using base::operator->;
  using base::operator=;
  using base::value;

  template <class... ErrorArgs>
  static result failure(ErrorArgs&&... error_args) {
    return base::template failure<Error, ErrorArgs...>(
        std::forward<ErrorArgs>(error_args)...);
  }

  const Error& error() const noexcept { return base::template error<Error>(); }
};

// Like std::optional<Error>, but with a similar interface to result<T, Error>.
template <class Error>
class result<void, Error> : private result_base<void, Error> {
 private:
  using base = result_base<void, Error>;

 public:
  using base::base;
  using base::operator=;
  using base::ok;

  template <class... ErrorArgs>
  static result failure(ErrorArgs&&... error_args) {
    return base::template failure<Error, ErrorArgs...>(
        std::forward<ErrorArgs>(error_args)...);
  }

  const Error& error() const noexcept { return base::template error<Error>(); }
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
