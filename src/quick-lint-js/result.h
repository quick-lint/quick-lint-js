// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_RESULT_H
#define QUICK_LINT_JS_RESULT_H

#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/variant.h>
#include <string>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
template <class T, class... Errors>
class result;
template <class T, class... Errors>
class result_base;

// Do not use result_propagation directly. Call result<>::propagate instead.
template <class T, class... Errors>
struct result_propagation {
  result_base<T, Errors...>& to_propagate;
};

// Do not use result_error_tag directly. Call result<>::failure instead.
template <class Error>
struct result_error_tag {};

// Do not use result_base directly. Use result instead.
template <class T, class... Errors>
class result_base {
 private:
  template <class U>
  using to_value_type =
      std::conditional_t<std::is_same_v<U, void>, monostate, U>;

  using result = quick_lint_js::result<T, Errors...>;
  using value_type = to_value_type<T>;

 public:
  // TODO(strager): Make explicit iff T(Args...) is explicit.
  template <class... Args,
            class = decltype(value_type(std::declval<Args>()...))>
  /*implicit*/ result_base(Args&&... args)
      : data_(std::in_place_index<0>, std::forward<Args>(args)...) {}

  // Private constructor used by failure. Do not call directly.
  template <class Error, class... ErrorArgs>
  explicit result_base(result_error_tag<Error>, ErrorArgs&&... error_args)
      // TODO(strager): If std::is_same_v<Error, value_type>, then
      // std::in_place_type is incorrect.
      : data_(std::in_place_type<Error>,
              std::forward<ErrorArgs>(error_args)...) {}

  // Private constructor used by propagate. Do not call directly.
  /*implicit*/ result_base(result_propagation<T, Errors...>&& propagation)
      : data_(std::move(propagation.to_propagate).data_) {}

  // Private constructor used by propagate. Do not call directly.
  template <class U, class... OtherErrors>
  /*implicit*/ result_base(result_propagation<U, OtherErrors...>&& propagation)
      : data_(quick_lint_js::visit(propagate_visitor<U>(),
                                   std::move(propagation.to_propagate).data_)) {
  }

  // TODO(strager): Allow copying.
  result_base(const result_base&) = delete;
  result_base& operator=(const result_base&) = delete;

  result_base(result_base&&) = default;
  result_base& operator=(result_base&&) = default;

  template <class... OtherErrors>
  /*implicit*/ result_base(quick_lint_js::result<T, OtherErrors...>&& other)
      : data_(visit(move_data_visitor(), std::move(other).data_)) {}

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

  value_type& value() & noexcept {
    QLJS_ASSERT(this->ok());
    return get<0>(this->data_);
  }

  value_type&& value() && noexcept {
    QLJS_ASSERT(this->ok());
    return get<0>(std::move(this->data_));
  }

  const value_type& value() const& noexcept {
    QLJS_ASSERT(this->ok());
    return get<0>(this->data_);
  }

  template <class Error>
  bool has_error() const noexcept {
    // TODO(strager): If std::is_same_v<Error, value_type>, then
    // holds_alternative is incorrect.
    return holds_alternative<Error>(this->data_);
  }

  template <class Error>
  const Error& error() const noexcept {
    QLJS_ASSERT(!this->ok());
    QLJS_ASSERT(this->has_error<Error>());
    // TODO(strager): If std::is_same_v<Error, value_type>, then get<Error>
    // is incorrect.
    return get<Error>(this->data_);
  }

  std::string error_to_string() const {
    QLJS_ASSERT(!this->ok());
    // TODO(strager): If std::is_same_v<Error, value_type>, then visit is
    // incorrect.
    return visit(to_string_visitor(), this->data_);
  }

  result_propagation<T, Errors...> propagate() & {
    QLJS_ASSERT(!this->ok());
    return result_propagation<T, Errors...>{*this};
  }

  result_propagation<T, Errors...> propagate() && = delete;

  template <class... NewErrors>
  quick_lint_js::result<void, NewErrors...> copy_errors() {
    // TODO(strager): If any of NewErrors equals value_type, then visit is
    // incorrect.
    return visit(copy_errors_visitor<NewErrors...>(), this->data_);
  }

  // For tests.
  template <class U>
  friend bool holds_alternative(const result_base<T, Errors...>& r) noexcept {
    // TODO(strager): Make this work properly if T is void.
    return holds_alternative<U>(r.data_);
  }

  // For tests.
  template <class U>
  friend const U& get(const result_base<T, Errors...>& r) noexcept {
    // TODO(strager): Make this work properly if T is void.
    return get<U>(r.data_);
  }

  value_type& operator*() & noexcept { return this->value(); }
  value_type&& operator*() && noexcept { return std::move(*this).value(); }
  const value_type& operator*() const& noexcept { return this->value(); }

  value_type* operator->() noexcept { return &this->value(); }
  const value_type* operator->() const noexcept { return &this->value(); }

  friend bool operator==(const result_base& lhs,
                         const result_base& rhs) noexcept {
    return lhs.data_ == rhs.data_;
  }

  friend bool operator!=(const result_base& lhs,
                         const result_base& rhs) noexcept {
    return !(lhs == rhs);
  }

 private:
  struct move_data_visitor {
    template <class TOrError>
    auto operator()(TOrError&& datum) {
      // TODO(strager): If (std::is_same_v<Errors, value_type> || ...),
      // then std::in_place_type is incorrect.
      return variant<value_type, Errors...>(
          std::in_place_type<std::decay_t<TOrError>>, std::move(datum));
    }
  };

  template <class U>
  struct propagate_visitor {
    variant<value_type, Errors...> operator()(to_value_type<U>&&) {
      QLJS_UNREACHABLE();
    }

    template <class Error>
    variant<value_type, Errors...> operator()(Error&& error) {
      // TODO(strager): If (std::is_same_v<Errors, value_type> || ...), then
      // std::in_place_type is incorrect.
      return variant<value_type, Errors...>(
          std::in_place_type<std::decay_t<Error>>, std::move(error));
    }
  };

  struct to_string_visitor {
    std::string operator()(const value_type&) { QLJS_UNREACHABLE(); }

    template <class Error>
    std::string operator()(const Error& error) {
      return error.to_string();
    }
  };

  template <class... NewErrors>
  struct copy_errors_visitor {
    quick_lint_js::result<void, NewErrors...> operator()(const value_type&) {
      return {};
    }

    template <class Error>
    quick_lint_js::result<void, NewErrors...> operator()(const Error& error) {
      return quick_lint_js::result<void, NewErrors...>(
          result_error_tag<Error>(), error);
    }
  };

  variant<value_type, Errors...> data_;

  template <class, class...>
  friend class result_base;
};

// Like variant<T, Errors...>, but more ergonomic.
//
// Errors must be unique. For example, result<int, char, char> is illegal.
template <class T, class... Errors>
class result : public result_base<T, Errors...> {
 private:
  using base = result_base<T, Errors...>;

 public:
  using base::base;
  using base::copy_errors;
  using base::error;
  using base::error_to_string;
  using base::failure;
  using base::has_error;
  using base::ok;
  using base::operator*;
  using base::operator->;
  using base::operator=;
  using base::propagate;
  using base::value;

  template <class, class...>
  friend class result_base;
};

// Like variant<monostate, Errors...>, but more ergonomic.
//
// Errors must be unique. For example, result<void, char, char> is illegal.
template <class... Errors>
class result<void, Errors...> : public result_base<void, Errors...> {
 private:
  using base = result_base<void, Errors...>;

 public:
  using base::base;
  using base::copy_errors;
  using base::error;
  using base::error_to_string;
  using base::failure;
  using base::has_error;
  using base::ok;
  using base::operator=;
  using base::propagate;

  template <class, class...>
  friend class result_base;
};

// Like variant<T, Error>, but more ergonomic.
template <class T, class Error>
class result<T, Error> : public result_base<T, Error> {
 private:
  using base = result_base<T, Error>;

 public:
  using base::base;
  using base::copy_errors;
  using base::error_to_string;
  using base::ok;
  using base::operator*;
  using base::operator->;
  using base::operator=;
  using base::propagate;
  using base::value;

  template <class... ErrorArgs>
  static result failure(ErrorArgs&&... error_args) {
    return base::template failure<Error, ErrorArgs...>(
        std::forward<ErrorArgs>(error_args)...);
  }

  const Error& error() const noexcept { return base::template error<Error>(); }

  template <class, class...>
  friend class result_base;
};

// Like std::optional<Error>, but with a similar interface to result<T, Error>.
template <class Error>
class result<void, Error> : public result_base<void, Error> {
 private:
  using base = result_base<void, Error>;

 public:
  using base::base;
  using base::copy_errors;
  using base::error_to_string;
  using base::ok;
  using base::operator=;
  using base::propagate;

  template <class... ErrorArgs>
  static result failure(ErrorArgs&&... error_args) {
    return base::template failure<Error, ErrorArgs...>(
        std::forward<ErrorArgs>(error_args)...);
  }

  const Error& error() const noexcept { return base::template error<Error>(); }

  template <class, class...>
  friend class result_base;
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
