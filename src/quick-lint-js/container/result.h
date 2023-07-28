// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_RESULT_H
#define QUICK_LINT_JS_CONTAINER_RESULT_H

#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/variant.h>
#include <quick-lint-js/port/unreachable.h>
#include <string>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
template <class T, class... Errors>
class Result;
template <class T, class... Errors>
class Result_Base;

// Do not use result_propagation directly. Call Result<>::propagate instead.
template <class T, class... Errors>
struct Result_Propagation {
  Result_Base<T, Errors...>& to_propagate;
};

// Do not use result_error directly. Call failed_result instead.
template <class Error>
struct Result_Error {
  static_assert(std::is_reference_v<Error&&>);
  Error&& e;
};

// After calling failed_result, you must immediately construct a new result or
// assign to an existing result.
template <class Error>
Result_Error<Error&&> failed_result(Error&& e) {
  return Result_Error<Error&&>{.e = std::forward<Error&&>(e)};
}

// Do not use result_base directly. Use result instead.
template <class T, class... Errors>
class Result_Base {
 private:
  template <class U>
  using To_Value_Type =
      std::conditional_t<std::is_same_v<U, void>, Monostate, U>;

  using Result = quick_lint_js::Result<T, Errors...>;
  using Value_Type = To_Value_Type<T>;

 public:
  // TODO(strager): Make explicit iff T(Args...) is explicit.
  template <class... Args,
            class = decltype(Value_Type(std::declval<Args>()...))>
  /*implicit*/ Result_Base(Args&&... args)
      : data_(std::in_place_index<0>, std::forward<Args>(args)...) {}

  template <class Error>
  /*implicit*/ Result_Base(Result_Error<Error>&& error)
      // TODO(strager): If std::is_same_v<Error, value_type>, then
      // std::in_place_type is incorrect.
      : data_(std::in_place_type<std::decay_t<Error>>,
              std::forward<Error>(error.e)) {}

  // Private constructor used by propagate. Do not call directly.
  /*implicit*/ Result_Base(Result_Propagation<T, Errors...>&& propagation)
      : data_(std::move(propagation.to_propagate).data_) {}

  // Private constructor used by propagate. Do not call directly.
  template <class U, class... OtherErrors>
  /*implicit*/ Result_Base(Result_Propagation<U, OtherErrors...>&& propagation)
      : data_(quick_lint_js::visit(Propagate_Visitor<U>(),
                                   std::move(propagation.to_propagate).data_)) {
  }

  // TODO(strager): Allow copying.
  Result_Base(const Result_Base&) = delete;
  Result_Base& operator=(const Result_Base&) = delete;

  Result_Base(Result_Base&&) = default;
  Result_Base& operator=(Result_Base&&) = default;

  template <class... OtherErrors>
  /*implicit*/ Result_Base(quick_lint_js::Result<T, OtherErrors...>&& other)
      : data_(visit(Move_Data_Visitor(), std::move(other).data_)) {}

  // FIXME(strager): The following code is a landmine:
  //
  //   r = failed_result(std::move(r.error()));
  template <class Error>
  Result_Base& operator=(Result_Error<Error>&& error) {
    // TODO(strager): If std::is_same_v<Error, value_type>, then the emplace
    // template parameter is incorrect.
    this->data_.template emplace<std::decay_t<Error>>(
        std::forward<Error>(error.e));
    return *this;
  }

  bool ok() const { return this->data_.index() == 0; }

  Value_Type& value() & {
    QLJS_ASSERT(this->ok());
    return get<0>(this->data_);
  }

  Value_Type&& value() && {
    QLJS_ASSERT(this->ok());
    return get<0>(std::move(this->data_));
  }

  const Value_Type& value() const& {
    QLJS_ASSERT(this->ok());
    return get<0>(this->data_);
  }

  template <class Error>
  bool has_error() const {
    // TODO(strager): If std::is_same_v<Error, value_type>, then
    // holds_alternative is incorrect.
    return holds_alternative<Error>(this->data_);
  }

  template <class Error>
  const Error& error() const {
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
    return visit(To_String_Visitor(), this->data_);
  }

  Result_Propagation<T, Errors...> propagate() & {
    QLJS_ASSERT(!this->ok());
    return Result_Propagation<T, Errors...>{*this};
  }

  Result_Propagation<T, Errors...> propagate() && = delete;

  template <class... NewErrors>
  quick_lint_js::Result<void, NewErrors...> copy_errors() {
    // TODO(strager): If any of NewErrors equals value_type, then visit is
    // incorrect.
    return visit(Copy_Errors_Visitor<NewErrors...>(), this->data_);
  }

  // For tests.
  template <class U>
  friend bool holds_alternative(const Result_Base<T, Errors...>& r) {
    // TODO(strager): Make this work properly if T is void.
    return holds_alternative<U>(r.data_);
  }

  // For tests.
  template <class U>
  friend const U& get(const Result_Base<T, Errors...>& r) {
    // TODO(strager): Make this work properly if T is void.
    return get<U>(r.data_);
  }

  Value_Type& operator*() & { return this->value(); }
  Value_Type&& operator*() && { return std::move(*this).value(); }
  const Value_Type& operator*() const& { return this->value(); }

  Value_Type* operator->() { return &this->value(); }
  const Value_Type* operator->() const { return &this->value(); }

  friend bool operator==(const Result_Base& lhs, const Result_Base& rhs) {
    return lhs.data_ == rhs.data_;
  }

  friend bool operator!=(const Result_Base& lhs, const Result_Base& rhs) {
    return !(lhs == rhs);
  }

 private:
  struct Move_Data_Visitor {
    template <class TOrError>
    auto operator()(TOrError&& datum) {
      // TODO(strager): If (std::is_same_v<Errors, value_type> || ...),
      // then std::in_place_type is incorrect.
      return Variant<Value_Type, Errors...>(
          std::in_place_type<std::decay_t<TOrError>>, std::move(datum));
    }
  };

  template <class U>
  struct Propagate_Visitor {
    Variant<Value_Type, Errors...> operator()(To_Value_Type<U>&&) {
      QLJS_UNREACHABLE();
    }

    template <class Error>
    Variant<Value_Type, Errors...> operator()(Error&& error) {
      // TODO(strager): If (std::is_same_v<Errors, value_type> || ...), then
      // std::in_place_type is incorrect.
      return Variant<Value_Type, Errors...>(
          std::in_place_type<std::decay_t<Error>>, std::move(error));
    }
  };

  struct To_String_Visitor {
    std::string operator()(const Value_Type&) { QLJS_UNREACHABLE(); }

    template <class Error>
    std::string operator()(const Error& error) {
      return error.to_string();
    }
  };

  template <class... NewErrors>
  struct Copy_Errors_Visitor {
    quick_lint_js::Result<void, NewErrors...> operator()(const Value_Type&) {
      return {};
    }

    template <class Error>
    quick_lint_js::Result<void, NewErrors...> operator()(const Error& error) {
      return quick_lint_js::Result<void, NewErrors...>(
          failed_result<const Error&>(error));
    }
  };

  Variant<Value_Type, Errors...> data_;

  template <class, class...>
  friend class Result_Base;
};

// Like variant<T, Errors...>, but more ergonomic.
//
// Errors must be unique. For example, Result<int, char, char> is illegal.
template <class T, class... Errors>
class Result : public Result_Base<T, Errors...> {
 private:
  using Base = Result_Base<T, Errors...>;

 public:
  using Base::Base;
  using Base::copy_errors;
  using Base::error;
  using Base::error_to_string;
  using Base::has_error;
  using Base::ok;
  using Base::operator*;
  using Base::operator->;
  using Base::operator=;
  using Base::propagate;
  using Base::value;

  template <class, class...>
  friend class Result_Base;
};

// Like variant<monostate, Errors...>, but more ergonomic.
//
// Errors must be unique. For example, Result<void, char, char> is illegal.
template <class... Errors>
class Result<void, Errors...> : public Result_Base<void, Errors...> {
 private:
  using Base = Result_Base<void, Errors...>;

 public:
  using Base::Base;
  using Base::copy_errors;
  using Base::error;
  using Base::error_to_string;
  using Base::has_error;
  using Base::ok;
  using Base::operator=;
  using Base::propagate;

  template <class, class...>
  friend class Result_Base;
};

// Like variant<T, Error>, but more ergonomic.
template <class T, class Error>
class Result<T, Error> : public Result_Base<T, Error> {
 private:
  using Base = Result_Base<T, Error>;

 public:
  using Base::Base;
  using Base::copy_errors;
  using Base::error_to_string;
  using Base::ok;
  using Base::operator*;
  using Base::operator->;
  using Base::operator=;
  using Base::propagate;
  using Base::value;

  const Error& error() const { return Base::template error<Error>(); }

  template <class, class...>
  friend class Result_Base;
};

// Like std::optional<Error>, but with a similar interface to Result<T, Error>.
template <class Error>
class Result<void, Error> : public Result_Base<void, Error> {
 private:
  using Base = Result_Base<void, Error>;

 public:
  using Base::Base;
  using Base::copy_errors;
  using Base::error_to_string;
  using Base::ok;
  using Base::operator=;
  using Base::propagate;

  const Error& error() const { return Base::template error<Error>(); }

  template <class, class...>
  friend class Result_Base;
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
