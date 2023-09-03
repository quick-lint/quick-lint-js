// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <new>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/math.h>
#include <quick-lint-js/port/unreachable.h>
#include <string>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
template <class T, class Error>
class Result;

// Do not use result_propagation directly. Call Result<>::propagate instead.
template <class T, class Error>
struct Result_Propagation {
  Result<T, Error>& to_propagate;
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

// Do not use Result<Result_Monostate, ...> directly. Use Result<void, ...>
// instead.
struct Result_Monostate {};

// Like std::variant<T, Error> or std::expected<T, Error>, but more ergonomic.
template <class T, class Error>
class Result {
 private:
  inline static constexpr bool has_void_value_type = std::is_same_v<T, void>;

  using Stored_Value_Type =
      std::conditional_t<has_void_value_type, Result_Monostate, T>;

  using Value_Type_Ref =
      std::conditional_t<has_void_value_type, void, Stored_Value_Type&>;
  using Value_Type_Ref_Ref =
      std::conditional_t<has_void_value_type, void, Stored_Value_Type&&>;
  using Value_Type_Const_Ref =
      std::conditional_t<has_void_value_type, void, const Stored_Value_Type&>;

 public:
  // TODO(strager): Make explicit iff T(Args...) is explicit.
  template <class... Args,
            class = decltype(Stored_Value_Type(std::declval<Args>()...))>
  /*implicit*/ Result(Args&&... args) {
    new (this->value_storage()) Stored_Value_Type(std::forward<Args>(args)...);
    this->is_ok_ = true;
  }

  template <class E>
  /*implicit*/ Result(Result_Error<E>&& error) {
    new (this->error_storage()) Error(std::forward<E>(error.e));
    this->is_ok_ = false;
  }

  // Private constructor used by propagate. Do not call directly.
  template <class U, class Other_E>
  /*implicit*/ Result(Result_Propagation<U, Other_E>&& propagation) {
    new (this->error_storage())
        Error(std::move(propagation.to_propagate).error());
    this->is_ok_ = false;
  }

  // TODO(strager): Allow copying.
  Result(const Result&) = delete;
  Result& operator=(const Result&) = delete;

  Result(Result&& other) { this->move_construct_from(std::move(other)); }

  Result& operator=(Result&& other) {
    if (this->ok() && other.ok()) {
      *this->value_storage() = std::move(*other.value_storage());
    } else if (!this->ok() && !other.ok()) {
      *this->error_storage() = std::move(*other.error_storage());
    } else {
      this->destroy();
      this->move_construct_from(std::move(other));
    }
    return *this;
  }

  ~Result() { this->destroy(); }

  // FIXME(strager): The following code is a landmine:
  //
  //   r = failed_result(std::move(r.error()));
  template <class E>
  Result& operator=(Result_Error<E>&& error) {
    this->destroy();

    new (this->error_storage()) Error(std::forward<E>(error.e));
    this->is_ok_ = false;
    return *this;
  }

  bool ok() const { return this->is_ok_; }

  Value_Type_Ref value() & {
    QLJS_ASSERT(this->ok());
    return *this->value_storage();
  }

  Value_Type_Ref_Ref value() && {
    QLJS_ASSERT(this->ok());
    return std::move(*this->value_storage());
  }

  Value_Type_Const_Ref value() const& {
    QLJS_ASSERT(this->ok());
    return *this->value_storage();
  }

  const Error& error() const& {
    QLJS_ASSERT(!this->ok());
    return *this->error_storage();
  }

  Error&& error() && {
    QLJS_ASSERT(!this->ok());
    return std::move(*this->error_storage());
  }

  std::string error_to_string() const {
    QLJS_ASSERT(!this->ok());
    return this->error_storage()->to_string();
  }

  Result_Propagation<T, Error> propagate() & {
    QLJS_ASSERT(!this->ok());
    return Result_Propagation<T, Error>{*this};
  }

  Result_Propagation<T, Error> propagate() && = delete;

  Value_Type_Ref operator*() & { return this->value(); }
  Value_Type_Ref_Ref operator*() && { return std::move(*this).value(); }
  Value_Type_Const_Ref operator*() const& { return this->value(); }

  T* operator->() { return &this->value(); }
  const T* operator->() const { return &this->value(); }

  friend bool operator==(const Result& lhs, const Result& rhs) {
    if (lhs.ok() != rhs.ok()) {
      return false;
    }
    if (lhs.ok()) {
      return lhs.value() == rhs.value();
    } else {
      return lhs.error() == rhs.error();
    }
  }

  friend bool operator!=(const Result& lhs, const Result& rhs) {
    return !(lhs == rhs);
  }

 private:
  Stored_Value_Type* value_storage() {
    return reinterpret_cast<Stored_Value_Type*>(this->storage_);
  }
  const Stored_Value_Type* value_storage() const {
    return reinterpret_cast<const Stored_Value_Type*>(this->storage_);
  }

  Error* error_storage() { return reinterpret_cast<Error*>(this->storage_); }
  const Error* error_storage() const {
    return reinterpret_cast<const Error*>(this->storage_);
  }

  template <class Other_Result>
  void move_construct_from(Other_Result&& other) {
    if (other.ok()) {
      new (this->value_storage())
          Stored_Value_Type(std::move(*other.value_storage()));
      this->is_ok_ = true;
    } else {
      new (this->error_storage()) Error(std::move(*other.error_storage()));
      this->is_ok_ = false;
    }
  }

  void destroy() {
    if (this->ok()) {
      this->value_storage()->~Stored_Value_Type();
    } else {
      this->error_storage()->~Error();
    }
  }

  bool is_ok_;
  // If is_ok_ == true, then storage_ holds an object of type T or
  // Result_Monostate.
  // If is_ok_ == false, then storage_ holds an object of type Error.
  alignas(
      maximum(alignof(Stored_Value_Type),
              alignof(Error))) char storage_[maximum(sizeof(Stored_Value_Type),
                                                     sizeof(Error))];
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
