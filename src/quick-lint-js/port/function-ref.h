// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/port/warning.h>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
template <class Func_Type>
class Async_Function_Ref;
template <class Func_Type>
class Temporary_Function_Ref;

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wcast-qual")
QLJS_WARNING_IGNORE_CLANG("-Wold-style-cast")
QLJS_WARNING_IGNORE_GCC("-Wconditionally-supported")
QLJS_WARNING_IGNORE_GCC("-Wold-style-cast")

// Temporary_Function_Ref is like std::function_ref
// (https://www.open-std.org/JTC1/SC22/WG21/docs/papers/2022/p0792r10.html).
//
// Deviations from the specification:
// * Our Temporary_Function_Ref does not allow construction from lvalue
//   functors.
//
// Temporary_Function_Ref is designed to work with C++ lifetime extension.
// Temporary_Function_Ref is useful for a std::for_each parameter, but not for a
// job queue routine.
template <class Result, class... Args>
class Temporary_Function_Ref<Result(Args...)> {
 public:
  // Construct an Temporary_Function_Ref which will call a function pointer (no
  // closure).
  //
  // This overload is disabled for incoming lvalue references. 'func' must be an
  // rvalue.
  template <class Func>
  /*implicit*/ Temporary_Function_Ref(
      Func&& func,
      std::enable_if_t<std::is_convertible_v<Func, Result (*)(Args...)> &&
                       !std::is_lvalue_reference_v<Func>>* = nullptr)
      : callback_(callback<Result(Args...)>),
        closure_(reinterpret_cast<const void*>(
            static_cast<Result (*)(Args...)>(func))) {}

  // Construct an Temporary_Function_Ref which will call a functor.
  //
  // This overload is disabled for incoming lvalue references. 'func' must be an
  // rvalue.
  //
  // This overload is disabled for function pointers and for functors which can
  // be converted into a function pointer.
  template <class Func>
  /*implicit*/ Temporary_Function_Ref(
      Func&& func,
      std::enable_if_t<std::is_invocable_r_v<Result, Func, Args...> &&
                       !std::is_lvalue_reference_v<Func> &&
                       !std::is_convertible_v<Func, Result (*)(Args...)>>* =
          nullptr)
      : callback_(callback<std::remove_reference_t<Func>>), closure_(&func) {}

  Temporary_Function_Ref(Temporary_Function_Ref& func) = default;
  Temporary_Function_Ref(const Temporary_Function_Ref& func) = default;
  Temporary_Function_Ref(Temporary_Function_Ref&& func) = default;

  Result operator()(Args... args) {
    return this->callback_(std::forward<Args>(args)..., this->closure_);
  }

 private:
  template <class Func>
  static Result callback(Args... args, const void* closure) {
    return (*(const Func*)closure)(std::forward<Args>(args)...);
  }

  Result (*callback_)(Args..., const void*);
  const void* closure_;
};

// Async_Function_Ref is like std::function_ref
// (https://www.open-std.org/JTC1/SC22/WG21/docs/papers/2022/p0792r10.html).
//
// Deviations from the specification:
// * Our Async_Function_Ref does not allow construction from rvalue functors.
//
// Async_Function_Ref is designed for asynchronous or deferred calling.
// Async_Function_Ref is useful for a job queue routine, but not for a
// std::for_each parameter.
template <class Result, class... Args>
class Async_Function_Ref<Result(Args...)> {
 public:
  // Construct an Async_Function_Ref which will call a function pointer (no
  // closure).
  template <class Func>
  /*implicit*/ Async_Function_Ref(
      Func&& func,
      std::enable_if_t<std::is_convertible_v<Func, Result (*)(Args...)>>* =
          nullptr)
      : callback_(callback<Result(Args...)>),
        closure_(reinterpret_cast<const void*>(
            static_cast<Result (*)(Args...)>(func))) {}

  // Construct an Async_Function_Ref which will call a functor.
  //
  // This overload is disabled for incoming rvalue references. 'func' must be an
  // lvalue reference.
  //
  // This overload is disabled for function pointers and for functors which can
  // be converted into a function pointer.
  template <class Func>
  /*implicit*/ Async_Function_Ref(
      Func&& func,
      std::enable_if_t<std::is_invocable_r_v<Result, Func, Args...> &&
                       std::is_lvalue_reference_v<Func> &&
                       !std::is_convertible_v<Func, Result (*)(Args...)>>* =
          nullptr)
      : callback_(callback<std::remove_reference_t<Func>>), closure_(&func) {}

  Async_Function_Ref(Async_Function_Ref& func) = default;
  Async_Function_Ref(const Async_Function_Ref& func) = default;
  Async_Function_Ref(Async_Function_Ref&& func) = default;

  Async_Function_Ref& operator=(const Async_Function_Ref&) = default;
  Async_Function_Ref& operator=(Async_Function_Ref&&) = default;

  Result operator()(Args... args) {
    return this->callback_(std::forward<Args>(args)..., this->closure_);
  }

 private:
  template <class Func>
  static Result callback(Args... args, const void* closure) {
    return (*(const Func*)closure)(std::forward<Args>(args)...);
  }

  Result (*callback_)(Args..., const void*);
  const void* closure_;
};

QLJS_WARNING_POP
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
