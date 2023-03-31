// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_HEAP_FUNCTION_H
#define QUICK_LINT_JS_CONTAINER_HEAP_FUNCTION_H

#include <type_traits>
#include <utility>

namespace quick_lint_js {
template <class FuncType>
class heap_function;

// heap_function is like std::function but with a few differences:
//
// * heap_function compiles significantly faster than libc++'s std::function.
// * heap_function supports move-only targets.
// * heap_function does not have inline storage. It always heap-allocates.
// * heap_function does not support allocators.
// * heap_function does not support member function pointers and other
//   craziness.
template <class Result, class... Args>
class heap_function<Result(Args...)> {
 public:
  /*implicit*/ heap_function() = default;

  template <class Func, class = typename std::enable_if<
                            std::is_invocable_r_v<Result, Func, Args...>>::type>
  /*implicit*/ heap_function(Func&& func)
      : callable_(new dynamic_callable<Func>(std::move(func))) {}

  heap_function(heap_function&& other) noexcept
      : callable_(std::exchange(other.callable_, nullptr)) {}

  heap_function& operator=(heap_function&& other) noexcept {
    if (this != &other) {
      delete this->callable_;
      this->callable_ = std::exchange(other.callable_, nullptr);
    }
    return *this;
  }

  ~heap_function() { delete this->callable_; }

  explicit operator bool() const noexcept { return this->callable_ != nullptr; }

  Result operator()(Args... args) {
    return this->callable_->call(std::forward<Args>(args)...);
  }

 private:
  struct dynamic_callable_base {
    virtual ~dynamic_callable_base() = default;

    virtual Result call(Args... args) = 0;
  };

  template <class Func>
  struct dynamic_callable : dynamic_callable_base {
    explicit dynamic_callable(Func&& func) : func(std::move(func)) {}

    virtual ~dynamic_callable() override = default;

    Result call(Args... args) override {
      return this->func(std::forward<Args>(args)...);
    }

    Func func;
  };

  dynamic_callable_base* callable_ = nullptr;
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
