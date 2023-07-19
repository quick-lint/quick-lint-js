// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_TRY_CATCH_STACK_H
#define QUICK_LINT_JS_UTIL_TRY_CATCH_STACK_H

#include <csetjmp>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <utility>
#include <vector>

namespace quick_lint_js {
// Implements a limited form of exceptions using setjmp/longjmp.
//
// * Might or might not call destructors of automatic variables.
template <class Exception>
class Try_Catch_Stack {
 public:
  // Calls try_func().
  //
  // If try_func() calls this->raise(e), calls catch_func(e) and returns its
  // result.
  //
  // If try_func() does not call this->raise(), returns the result of
  // try_func().
  template <class ResultType, class TryFunc, class CatchFunc>
  ResultType try_catch(TryFunc &&try_func, CatchFunc &&catch_func) {
    this->catch_stack_.emplace_back();
    if (setjmp(this->catch_stack_.back().buf) == 0) {
      ResultType result = std::move(try_func)();
      // this->raise() was not called.
      this->catch_stack_.pop_back();
      return result;
    } else {
      // this->raise() was called.
      Catch_Entry &c = this->catch_stack_.back();
      QLJS_ASSERT(c.exception.has_value());
      Exception exception = std::move(*c.exception);
      this->catch_stack_.pop_back();
      ResultType result = std::move(catch_func)(std::move(exception));
      return result;
    }
  }

  template <class TryFunc, class FinallyFunc>
  void try_finally(TryFunc &&try_func, FinallyFunc &&finally_func) {
    if (this->catch_stack_.empty()) {
      // Because the catch stack is empty, a call to this->raise_if_have_handler
      // should return. If we called this->try_catch here, then a call to
      // this->raise_if_have_handler would not return. Therefore, avoid calling
      // this->try_catch.
      std::move(try_func)();
      std::move(finally_func)();
    } else {
      bool try_finished = false;
      // HACK(strager): Dummy int is because try_catch does not support void.
      this->try_catch<int>(
          [&]() -> int {
            std::move(try_func)();
            try_finished = true;
            std::move(finally_func)();
            return 0;
          },
          [&](Exception &&e) -> int {
            QLJS_ASSERT(!try_finished &&
                        "finally_func should not call raise_if_have_handler");
            std::move(finally_func)();
            QLJS_ASSERT(!this->catch_stack_.empty());
            this->raise_if_have_handler(std::move(e));
            // We had a handler, so raise_if_have_handler should raise and not
            // return.
            QLJS_UNREACHABLE();
            return 0;
          });
    }
  }

  // If this->raise_if_have_handler(e) was called by t in this->try_catch(t, c),
  // then this function unwinds the stack and calls c(e).
  //
  // Otherwise, this function does nothing and returns. The caller is
  // responsible for figuring out what to do in this case.
  void raise_if_have_handler(Exception &&e) {
    if (!this->catch_stack_.empty()) {
      Catch_Entry &c = this->catch_stack_.back();
      c.exception.emplace(std::move(e));
      std::longjmp(c.buf, 1);
      QLJS_UNREACHABLE();
    }
  }

 private:
  struct Catch_Entry {
    std::jmp_buf buf;
    std::optional<Exception> exception;
  };
  std::vector<Catch_Entry> catch_stack_;
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
