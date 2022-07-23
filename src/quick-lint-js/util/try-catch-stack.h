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
class try_catch_stack {
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
      catch_entry &c = this->catch_stack_.back();
      QLJS_ASSERT(c.exception.has_value());
      ResultType result = std::move(catch_func)(std::move(*c.exception));
      this->catch_stack_.pop_back();
      return result;
    }
  }

  // If this->try_raise(e) was called by t in this->try_catch(t, c), then this
  // function unwinds the stack and calls c(e).
  //
  // Otherwise, this function does nothing and returns. The caller is
  // responsible for figuring out what to do in this case.
  void try_raise(Exception &&e) {
    if (!this->catch_stack_.empty()) {
      catch_entry &c = this->catch_stack_.back();
      c.exception.emplace(std::move(e));
      std::longjmp(c.buf, 1);
      QLJS_UNREACHABLE();
    }
  }

 private:
  struct catch_entry {
    std::jmp_buf buf;
    std::optional<Exception> exception;
  };
  std::vector<catch_entry> catch_stack_;
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
