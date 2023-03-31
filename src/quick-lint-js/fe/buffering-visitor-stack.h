// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_BUFFERING_VISITOR_STACK_H
#define QUICK_LINT_JS_FE_BUFFERING_VISITOR_STACK_H

#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/port/memory-resource.h>

namespace quick_lint_js {
class stacked_buffering_visitor;

// A stack of buffering_visitor objects.
//
// Use this class instead of allocating buffering_visitor instances as local
// function variables so that the buffering_visitor instances can be deallocated
// in case setjmp is called.
class buffering_visitor_stack {
 public:
  explicit buffering_visitor_stack()
      : buffering_visitor_stack(new_delete_resource()) {}

  explicit buffering_visitor_stack(memory_resource *memory) : stack_(memory) {}

  [[nodiscard]] stacked_buffering_visitor push();

 private:
  void pop(buffering_visitor *v) {
    for (;;) {
      if (this->stack_.empty()) {
        QLJS_ASSERT(false);
        return;
      }
      if (&this->stack_.back() == v) {
        this->stack_.pop_back();
        return;
      }
      // Someone else pushed to the stack but didn't pop. This is probably due
      // to stack unwinding. Pop on their behalf.
      this->stack_.pop_back();
    }
  }

  linked_vector<buffering_visitor> stack_;

  friend class stacked_buffering_visitor;
};

class stacked_buffering_visitor {
 public:
  stacked_buffering_visitor(const stacked_buffering_visitor &) = delete;
  stacked_buffering_visitor &operator=(const stacked_buffering_visitor &) =
      delete;

  ~stacked_buffering_visitor() { this->stack_->pop(this->visitor_); }

  buffering_visitor &visitor() { return *this->visitor_; }

 private:
  explicit stacked_buffering_visitor(buffering_visitor_stack *stack,
                                     buffering_visitor *visitor)
      : stack_(stack), visitor_(visitor) {}

  buffering_visitor_stack *stack_;
  buffering_visitor *visitor_;

  friend class buffering_visitor_stack;
};

inline stacked_buffering_visitor buffering_visitor_stack::push() {
  this->stack_.emplace_back(new_delete_resource());
  return stacked_buffering_visitor(this, &this->stack_.back());
}
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
