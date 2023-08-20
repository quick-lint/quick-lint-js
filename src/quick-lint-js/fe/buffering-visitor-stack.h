// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/port/memory-resource.h>

namespace quick_lint_js {
class Stacked_Buffering_Visitor;

// A stack of buffering_visitor objects.
//
// Use this class instead of allocating buffering_visitor instances as local
// function variables so that the buffering_visitor instances can be deallocated
// in case setjmp is called.
class Buffering_Visitor_Stack {
 public:
  explicit Buffering_Visitor_Stack()
      : Buffering_Visitor_Stack(new_delete_resource()) {}

  explicit Buffering_Visitor_Stack(Memory_Resource *memory) : stack_(memory) {}

  [[nodiscard]] Stacked_Buffering_Visitor push();

 private:
  void pop(Buffering_Visitor *v) {
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

  Linked_Vector<Buffering_Visitor> stack_;

  friend class Stacked_Buffering_Visitor;
};

class Stacked_Buffering_Visitor {
 public:
  Stacked_Buffering_Visitor(const Stacked_Buffering_Visitor &) = delete;
  Stacked_Buffering_Visitor &operator=(const Stacked_Buffering_Visitor &) =
      delete;

  ~Stacked_Buffering_Visitor() { this->stack_->pop(this->visitor_); }

  Buffering_Visitor &visitor() { return *this->visitor_; }

 private:
  explicit Stacked_Buffering_Visitor(Buffering_Visitor_Stack *stack,
                                     Buffering_Visitor *visitor)
      : stack_(stack), visitor_(visitor) {}

  Buffering_Visitor_Stack *stack_;
  Buffering_Visitor *visitor_;

  friend class Buffering_Visitor_Stack;
};

inline Stacked_Buffering_Visitor Buffering_Visitor_Stack::push() {
  this->stack_.emplace_back(new_delete_resource());
  return Stacked_Buffering_Visitor(this, &this->stack_.back());
}
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
