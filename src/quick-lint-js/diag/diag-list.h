// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/port/memory-resource.h>

namespace quick_lint_js {
// Internally, Diag_List is implemented as a singly linked list.
class Diag_List {
 public:
  // Private to Diag_List. Do not use.
  //
  // Node_Base is split from Node to improve compile times. The union inside
  // Node compiles slowly.
  //
  // Node_Base needs to be in the header for an efficient implementation of
  // for_each.
  struct Node_Base {
    Node_Base *next;
    Diag_Type type;

    // Returns &static_cast<Node*>(this)->data.
    void *get_data() { return &this[1]; }
  };
  // Private to Diag_List. Do not use.
  struct Node;

  struct Rewind_State {
    // Private to Diag_List. Do not use.
    Node_Base *first_;
    Node_Base *last_;
  };

  explicit Diag_List(Memory_Resource *);

  Diag_List(Diag_List &&) = delete;
  Diag_List &operator=(Diag_List &&) = delete;

  ~Diag_List();

#define QLJS_DIAG_TYPE_NAME(name) void add(name diag);
  QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

  Rewind_State prepare_for_rewind();

  void rewind(Rewind_State &&r);

  // Func must have the following signature:
  //
  //   void(Diag_Type type, const void* diag);
  template <class Func>
  void for_each(Func &&callback) const {
    for (Node_Base *node = this->first_; node != nullptr; node = node->next) {
      callback(node->type, node->get_data());
    }
  }

  bool empty() const;
  bool reported_any_diagnostic_except_since(
      std::initializer_list<Diag_Type> ignored_types, const Rewind_State &);

  void clear();

 private:
  template <std::size_t Diag_Size>
  void add_impl(Diag_Type type, void *diag);

  Memory_Resource *memory_;
  Node_Base *first_ = nullptr;
  Node_Base *last_ = nullptr;
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
