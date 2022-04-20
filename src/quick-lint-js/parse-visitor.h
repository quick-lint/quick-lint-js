// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_VISITOR_H
#define QUICK_LINT_JS_PARSE_VISITOR_H

#include <optional>
#include <quick-lint-js/have.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <type_traits>

namespace quick_lint_js {
// TOdO(strager): Rename this class to parse_visitor.
class parse_visitor_base {
 public:
  parse_visitor_base() noexcept = default;

  parse_visitor_base(const parse_visitor_base &) noexcept = default;
  parse_visitor_base &operator=(const parse_visitor_base &) noexcept = default;

  parse_visitor_base(parse_visitor_base &&) noexcept = default;
  parse_visitor_base &operator=(parse_visitor_base &&) noexcept = default;

  virtual ~parse_visitor_base() = default;

  virtual void visit_enter_block_scope() = 0;
  virtual void visit_enter_with_scope() = 0;
  virtual void visit_enter_class_scope() = 0;
  virtual void visit_enter_for_scope() = 0;
  virtual void visit_enter_function_scope() = 0;
  virtual void visit_enter_function_scope_body() = 0;
  virtual void visit_enter_named_function_scope(identifier) = 0;
  virtual void visit_exit_block_scope() = 0;
  virtual void visit_exit_with_scope() = 0;
  virtual void visit_exit_class_scope() = 0;
  virtual void visit_exit_for_scope() = 0;
  virtual void visit_exit_function_scope() = 0;
  virtual void visit_keyword_variable_use(identifier name) = 0;
  virtual void visit_property_declaration(std::optional<identifier>) = 0;
  virtual void visit_variable_declaration(identifier name, variable_kind kind,
                                          variable_init_kind init_kind) = 0;
  virtual void visit_variable_assignment(identifier name) = 0;
  virtual void visit_variable_delete_use(identifier name,
                                         source_code_span delete_keyword) = 0;
  virtual void visit_variable_export_use(identifier name) = 0;
  virtual void visit_variable_typeof_use(identifier name) = 0;
  virtual void visit_variable_use(identifier name) = 0;
  virtual void visit_end_of_module() = 0;
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
