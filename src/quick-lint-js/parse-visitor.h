// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_PARSE_VISITOR_H
#define QUICK_LINT_JS_PARSE_VISITOR_H

#include <quick-lint-js/have.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>

// For portability, use QLJS_PARSE_VISITOR in template parameter lists instead
// of using parse_visitor directly:
//
//   template <QLJS_PARSE_VISITOR Visitor>
//   void visit_thing(Visitor &v) { ... }
#if QLJS_HAVE_CXX_CONCEPTS
#define QLJS_PARSE_VISITOR ::quick_lint_js::parse_visitor
#else
#define QLJS_PARSE_VISITOR class
#endif

namespace quick_lint_js {
#if QLJS_HAVE_CXX_CONCEPTS
template <class Visitor>
concept parse_visitor = requires(Visitor v, identifier name,
                                 variable_kind var_kind) {
  {v.visit_end_of_module()};
  {v.visit_enter_block_scope()};
  {v.visit_enter_class_scope()};
  {v.visit_enter_for_scope()};
  {v.visit_enter_function_scope()};
  {v.visit_enter_function_scope_body()};
  {v.visit_enter_named_function_scope(name)};
  {v.visit_exit_block_scope()};
  {v.visit_exit_class_scope()};
  {v.visit_exit_for_scope()};
  {v.visit_exit_function_scope()};
  {v.visit_property_declaration(name)};
  {v.visit_variable_assignment(name)};
  {v.visit_variable_declaration(name, var_kind)};
  {v.visit_variable_typeof_use(name)};
  {v.visit_variable_use(name)};
};
#endif
}

#endif
