// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_NULL_VISITOR_H
#define QUICK_LINT_JS_NULL_VISITOR_H

#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse-visitor.h>

namespace quick_lint_js {
class null_visitor final : public parse_visitor_base {
 public:
  void visit_end_of_module() override {}
  void visit_enter_block_scope() override {}
  void visit_enter_with_scope() override {}
  void visit_enter_class_scope() override {}
  void visit_enter_for_scope() override {}
  void visit_enter_function_scope() override {}
  void visit_enter_function_scope_body() override {}
  void visit_enter_named_function_scope(identifier) override {}
  void visit_exit_block_scope() override {}
  void visit_exit_with_scope() override {}
  void visit_exit_class_scope() override {}
  void visit_exit_for_scope() override {}
  void visit_exit_function_scope() override {}
  void visit_keyword_variable_use(identifier) override {}
  void visit_property_declaration(std::optional<identifier>) override {}
  void visit_variable_assignment(identifier) override {}
  void visit_variable_declaration(identifier, variable_kind,
                                  variable_init_kind) override {}
  void visit_variable_delete_use(identifier, source_code_span) override {}
  void visit_variable_export_use(identifier) override {}
  void visit_variable_typeof_use(identifier) override {}
  void visit_variable_use(identifier) override {}
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
