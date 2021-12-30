// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_EXPRESSION_INL_H
#define QUICK_LINT_JS_PARSE_EXPRESSION_INL_H

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/expression.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/warning.h>
#include <utility>

namespace quick_lint_js {
template <QLJS_PARSE_VISITOR Visitor>
void parser::visit_expression(expression *ast, Visitor &v,
                              parser::variable_context context) {
  auto visit_children = [&] {
    int child_count = ast->child_count();
    for (int i = 0; i < child_count; ++i) {
      this->visit_expression(ast->child(i), v, context);
    }
  };
  auto visit_parameters = [&]() {
    int parameter_count = ast->child_count();
    for (int i = 0; i < parameter_count; ++i) {
      expression *parameter = ast->child(i);
      this->visit_binding_element(parameter, v, variable_kind::_parameter,
                                  /*declaring_token=*/std::nullopt);
    }
  };
  switch (ast->kind()) {
  case expression_kind::_invalid:
  case expression_kind::_missing:
  case expression_kind::import:
  case expression_kind::literal:
  case expression_kind::new_target:
  case expression_kind::private_variable:
  case expression_kind::super:
  case expression_kind::yield_none:
    break;
  case expression_kind::_new:
  case expression_kind::_template:
  case expression_kind::array:
  case expression_kind::binary_operator:
  case expression_kind::call:
  case expression_kind::tagged_template_literal:
    visit_children();
    break;
  case expression_kind::trailing_comma: {
    auto &trailing_comma_ast = static_cast<expression::trailing_comma &>(*ast);
    this->error_reporter_->report(error_missing_operand_for_operator{
        .where = trailing_comma_ast.comma_span(),
    });
    visit_children();
    break;
  }
  case expression_kind::_class:
    v.visit_enter_class_scope();
    ast->visit_children(v, this->expressions_);
    v.visit_exit_class_scope();
    break;
  case expression_kind::arrow_function_with_expression: {
    auto *arrow =
        static_cast<expression::arrow_function_with_expression *>(ast);
    v.visit_enter_function_scope();
    visit_parameters();
    v.visit_enter_function_scope_body();
    this->visit_expression(arrow->body_, v, variable_context::rhs);
    v.visit_exit_function_scope();
    break;
  }
  case expression_kind::arrow_function_with_statements:
    v.visit_enter_function_scope();
    visit_parameters();
    v.visit_enter_function_scope_body();
    ast->visit_children(v, this->expressions_);
    v.visit_exit_function_scope();
    break;
  case expression_kind::assignment: {
    expression *lhs = ast->child_0();
    expression *rhs = ast->child_1();
    this->visit_assignment_expression(lhs, rhs, v);
    break;
  }
  case expression_kind::compound_assignment:
  case expression_kind::conditional_assignment: {
    expression *lhs = ast->child_0();
    expression *rhs = ast->child_1();
    this->visit_compound_or_conditional_assignment_expression(lhs, rhs, v);
    break;
  }
  case expression_kind::_typeof: {
    expression *child = ast->child_0()->without_paren();
    if (child->kind() == expression_kind::variable) {
      v.visit_variable_typeof_use(child->variable_identifier());
    } else {
      this->visit_expression(child, v, context);
    }
    break;
  }
  case expression_kind::_delete: {
    expression *child = ast->child_0();
    if (child->kind() == expression_kind::variable) {
      v.visit_variable_delete_use(
          child->variable_identifier(),
          static_cast<expression::_delete *>(ast)->unary_operator_span());
    } else {
      this->visit_expression(child, v, context);
    }
    break;
  }
  case expression_kind::await:
  case expression_kind::spread:
  case expression_kind::unary_operator:
  case expression_kind::yield_many:
  case expression_kind::yield_one:
    this->visit_expression(ast->child_0(), v, context);
    break;
  case expression_kind::conditional:
    this->visit_expression(ast->child_0(), v, context);
    this->visit_expression(ast->child_1(), v, context);
    this->visit_expression(ast->child_2(), v, context);
    break;
  case expression_kind::dot:
    this->visit_expression(ast->child_0(), v, variable_context::rhs);
    break;
  case expression_kind::index:
    this->visit_expression(ast->child_0(), v, variable_context::rhs);
    this->visit_expression(ast->child_1(), v, variable_context::rhs);
    break;
  case expression_kind::jsx_element: {
    auto *element = static_cast<expression::jsx_element *>(ast);
    if (!element->is_intrinsic()) {
      v.visit_variable_use(element->tag);
    }
    visit_children();
    break;
  }
  case expression_kind::jsx_element_with_members: {
    auto *element = static_cast<expression::jsx_element_with_members *>(ast);
    QLJS_ASSERT(element->members.size() >= 1);
    v.visit_variable_use(element->members[0]);
    visit_children();
    break;
  }
  case expression_kind::jsx_element_with_namespace:
    visit_children();
    break;
  case expression_kind::jsx_fragment:
    visit_children();
    break;
  case expression_kind::object:
    for (int i = 0; i < ast->object_entry_count(); ++i) {
      auto entry = ast->object_entry(i);
      if (entry.property.has_value()) {
        this->visit_expression(*entry.property, v, variable_context::rhs);
      }
      this->visit_expression(entry.value, v, context);
    }
    break;
  case expression_kind::paren:
    this->visit_expression(ast->child_0(), v, context);
    break;
  case expression_kind::rw_unary_prefix:
  case expression_kind::rw_unary_suffix: {
    expression *child = ast->child_0();
    this->visit_expression(child, v, variable_context::rhs);
    this->maybe_visit_assignment(child, v);
    break;
  }
  case expression_kind::variable:
    switch (context) {
    case variable_context::lhs:
      break;
    case variable_context::rhs:
      if (ast->variable_identifier_token_type() ==
          token_type::reserved_keyword_with_escape_sequence) {
        v.visit_keyword_variable_use(ast->variable_identifier());
      } else {
        v.visit_variable_use(ast->variable_identifier());
      }
      break;
    }
    break;
  case expression_kind::function:
    v.visit_enter_function_scope();
    ast->visit_children(v, this->expressions_);
    v.visit_exit_function_scope();
    break;
  case expression_kind::named_function:
    v.visit_enter_named_function_scope(ast->variable_identifier());
    ast->visit_children(v, this->expressions_);
    v.visit_exit_function_scope();
    break;
  }
}

template <QLJS_PARSE_VISITOR Visitor>
void parser::visit_assignment_expression(expression *lhs, expression *rhs,
                                         Visitor &v) {
  this->visit_expression(lhs, v, variable_context::lhs);
  this->visit_expression(rhs, v, variable_context::rhs);
  this->maybe_visit_assignment(lhs, v);
}

template <QLJS_PARSE_VISITOR Visitor>
void parser::visit_compound_or_conditional_assignment_expression(
    expression *lhs, expression *rhs, Visitor &v) {
  this->visit_expression(lhs, v, variable_context::rhs);
  this->visit_expression(rhs, v, variable_context::rhs);
  this->maybe_visit_assignment(lhs, v);
}

template <QLJS_PARSE_VISITOR Visitor>
void parser::maybe_visit_assignment(expression *ast, Visitor &v) {
  switch (ast->kind()) {
  case expression_kind::array:
    for (expression *child : ast->children()) {
      this->maybe_visit_assignment(child, v);
    }
    break;
  case expression_kind::object:
    for (int i = 0; i < ast->object_entry_count(); ++i) {
      expression *value = ast->object_entry(i).value;
      this->maybe_visit_assignment(value, v);
    }
    break;
  case expression_kind::paren:
    this->maybe_visit_assignment(ast->child_0(), v);
    break;
  case expression_kind::variable:
    v.visit_variable_assignment(ast->variable_identifier());
    break;
  default:
    break;
  }
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
