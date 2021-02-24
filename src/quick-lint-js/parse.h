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

#ifndef QUICK_LINT_JS_PARSE_H
#define QUICK_LINT_JS_PARSE_H

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/expression.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/null-visitor.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/warning.h>
#include <utility>

#define QLJS_CASE_BINARY_ONLY_OPERATOR      \
  case token_type::kw_instanceof:           \
  case token_type::ampersand:               \
  case token_type::ampersand_ampersand:     \
  case token_type::bang_equal:              \
  case token_type::bang_equal_equal:        \
  case token_type::circumflex:              \
  case token_type::equal_equal:             \
  case token_type::equal_equal_equal:       \
  case token_type::greater:                 \
  case token_type::greater_equal:           \
  case token_type::greater_greater:         \
  case token_type::greater_greater_greater: \
  case token_type::less:                    \
  case token_type::less_equal:              \
  case token_type::less_less:               \
  case token_type::percent:                 \
  case token_type::pipe:                    \
  case token_type::pipe_pipe:               \
  case token_type::star:                    \
  case token_type::star_star

#define QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR    \
  case token_type::ampersand_equal:               \
  case token_type::circumflex_equal:              \
  case token_type::greater_greater_equal:         \
  case token_type::greater_greater_greater_equal: \
  case token_type::less_less_equal:               \
  case token_type::minus_equal:                   \
  case token_type::percent_equal:                 \
  case token_type::pipe_equal:                    \
  case token_type::plus_equal:                    \
  case token_type::slash_equal:                   \
  case token_type::star_equal:                    \
  case token_type::star_star_equal

#define QLJS_PARSER_UNIMPLEMENTED()                                   \
  do {                                                                \
    this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__); \
  } while (false)

#define QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(expected_token_type) \
  do {                                                              \
    if (this->peek().type != (expected_token_type)) {               \
      QLJS_PARSER_UNIMPLEMENTED();                                  \
    }                                                               \
  } while (false)

namespace quick_lint_js {
// A parser reads JavaScript source code and calls the member functions of a
// parse_visitor (visit_variable_declaration, visit_enter_function_scope, etc.).
class parser {
 private:
  class function_guard;
  class loop_guard;
  class switch_guard;

 public:
  explicit parser(padded_string_view input, error_reporter *error_reporter)
      : lexer_(input, error_reporter), error_reporter_(error_reporter) {}

  quick_lint_js::lexer &lexer() noexcept { return this->lexer_; }

  // For testing only.
  quick_lint_js::expression_arena &expression_arena() noexcept {
    return this->expressions_;
  }

  // For testing and internal use only.
  [[nodiscard]] function_guard enter_function(function_attributes);
  [[nodiscard]] loop_guard enter_loop();

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_module(Visitor &v) {
    while (this->peek().type != token_type::end_of_file) {
      this->parse_and_visit_statement(v);
    }
    v.visit_end_of_module();
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_statement(Visitor &v) {
    auto parse_expression_end = [this]() -> void {
      while (this->peek().type == token_type::right_paren) {
        this->error_reporter_->report(error_unmatched_parenthesis{
            .where = this->peek().span(),
        });
        this->skip();
      }
      this->consume_semicolon();
    };

  parse_statement:
    switch (this->peek().type) {
    // export class C {}
    // export {taco} from "taco-stand";
    case token_type::kw_export:
      this->parse_and_visit_export(v);
      break;

    case token_type::semicolon:
      this->skip();
      break;

    // function f() {}
    case token_type::kw_function:
      this->parse_and_visit_function_declaration(
          v, function_attributes::normal,
          /*begin=*/this->peek().begin,
          /*require_name=*/
          name_requirement::required_for_statement);
      break;

    // var x = 42;
    case token_type::kw_const:
    case token_type::kw_var:
      this->parse_and_visit_variable_declaration_statement(v);
      break;

    // let x = 42;
    // let();
    // let: while (true) {}
    case token_type::kw_let: {
      token let_token = this->peek();
      lexer_transaction transaction = this->lexer_.begin_transaction();
      this->skip();
      if (this->peek().type == token_type::colon) {
        // Labelled statement.
        this->lexer_.commit_transaction(std::move(transaction));
        this->skip();
        goto parse_statement;
      } else if (this->is_let_token_a_variable_reference(this->peek().type)) {
        // Expression.
        this->lexer_.roll_back_transaction(std::move(transaction));
        expression *ast =
            this->parse_expression(precedence{.in_operator = true});
        this->visit_expression(ast, v, variable_context::rhs);
      } else {
        // Variable declaration.
        this->lexer_.commit_transaction(std::move(transaction));
        this->parse_and_visit_let_bindings(v, let_token,
                                           /*allow_in_operator=*/true);
        this->consume_semicolon();
      }
      break;
    }

    // async function f() {}
    // async = 42;
    case token_type::kw_async: {
      token async_token = this->peek();
      this->skip();
      switch (this->peek().type) {
      // async function f() {}
      case token_type::kw_function:
        this->parse_and_visit_function_declaration(
            v, function_attributes::async,
            /*begin=*/async_token.begin,
            /*require_name=*/
            name_requirement::required_for_statement);
        break;

      // async (x, y) => expressionOrStatement
      // async x => expressionOrStatement
      // async => expressionOrStatement
      // async += 42;
      QLJS_CASE_BINARY_ONLY_OPERATOR:
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      case token_type::comma:
      case token_type::complete_template:
      case token_type::dot:
      case token_type::equal:
      case token_type::equal_greater:
      case token_type::identifier:
      case token_type::incomplete_template:
      case token_type::kw_as:
      case token_type::kw_async:
      case token_type::kw_await:
      case token_type::kw_from:
      case token_type::kw_get:
      case token_type::kw_in:
      case token_type::kw_let:
      case token_type::kw_of:
      case token_type::kw_set:
      case token_type::kw_static:
      case token_type::kw_yield:
      case token_type::left_paren:
      case token_type::minus:
      case token_type::minus_minus:
      case token_type::plus:
      case token_type::plus_plus:
      case token_type::question:
      case token_type::semicolon:
      case token_type::slash: {
        expression *ast =
            this->parse_async_expression(async_token, precedence{});
        this->visit_expression(ast, v, variable_context::rhs);
        break;
      }

      // Labelled statement.
      case token_type::colon:
        this->skip();
        goto parse_statement;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // import {bananas} from "Thailand";
    // import(url).then(loaded);
    case token_type::kw_import:
      this->parse_and_visit_import(v);
      break;

    // this.explode();
    // [1, 2, 3].forEach(x => console.log(x));
    // ^ x  // invalid expression
    QLJS_CASE_BINARY_ONLY_OPERATOR:
    case token_type::bang:
    case token_type::complete_template:
    case token_type::dot:
    case token_type::equal:
    case token_type::incomplete_template:
    case token_type::kw_as:
    case token_type::kw_delete:
    case token_type::kw_false:
    case token_type::kw_from:
    case token_type::kw_get:
    case token_type::kw_new:
    case token_type::kw_null:
    case token_type::kw_of:
    case token_type::kw_set:
    case token_type::kw_static:
    case token_type::kw_super:
    case token_type::kw_this:
    case token_type::kw_true:
    case token_type::kw_typeof:
    case token_type::kw_void:
    case token_type::left_paren:
    case token_type::left_square:
    case token_type::minus:
    case token_type::minus_minus:
    case token_type::number:
    case token_type::plus:
    case token_type::plus_plus:
    case token_type::right_paren:
    case token_type::slash:
    case token_type::slash_equal:
    case token_type::string:
    case token_type::tilde:
      this->parse_and_visit_expression(v);
      parse_expression_end();
      break;

    // await settings.save();
    // await = value;
    // await: for(;;);
    case token_type::kw_await:
      if (this->in_async_function_) {
        this->parse_and_visit_expression(v);
        parse_expression_end();
        break;
      } else {
        goto parse_loop_label_or_expression_starting_with_identifier;
      }

    // yield value;
    // yield = value;
    // yield: for(;;);
    case token_type::kw_yield:
      if (this->in_generator_function_) {
        this->parse_and_visit_expression(v);
        parse_expression_end();
        break;
      } else {
        goto parse_loop_label_or_expression_starting_with_identifier;
      }

    // console.log("hello");
    // label: for(;;);
    parse_loop_label_or_expression_starting_with_identifier:
    case token_type::identifier: {
      identifier ident = this->peek().identifier_name();
      this->skip();
      switch (this->peek().type) {
      // Labelled statement.
      case token_type::colon:
        this->skip();
        goto parse_statement;

      // Expression statement.
      default:
        expression *ast = this->make_expression<expression::variable>(
            ident, token_type::identifier);
        ast = this->parse_expression_remainder(ast, precedence{});
        this->visit_expression(ast, v, variable_context::rhs);
        parse_expression_end();
        break;
      }
      break;
    }

    // class C {}
    case token_type::kw_class:
      this->parse_and_visit_class(
          v,
          /*require_name=*/name_requirement::required_for_statement);
      break;

    // switch (x) { default: ; }
    case token_type::kw_switch: {
      switch_guard s(this, std::exchange(this->in_switch_statement_, true));
      this->parse_and_visit_switch(v);
      break;
    }

    // return;
    // return 42;
    case token_type::kw_return:
      this->skip();
      switch (this->peek().type) {
      case token_type::semicolon:
        this->skip();
        break;

      case token_type::right_curly:
        break;

      default:
        this->parse_and_visit_expression(v);
        parse_expression_end();
        break;
      }
      break;

    // throw fit;
    case token_type::kw_throw:
      this->skip();
      if (this->peek().type == token_type::semicolon) {
        this->error_reporter_->report(
            error_expected_expression_before_semicolon{this->peek().span()});
        this->skip();
        break;
      }
      if (this->peek().has_leading_newline) {
        this->lexer_.insert_semicolon();
        this->error_reporter_->report(
            error_expected_expression_before_newline{this->peek().span()});
        this->skip();
        break;
      }
      this->parse_and_visit_expression(v);
      parse_expression_end();
      break;

    // try { hard(); } catch (exhaustion) {}
    case token_type::kw_try:
      this->parse_and_visit_try(v);
      break;

    // do { } while (can);
    case token_type::kw_do: {
      loop_guard guard = this->enter_loop();
      this->parse_and_visit_do_while(v);
      break;
    }

    // for (let i = 0; i < length; ++i) {}
    // for (let x of xs) {}
    case token_type::kw_for: {
      loop_guard guard = this->enter_loop();
      this->parse_and_visit_for(v);
      break;
    }

    // while (cond) {}
    case token_type::kw_while: {
      loop_guard guard = this->enter_loop();
      this->parse_and_visit_while(v);
      break;
    }

    // with (o) { eek(); }
    case token_type::kw_with:
      this->parse_and_visit_with(v);
      break;

    // if (cond) { yay; } else { nay; }
    case token_type::kw_if:
      this->parse_and_visit_if(v);
      break;

    // else { nay; } // Invalid.
    case token_type::kw_else:
      this->error_reporter_->report(error_else_has_no_if{
          .else_token = this->peek().span(),
      });
      this->skip();
      this->parse_and_visit_statement(v);
      break;

    // break;
    // continue label;
    case token_type::kw_break:
    case token_type::kw_continue: {
      bool is_break = this->peek().type == token_type::kw_break;
      source_code_span token_span = this->peek().span();
      this->skip();
      switch (this->peek().type) {
      // TODO(strager): Are contextual keywords allowed as labels?
      case token_type::identifier:
        // Loop label.
        this->skip();
        break;
      default:
        if (is_break) {
          if (!(this->in_switch_statement_ || this->in_loop_statement_)) {
            this->error_reporter_->report(error_invalid_break{token_span});
          }
        } else {
          if (!this->in_loop_statement_) {
            this->error_reporter_->report(error_invalid_continue{token_span});
          }
        }
        break;
      }
      this->consume_semicolon();
      break;
    }

    // debugger;
    case token_type::kw_debugger:
      this->skip();
      this->consume_semicolon();
      break;

    // { statement; statement; }
    case token_type::left_curly:
      v.visit_enter_block_scope();
      this->parse_and_visit_statement_block_no_scope(v);
      v.visit_exit_block_scope();
      break;

    case token_type::right_curly:
      // TODO(strager): Should we report an error? The caller expected a
      // statement.
      break;

    case token_type::colon:
      this->error_reporter_->report(error_unexpected_token{
          .token = this->peek().span(),
      });
      this->skip();
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_expression(Visitor &v) {
    this->parse_and_visit_expression(v, precedence{});
  }

  expression *parse_expression() {
    return this->parse_expression(precedence{});
  }

 private:
  enum class variable_context {
    lhs,
    rhs,
  };

  template <QLJS_PARSE_VISITOR Visitor>
  void visit_expression(expression *ast, Visitor &v, variable_context context) {
    auto visit_children = [&] {
      int child_count = ast->child_count();
      for (int i = 0; i < child_count; ++i) {
        this->visit_expression(ast->child(i), v, context);
      }
    };
    auto visit_parameters = [&](int parameter_count) {
      for (int i = 0; i < parameter_count; ++i) {
        expression *parameter = ast->child(i);
        this->visit_binding_element(parameter, v, variable_kind::_parameter);
      }
    };
    switch (ast->kind()) {
    case expression_kind::_invalid:
    case expression_kind::import:
    case expression_kind::literal:
    case expression_kind::new_target:
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
      auto &trailing_comma_ast =
          static_cast<expression::trailing_comma &>(*ast);
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
      v.visit_enter_function_scope();
      int body_child_index = ast->child_count() - 1;
      visit_parameters(body_child_index);
      v.visit_enter_function_scope_body();
      this->visit_expression(ast->child(body_child_index), v,
                             variable_context::rhs);
      v.visit_exit_function_scope();
      break;
    }
    case expression_kind::arrow_function_with_statements:
      v.visit_enter_function_scope();
      visit_parameters(ast->child_count());
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
    case expression_kind::compound_assignment: {
      expression *lhs = ast->child_0();
      expression *rhs = ast->child_1();
      this->visit_compound_assignment_expression(lhs, rhs, v);
      break;
    }
    case expression_kind::_typeof: {
      expression *child = ast->child_0();
      if (child->kind() == expression_kind::variable) {
        v.visit_variable_typeof_use(child->variable_identifier());
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
    case expression_kind::object:
      for (int i = 0; i < ast->object_entry_count(); ++i) {
        auto entry = ast->object_entry(i);
        if (entry.property.has_value()) {
          this->visit_expression(*entry.property, v, variable_context::rhs);
        }
        this->visit_expression(entry.value, v, context);
      }
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
        v.visit_variable_use(ast->variable_identifier());
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
  void visit_assignment_expression(expression *lhs, expression *rhs,
                                   Visitor &v) {
    this->visit_expression(lhs, v, variable_context::lhs);
    this->visit_expression(rhs, v, variable_context::rhs);
    this->maybe_visit_assignment(lhs, v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void visit_compound_assignment_expression(expression *lhs, expression *rhs,
                                            Visitor &v) {
    this->visit_expression(lhs, v, variable_context::rhs);
    this->visit_expression(rhs, v, variable_context::rhs);
    this->maybe_visit_assignment(lhs, v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void maybe_visit_assignment(expression *ast, Visitor &v) {
    switch (ast->kind()) {
    case expression_kind::object:
      for (int i = 0; i < ast->object_entry_count(); ++i) {
        expression *value = ast->object_entry(i).value;
        this->maybe_visit_assignment(value, v);
      }
      break;
    case expression_kind::variable:
      v.visit_variable_assignment(ast->variable_identifier());
      break;
    default:
      break;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_export(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_export);
    this->skip();
    switch (this->peek().type) {
    // export default class C {}
    case token_type::kw_default:
      this->skip();
      switch (this->peek().type) {
      // export default async function f() {}
      // export default async () => {}
      case token_type::kw_async: {
        token async_token = this->peek();
        this->skip();
        if (this->peek().type == token_type::kw_function) {
          this->parse_and_visit_function_declaration(
              v, function_attributes::async,
              /*begin=*/async_token.begin,
              /*require_name=*/name_requirement::optional);
        } else {
          expression *ast =
              this->parse_async_expression(async_token, precedence{});
          this->visit_expression(ast, v, variable_context::rhs);
          this->consume_semicolon();
        }
        break;
      }

      // export default class C {}
      case token_type::kw_class:
        this->parse_and_visit_class(
            v,
            /*require_name=*/name_requirement::optional);
        break;

      // export default function f() {}
      case token_type::kw_function:
        this->parse_and_visit_function_declaration(
            v, function_attributes::normal,
            /*begin=*/this->peek().begin,
            /*require_name=*/name_requirement::optional);
        break;

      // export default let x = null;  // Invalid.
      // export default let;           // Invalid.
      case token_type::kw_const:
      case token_type::kw_let:
      case token_type::kw_var: {
        token declaring_token = this->peek();
        this->skip();
        this->error_reporter_->report(error_cannot_export_default_variable{
            .declaring_token = declaring_token.span(),
        });
        this->parse_and_visit_let_bindings(v, declaring_token,
                                           /*allow_in_operator=*/true);
        break;
      }

      // export default 2 + 2;
      default:
        this->parse_and_visit_expression(v);
        this->consume_semicolon();
        break;
      }
      break;

    // export * from "module";
    // export * as name from "module";
    case token_type::star:
      this->skip();
      if (this->peek().type == token_type::kw_as) {
        this->skip();
        switch (this->peek().type) {
        QLJS_CASE_KEYWORD:
        case token_type::identifier:
          this->skip();
          break;
        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
      }
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::kw_from);
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::string);
      this->skip();
      this->consume_semicolon();
      break;

    // export {a as default, b};
    // export {a, b, c} from "module";
    case token_type::left_curly: {
      buffering_visitor exports_visitor;
      this->parse_and_visit_named_exports(exports_visitor, /*is_export=*/true);
      if (this->peek().type == token_type::kw_from) {
        this->skip();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::string);
        this->skip();
      } else {
        exports_visitor.move_into(v);
      }
      this->consume_semicolon();
      break;
    }

    // export async function f() {}
    case token_type::kw_async: {
      const char8 *async_token_begin = this->peek().begin;
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::kw_function);
      this->parse_and_visit_function_declaration(
          v, function_attributes::async,
          /*begin=*/async_token_begin,
          /*require_name=*/name_requirement::required_for_export);
      break;
    }

    // export function f() {}
    case token_type::kw_function:
      this->parse_and_visit_function_declaration(
          v, function_attributes::normal,
          /*begin=*/this->peek().begin,
          /*require_name=*/name_requirement::required_for_export);
      break;

    // export class C {}
    case token_type::kw_class:
      this->parse_and_visit_class(
          v, /*require_name=*/name_requirement::required_for_export);
      break;

    // export let x = 42;
    case token_type::kw_const:
    case token_type::kw_let:
    case token_type::kw_var:
      this->parse_and_visit_variable_declaration_statement(v);
      break;

    // export stuff;    // Invalid.
    // export a, b, c;  // Invalid.
    // export 2 + 2;    // Invalid.
    case token_type::identifier:
    case token_type::number: {
      expression *ast = this->parse_expression();
      switch (ast->kind()) {
      case expression_kind::variable:
        this->error_reporter_->report(error_exporting_requires_curlies{
            .names = ast->span(),
        });
        break;
      default:
        this->error_reporter_->report(error_exporting_requires_default{
            .expression = ast->span(),
        });
        break;
      }
      this->visit_expression(ast, v, variable_context::rhs);
      this->consume_semicolon();
      break;
    }

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_statement_block_no_scope(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::left_curly);
    this->skip();
    for (;;) {
      this->parse_and_visit_statement(v);
      if (this->peek().type == token_type::right_curly) {
        this->skip();
        break;
      }
      if (this->peek().type == token_type::end_of_file) {
        QLJS_PARSER_UNIMPLEMENTED();
      }
    }
  }

  enum class name_requirement {
    optional,
    required_for_export,
    required_for_statement,
  };

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_declaration(Visitor &v,
                                            function_attributes attributes,
                                            const char8 *begin,
                                            name_requirement require_name) {
    QLJS_ASSERT(this->peek().type == token_type::kw_function);
    source_code_span function_token_span = this->peek().span();
    const char8 *function_token_begin = function_token_span.begin();
    this->skip();
    attributes = this->parse_generator_star(attributes);

    switch (this->peek().type) {
    case token_type::kw_await:
      if (this->in_async_function_) {
        this->error_reporter_->report(
            error_cannot_declare_await_in_async_function{
                .name = this->peek().identifier_name(),
            });
      }
      goto named_function;

    case token_type::kw_yield:
      if (this->in_generator_function_) {
        this->error_reporter_->report(
            error_cannot_declare_yield_in_generator_function{
                .name = this->peek().identifier_name(),
            });
      }
      goto named_function;

    named_function:
    case token_type::identifier:
    case token_type::kw_as:
    case token_type::kw_async:
    case token_type::kw_from:
    case token_type::kw_get:
    case token_type::kw_let:
    case token_type::kw_of:
    case token_type::kw_set:
    case token_type::kw_static:
      v.visit_variable_declaration(this->peek().identifier_name(),
                                   variable_kind::_function);
      this->skip();

      this->parse_and_visit_function_parameters_and_body(v, attributes);
      break;

    // export default function() {}
    case token_type::left_paren:
      switch (require_name) {
      case name_requirement::required_for_statement: {
        const char8 *left_paren_end = this->peek().end;

        // The function should have a name, but doesn't have a name. Perhaps the
        // user intended to include parentheses. Parse the function as an
        // expression instead of as a declaration.
        buffering_visitor *function_visitor =
            this->expressions_.make_buffering_visitor();
        this->parse_and_visit_function_parameters_and_body_no_scope(
            *function_visitor, attributes);
        const char8 *function_end = this->lexer_.end_of_previous_token();
        expression *function = this->make_expression<expression::function>(
            attributes, function_visitor,
            source_code_span(function_token_begin, function_end));
        expression *full_expression =
            this->parse_expression_remainder(function, precedence{});
        this->visit_expression(full_expression, v, variable_context::rhs);

        if (full_expression == function) {
          this->error_reporter_->report(
              error_missing_name_in_function_statement{
                  .where =
                      source_code_span(function_token_begin, left_paren_end),
              });
        } else {
          this->error_reporter_->report(
              error_missing_name_or_parentheses_for_function{
                  .where =
                      source_code_span(function_token_begin, left_paren_end),
                  .function = source_code_span(begin, function->span().end()),
              });
        }
        break;
      }

      case name_requirement::required_for_export: {
        this->error_reporter_->report(error_missing_name_of_exported_function{
            .function_keyword = function_token_span,
        });
        this->parse_and_visit_function_parameters_and_body(v, attributes);
        break;
      }

      case name_requirement::optional:
        this->parse_and_visit_function_parameters_and_body(v, attributes);
        break;
      }
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_parameters_and_body(
      Visitor &v, function_attributes attributes) {
    v.visit_enter_function_scope();
    this->parse_and_visit_function_parameters_and_body_no_scope(v, attributes);
    v.visit_exit_function_scope();
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_parameters_and_body_no_scope(
      Visitor &v, function_attributes attributes) {
    function_guard guard = this->enter_function(attributes);

    if (this->peek().type != token_type::left_paren) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();

    std::optional<source_code_span> last_parameter_spread_span = std::nullopt;
    bool first_parameter = true;
    for (;;) {
      std::optional<source_code_span> comma_span = std::nullopt;
      if (!first_parameter) {
        if (this->peek().type != token_type::comma) {
          break;
        }
        comma_span = this->peek().span();
        this->skip();
      }

      switch (this->peek().type) {
      case token_type::kw_await:
        // TODO(strager): Disallow parameters named 'await' for async functions.
        [[fallthrough]];
      case token_type::dot_dot_dot:
      case token_type::identifier:
      case token_type::kw_as:
      case token_type::kw_async:
      case token_type::kw_from:
      case token_type::kw_get:
      case token_type::kw_let:
      case token_type::kw_of:
      case token_type::kw_set:
      case token_type::kw_static:
      case token_type::kw_yield:
      case token_type::left_curly:
      case token_type::left_square: {
        expression *parameter = this->parse_expression(
            precedence{.commas = false, .in_operator = true});
        this->visit_binding_element(parameter, v, variable_kind::_parameter);
        if (parameter->kind() == expression_kind::spread) {
          last_parameter_spread_span = parameter->span();
        } else {
          last_parameter_spread_span = std::nullopt;
        }
        break;
      }
      case token_type::right_paren:
        if (last_parameter_spread_span.has_value()) {
          // function f(...args,)  // Trailing comma is illegal.
          QLJS_ASSERT(comma_span.has_value());
          this->error_reporter_->report(
              error_comma_not_allowed_after_spread_parameter{
                  .comma = *comma_span,
                  .spread = *last_parameter_spread_span,
              });
        }
        goto done;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      first_parameter = false;
    }
  done:

    if (this->peek().type != token_type::right_paren) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();

    v.visit_enter_function_scope_body();

    this->parse_and_visit_statement_block_no_scope(v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class(Visitor &v, name_requirement require_name) {
    QLJS_ASSERT(this->peek().type == token_type::kw_class);

    this->parse_and_visit_class_heading(v, /*require_name=*/require_name);

    v.visit_enter_class_scope();

    switch (this->peek().type) {
    case token_type::left_curly:
      this->skip();
      this->parse_and_visit_class_body(v);

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
      this->skip();
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    v.visit_exit_class_scope();
  }

  // Parse the 'class' keyword, the class's optional name, and any extends
  // clause.
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class_heading(Visitor &v,
                                     name_requirement require_name) {
    QLJS_ASSERT(this->peek().type == token_type::kw_class);
    source_code_span class_keyword_span = this->peek().span();
    this->skip();

    std::optional<identifier> optional_class_name;
    switch (this->peek().type) {
    case token_type::kw_let:
      this->error_reporter_->report(error_cannot_declare_class_named_let{
          .name = this->peek().identifier_name().span()});
      [[fallthrough]];
    case token_type::identifier:
    case token_type::kw_as:
    case token_type::kw_async:
    case token_type::kw_await:
    case token_type::kw_from:
    case token_type::kw_get:
    case token_type::kw_of:
    case token_type::kw_set:
    case token_type::kw_static:
    case token_type::kw_yield:
      optional_class_name = this->peek().identifier_name();
      this->skip();
      break;

    // class { ... }
    case token_type::left_curly:
      break;

    // class extends C { }
    case token_type::kw_extends:
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    switch (this->peek().type) {
    case token_type::kw_extends:
      this->skip();
      // TODO(strager): Error when extending things like '0' or 'true'.
      this->parse_and_visit_expression(v, precedence{.commas = false});
      break;

    case token_type::left_curly:
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    if (optional_class_name.has_value()) {
      v.visit_variable_declaration(*optional_class_name, variable_kind::_class);
    } else {
      switch (require_name) {
      case name_requirement::optional:
        break;
      case name_requirement::required_for_export:
        this->error_reporter_->report(error_missing_name_of_exported_class{
            .class_keyword = class_keyword_span,
        });
        break;
      case name_requirement::required_for_statement:
        this->error_reporter_->report(error_missing_name_in_class_statement{
            .class_keyword = class_keyword_span,
        });
        break;
      }
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class_body(Visitor &v) {
    while (this->peek().type != token_type::right_curly) {
      this->parse_and_visit_class_member(v);
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class_member(Visitor &v) {
    std::optional<identifier> last_ident;
    function_attributes method_attributes = function_attributes::normal;

  next:
    switch (this->peek().type) {
    // async f() {}
    case token_type::kw_async:
      last_ident = this->peek().identifier_name();
      this->skip();
      if (this->peek().type != token_type::left_paren) {
        method_attributes = function_attributes::async;
      }
      goto next;

    // static f() {}
    case token_type::kw_static:
      last_ident = this->peek().identifier_name();
      this->skip();
      goto next;

    // *g() {}
    case token_type::star:
      method_attributes = function_attributes::generator;
      this->skip();
      goto next;

    // get prop() {}
    case token_type::kw_get:
    case token_type::kw_set:
      last_ident = this->peek().identifier_name();
      this->skip();
      goto next;

    // method() {}
    case token_type::identifier:
    case token_type::kw_as:
    case token_type::kw_await:
    case token_type::kw_break:
    case token_type::kw_case:
    case token_type::kw_catch:
    case token_type::kw_class:
    case token_type::kw_const:
    case token_type::kw_continue:
    case token_type::kw_debugger:
    case token_type::kw_default:
    case token_type::kw_delete:
    case token_type::kw_do:
    case token_type::kw_else:
    case token_type::kw_export:
    case token_type::kw_extends:
    case token_type::kw_false:
    case token_type::kw_finally:
    case token_type::kw_for:
    case token_type::kw_from:
    case token_type::kw_if:
    case token_type::kw_import:
    case token_type::kw_in:
    case token_type::kw_instanceof:
    case token_type::kw_let:
    case token_type::kw_new:
    case token_type::kw_null:
    case token_type::kw_of:
    case token_type::kw_return:
    case token_type::kw_super:
    case token_type::kw_switch:
    case token_type::kw_this:
    case token_type::kw_throw:
    case token_type::kw_true:
    case token_type::kw_try:
    case token_type::kw_typeof:
    case token_type::kw_var:
    case token_type::kw_void:
    case token_type::kw_while:
    case token_type::kw_with:
    case token_type::kw_yield:
      v.visit_property_declaration(this->peek().identifier_name());
      this->skip();
      this->parse_and_visit_function_parameters_and_body(v, method_attributes);
      break;

    // "method"() {}
    // 9001() {}
    case token_type::number:
    case token_type::string:
      v.visit_property_declaration();
      this->skip();
      this->parse_and_visit_function_parameters_and_body(v, method_attributes);
      break;

    // [methodNameExpression]() {}
    case token_type::left_square:
      this->skip();

      this->parse_and_visit_expression(v);

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
      this->skip();

      v.visit_property_declaration();
      this->parse_and_visit_function_parameters_and_body(v, method_attributes);
      break;

    // async() {}
    // get() {}
    case token_type::left_paren:
      if (last_ident.has_value()) {
        v.visit_property_declaration(*last_ident);
        this->parse_and_visit_function_parameters_and_body(v,
                                                           method_attributes);
      } else {
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;

    // function() {}
    // function f() {}  // Invalid.
    case token_type::kw_function: {
      token function_token = this->peek();
      this->skip();
      if (this->peek().type == token_type::left_paren) {
        // function() {}
        v.visit_property_declaration(function_token.identifier_name());
        this->parse_and_visit_function_parameters_and_body(v,
                                                           method_attributes);
        break;
      } else {
        // function f() {}  // Invalid.
        this->error_reporter_->report(
            error_methods_should_not_use_function_keyword{
                .function_token = function_token.span(),
            });
        goto next;
      }
    }

    case token_type::semicolon:
      this->skip();
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_switch(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_switch);
    this->skip();

    this->parse_and_visit_parenthesized_expression<
        error_expected_parentheses_around_switch_condition,
        error_expected_parenthesis_around_switch_condition>(v);

    v.visit_enter_block_scope();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_curly);
    this->skip();

    bool keep_going = true;
    while (keep_going) {
      switch (this->peek().type) {
      case token_type::right_curly:
        this->skip();
        keep_going = false;
        break;
      case token_type::kw_case:
        this->skip();
        this->parse_and_visit_expression(v);
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
        this->skip();
        break;
      case token_type::kw_default:
        this->skip();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
        this->skip();
        break;
      default:
        this->parse_and_visit_statement(v);
        break;
      }
    }

    v.visit_exit_block_scope();
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_try(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_try);
    this->skip();

    v.visit_enter_block_scope();
    this->parse_and_visit_statement_block_no_scope(v);
    v.visit_exit_block_scope();

    if (this->peek().type == token_type::kw_catch) {
      this->skip();

      v.visit_enter_block_scope();
      if (this->peek().type == token_type::left_paren) {
        this->skip();

        switch (this->peek().type) {
        case token_type::kw_await:
          if (this->in_async_function_) {
            this->error_reporter_->report(
                error_cannot_declare_await_in_async_function{
                    .name = this->peek().identifier_name(),
                });
          }
          goto catch_identifier;

        case token_type::kw_yield:
          if (this->in_generator_function_) {
            this->error_reporter_->report(
                error_cannot_declare_yield_in_generator_function{
                    .name = this->peek().identifier_name(),
                });
          }
          goto catch_identifier;

        catch_identifier:
        case token_type::identifier:
        case token_type::kw_as:
        case token_type::kw_async:
        case token_type::kw_from:
        case token_type::kw_get:
        case token_type::kw_let:
        case token_type::kw_of:
        case token_type::kw_set:
        case token_type::kw_static:
          v.visit_variable_declaration(this->peek().identifier_name(),
                                       variable_kind::_catch);
          this->skip();
          break;

        case token_type::left_curly:
        case token_type::left_square:
          this->parse_and_visit_binding_element(v, variable_kind::_catch,
                                                /*allow_in_operator=*/false);
          break;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
        }

        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
        this->skip();
      }
      this->parse_and_visit_statement_block_no_scope(v);
      v.visit_exit_block_scope();
    }
    if (this->peek().type == token_type::kw_finally) {
      this->skip();

      v.visit_enter_block_scope();
      this->parse_and_visit_statement_block_no_scope(v);
      v.visit_exit_block_scope();
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_do_while(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_do);
    this->skip();

    this->parse_and_visit_statement(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::kw_while);
    this->skip();

    this->parse_and_visit_parenthesized_expression<
        error_expected_parentheses_around_do_while_condition,
        error_expected_parenthesis_around_do_while_condition>(v);

    if (this->peek().type == token_type::semicolon) {
      this->skip();
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_for(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_for);
    const char8 *for_token_begin = this->peek().begin;
    this->skip();

    if (this->peek().type == token_type::kw_await) {
      this->skip();
    }

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
    this->skip();

    std::optional<expression *> after_expression;
    auto parse_c_style_head_remainder =
        [&](source_code_span first_semicolon_span) {
          if (this->peek().type != token_type::semicolon) {
            this->parse_and_visit_expression(v);
          }

          if (this->peek().type == token_type::semicolon) {
            this->skip();
            if (this->peek().type != token_type::right_paren) {
              after_expression = this->parse_expression();
            }
          } else {
            this->error_reporter_->report(
                error_c_style_for_loop_is_missing_third_component{
                    .expected_last_component = this->peek().span(),
                    .existing_semicolon = first_semicolon_span,
                });
          }
        };

    bool entered_for_scope = false;

    QLJS_WARNING_PUSH
    QLJS_WARNING_IGNORE_GCC("-Wshadow-local")
    auto parse_in_or_of_or_condition_update =
        [&, this](auto &v, expression *init_expression) -> void {
      QLJS_WARNING_POP
      switch (this->peek().type) {
      case token_type::semicolon: {
        source_code_span first_semicolon_span = this->peek().span();
        this->skip();
        this->visit_expression(init_expression, v, variable_context::rhs);
        parse_c_style_head_remainder(first_semicolon_span);
        break;
      }
      case token_type::kw_in:
      case token_type::kw_of: {
        this->skip();
        expression *rhs = this->parse_expression();
        this->visit_assignment_expression(init_expression, rhs, v);
        break;
      }
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    };
    switch (this->peek().type) {
    // for (;;) {}
    case token_type::semicolon: {
      source_code_span first_semicolon_span = this->peek().span();
      this->skip();
      parse_c_style_head_remainder(first_semicolon_span);
      break;
    }

    // for (let i = 0; i < length; ++length) {}
    // for (let x of xs) {}
    // for (let in xs) {}
    case token_type::kw_const:
    case token_type::kw_let:
      v.visit_enter_for_scope();
      entered_for_scope = true;
      [[fallthrough]];
    case token_type::kw_var: {
      token declaring_token = this->peek();

      lexer_transaction transaction = this->lexer_.begin_transaction();
      this->skip();
      buffering_visitor lhs;
      if (declaring_token.type == token_type::kw_let &&
          this->is_let_token_a_variable_reference(this->peek().type)) {
        // for (let = expression; cond; up) {}
        // for (let(); cond; up) {}
        // for (let; cond; up) {}
        // for (let in myArray) {}
        this->lexer_.roll_back_transaction(std::move(transaction));
        expression *ast =
            this->parse_expression(precedence{.in_operator = false});
        this->visit_expression(ast, lhs, variable_context::lhs);
        this->maybe_visit_assignment(ast, lhs);
      } else if (declaring_token.type == token_type::kw_let &&
                 this->peek().type == token_type::kw_of) {
        // for (let of xs) {}  // Invalid.
        this->lexer_.commit_transaction(std::move(transaction));
        this->error_reporter_->report(error_let_with_no_bindings{
            .where = declaring_token.span(),
        });
      } else {
        // for (let i = 0; i < length; ++length) {}
        // for (let x of xs) {}
        this->lexer_.commit_transaction(std::move(transaction));
        this->parse_and_visit_let_bindings(lhs, declaring_token,
                                           /*allow_in_operator=*/false);
      }
      switch (this->peek().type) {
      // for (let i = 0; i < length; ++length) {}
      case token_type::semicolon: {
        source_code_span first_semicolon_span = this->peek().span();
        this->skip();
        lhs.move_into(v);
        parse_c_style_head_remainder(first_semicolon_span);
        break;
      }

      // for (let x of xs) {}
      case token_type::kw_in:
      case token_type::kw_of: {
        bool is_var_in = declaring_token.type == token_type::kw_var &&
                         this->peek().type == token_type::kw_in;
        this->skip();
        expression *rhs = this->parse_expression();
        if (is_var_in) {
          // In the following code, 'init' is evaluated before 'array':
          //
          //   for (var x = init in array) {}
          lhs.move_into(v);
        }
        this->visit_expression(rhs, v, variable_context::rhs);
        if (!is_var_in) {
          // In the following code, 'x' is declared before 'array' is evaluated:
          //
          //   for (let x in array) {}
          lhs.move_into(v);
        }
        break;
      }

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // for (async; condition; update) {}
    // for (async.prop; condition; update) {}
    // for (async in things) {}
    // for (async.prop of things) {}
    // for (async of => {}; condition; update) {}
    // for (async of things) {}  // Invalid.
    case token_type::kw_async: {
      token async_token = this->peek();

      lexer_transaction transaction = this->lexer_.begin_transaction();
      bool is_invalid_async_of_sequence = false;
      this->skip();
      if (this->peek().type == token_type::kw_of) {
        this->skip();
        if (this->peek().type != token_type::equal_greater) {
          is_invalid_async_of_sequence = true;
        }
      }
      this->lexer_.roll_back_transaction(std::move(transaction));

      expression *init_expression(nullptr);
      if (is_invalid_async_of_sequence) {
        // for (async of things) {}  // Invalid.
        this->error_reporter_->report(
            error_cannot_assign_to_variable_named_async_in_for_of_loop{
                .async_identifier = async_token.identifier_name(),
            });

        this->skip();
        QLJS_ASSERT(this->peek().type == token_type::kw_of);
        init_expression = this->make_expression<expression::variable>(
            async_token.identifier_name(), async_token.type);
      } else {
        init_expression =
            this->parse_expression(precedence{.in_operator = false});
      }
      parse_in_or_of_or_condition_update(v, init_expression);
      break;
    }

    // for (init; condition; update) {}
    // for (item of things) {}
    // for (item in things) {}
    default: {
      expression *init_expression =
          this->parse_expression(precedence{.in_operator = false});
      parse_in_or_of_or_condition_update(v, init_expression);
      break;
    }
    }

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    this->skip();

    switch (this->peek().type) {
    default:
      this->parse_and_visit_statement(v);
      break;

    case token_type::end_of_file:
    case token_type::right_curly:
      this->error_reporter_->report(error_missing_body_for_for_statement{
          .for_and_header = source_code_span(
              for_token_begin, this->lexer_.end_of_previous_token()),
      });
      break;
    }

    if (after_expression.has_value()) {
      this->visit_expression(*after_expression, v, variable_context::rhs);
    }
    if (entered_for_scope) {
      v.visit_exit_for_scope();
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_while(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_while);
    this->skip();

    this->parse_and_visit_parenthesized_expression<
        error_expected_parentheses_around_while_condition,
        error_expected_parenthesis_around_while_condition>(v);

    this->parse_and_visit_statement(v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_with(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_with);
    this->skip();

    this->parse_and_visit_parenthesized_expression<
        error_expected_parentheses_around_with_expression,
        error_expected_parenthesis_around_with_expression>(v);

    this->parse_and_visit_statement(v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_if(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_if);
    const char8 *if_token_begin = this->peek().begin;
    this->skip();

    this->parse_and_visit_parenthesized_expression<
        error_expected_parentheses_around_if_condition,
        error_expected_parenthesis_around_if_condition>(v);

    switch (this->peek().type) {
    default:
      this->parse_and_visit_statement(v);
      break;

    case token_type::kw_else:
    case token_type::right_curly:
      this->error_reporter_->report(error_missing_body_for_if_statement{
          .if_and_condition = source_code_span(
              if_token_begin, this->lexer_.end_of_previous_token()),
      });
      break;
    }

    if (this->peek().type == token_type::kw_else) {
      this->skip();
      this->parse_and_visit_statement(v);
    }
  }

  template <class ExpectedParenthesesError, class ExpectedParenthesisError,
            QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_parenthesized_expression(Visitor &v) {
    bool have_expression_left_paren =
        this->peek().type == token_type::left_paren;
    if (have_expression_left_paren) {
      this->skip();
    }
    const char8 *expression_begin = this->peek().begin;

    this->parse_and_visit_expression(v);

    const char8 *expression_end = this->lexer_.end_of_previous_token();
    bool have_expression_right_paren =
        this->peek().type == token_type::right_paren;
    if (have_expression_right_paren) {
      this->skip();
    }

    if (!have_expression_left_paren && !have_expression_right_paren) {
      this->error_reporter_->report(ExpectedParenthesesError{
          source_code_span(expression_begin, expression_end)});
    } else if (!have_expression_right_paren) {
      this->error_reporter_->report(ExpectedParenthesisError{
          .where = source_code_span(expression_end, expression_end),
          .token = ')',
      });
    } else if (!have_expression_left_paren) {
      this->error_reporter_->report(ExpectedParenthesisError{
          .where = source_code_span(expression_begin, expression_begin),
          .token = '(',
      });
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_import(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_import);
    source_code_span import_span = this->peek().span();
    this->skip();

    switch (this->peek().type) {
    // import let from "module";
    case token_type::kw_let:
      this->error_reporter_->report(
          error_cannot_import_let{.import_name = this->peek().span()});
      [[fallthrough]];
    // import fs from "fs";
    case token_type::identifier:
    case token_type::kw_async:
      v.visit_variable_declaration(this->peek().identifier_name(),
                                   variable_kind::_import);
      this->skip();
      if (this->peek().type == token_type::comma) {
        this->skip();
        switch (this->peek().type) {
        // import fs, {readFile} from "fs";
        case token_type::left_curly:
          parse_and_visit_named_exports(v, /*is_export=*/false);
          break;

        // import fs, * as fs2 from "fs";
        case token_type::star:
          this->parse_and_visit_name_space_import(v);
          break;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
      }
      break;

    // import {readFile} from "fs";
    case token_type::left_curly:
      parse_and_visit_named_exports(v, /*is_export=*/false);
      break;

    // import expression statement:
    //
    // import(url).then(() => { /* ... */ })
    case token_type::left_paren: {
      expression *ast = this->parse_expression_remainder(
          this->make_expression<expression::import>(import_span), precedence{});
      this->visit_expression(ast, v, variable_context::rhs);
      this->consume_semicolon();
      return;
    }

    // import * as fs from "fs";
    case token_type::star:
      this->parse_and_visit_name_space_import(v);
      break;

    // import "foo";
    case token_type::string:
      this->skip();
      this->consume_semicolon();
      return;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    if (this->peek().type != token_type::kw_from) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();

    if (this->peek().type != token_type::string) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();

    if (this->peek().type == token_type::semicolon) {
      this->skip();
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_name_space_import(Visitor &v) {
    this->skip();

    if (this->peek().type != token_type::kw_as) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();

    switch (this->peek().type) {
    case token_type::kw_let:
      this->error_reporter_->report(error_cannot_import_let{
          .import_name = this->peek().identifier_name().span()});
      [[fallthrough]];
    case token_type::identifier:
    case token_type::kw_async:
      v.visit_variable_declaration(this->peek().identifier_name(),
                                   variable_kind::_import);
      this->skip();
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_named_exports(Visitor &v, bool is_export) {
    QLJS_ASSERT(this->peek().type == token_type::left_curly);
    this->skip();
    for (;;) {
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case token_type::identifier: {
        // TODO(strager): Is 'import {default} ...' allowed?
        identifier left_name = this->peek().identifier_name();
        identifier right_name = left_name;
        token_type right_token_type = this->peek().type;
        this->skip();
        if (this->peek().type == token_type::kw_as) {
          this->skip();
          switch (this->peek().type) {
          QLJS_CASE_KEYWORD:
          case token_type::identifier:
            // TODO(strager): Is 'import {x as default} ...' allowed?
            right_name = this->peek().identifier_name();
            right_token_type = this->peek().type;
            this->skip();
            break;
          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
          }
        }
        if (is_export) {
          v.visit_variable_export_use(left_name);
        } else {
          if (right_token_type == token_type::kw_let) {
            // TODO(strager): What about other keywords?
            this->error_reporter_->report(
                error_cannot_import_let{.import_name = right_name.span()});
          }
          v.visit_variable_declaration(right_name, variable_kind::_import);
        }
        break;
      }
      case token_type::right_curly:
        goto done;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      if (this->peek().type == token_type::comma) {
        this->skip();
      }
    }
  done:
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
    this->skip();
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_variable_declaration_statement(Visitor &v) {
    token declaring_token = this->peek();
    QLJS_ASSERT(declaring_token.type == token_type::kw_const ||
                declaring_token.type == token_type::kw_let ||
                declaring_token.type == token_type::kw_var);
    this->skip();
    this->parse_and_visit_let_bindings(v, declaring_token,
                                       /*allow_in_operator=*/true);
    this->consume_semicolon();
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_let_bindings(Visitor &v, token declaring_token,
                                    bool allow_in_operator) {
    variable_kind declaration_kind;
    switch (declaring_token.type) {
    case token_type::kw_const:
      declaration_kind = variable_kind::_const;
      break;
    case token_type::kw_let:
      declaration_kind = variable_kind::_let;
      break;
    case token_type::kw_var:
      declaration_kind = variable_kind::_var;
      break;
    default:
      QLJS_ASSERT(false);
      declaration_kind = variable_kind::_let;
      break;
    }
    this->parse_and_visit_let_bindings(v, declaring_token, declaration_kind,
                                       /*allow_in_operator=*/allow_in_operator);
  }

  // declaring_token is the const/let/var token.
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_let_bindings(Visitor &v, token declaring_token,
                                    variable_kind declaration_kind,
                                    bool allow_in_operator) {
    source_code_span let_span = declaring_token.span();
    bool first_binding = true;
    for (;;) {
      std::optional<source_code_span> comma_span = std::nullopt;
      if (!first_binding) {
        if (this->peek().type != token_type::comma) {
          break;
        }
        comma_span = this->peek().span();
        this->skip();
      }

      switch (this->peek().type) {
      case token_type::kw_await:
        if (this->in_async_function_) {
          this->error_reporter_->report(
              error_cannot_declare_await_in_async_function{
                  .name = this->peek().identifier_name(),
              });
        }
        goto variable_name;

      case token_type::kw_yield:
        if (this->in_generator_function_) {
          this->error_reporter_->report(
              error_cannot_declare_yield_in_generator_function{
                  .name = this->peek().identifier_name(),
              });
        }
        goto variable_name;

        // let x;
        // let x = 42;
      variable_name:
      case token_type::identifier:
      case token_type::kw_as:
      case token_type::kw_from:
      case token_type::kw_get:
      case token_type::kw_let:
      case token_type::kw_of:
      case token_type::kw_set:
      case token_type::kw_static: {
        expression *variable = this->make_expression<expression::variable>(
            this->peek().identifier_name(), this->peek().type);
        this->skip();
        expression *ast = this->parse_expression_remainder(
            variable,
            precedence{.commas = false, .in_operator = allow_in_operator});
        this->visit_binding_element(ast, v, declaration_kind);
        break;
      }

      case token_type::kw_async:
      case token_type::left_curly:
      case token_type::left_square:
        this->parse_and_visit_binding_element(
            v, declaration_kind, /*allow_in_operator=*/allow_in_operator);
        break;
      case token_type::kw_if:
      case token_type::kw_break:
      case token_type::kw_continue:
      case token_type::kw_debugger:
      case token_type::kw_false:
      case token_type::kw_null:
      case token_type::kw_true:
      case token_type::kw_void:
      case token_type::number:
        this->error_reporter_->report(
            error_invalid_binding_in_let_statement{this->peek().span()});
        this->skip();
        break;

      QLJS_CASE_BINARY_ONLY_OPERATOR:
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      case token_type::comma:
      case token_type::complete_template:
      case token_type::dot:
      case token_type::equal:
      case token_type::equal_greater:
      case token_type::incomplete_template:
      case token_type::kw_in:
      case token_type::left_paren:
      case token_type::minus:
      case token_type::minus_minus:
      case token_type::plus:
      case token_type::plus_plus:
      case token_type::question:
      case token_type::semicolon:
      case token_type::slash:
        QLJS_PARSER_UNIMPLEMENTED();
        break;

      default:
        if (first_binding) {
          this->error_reporter_->report(error_let_with_no_bindings{let_span});
        } else {
          QLJS_ASSERT(comma_span.has_value());
          this->error_reporter_->report(
              error_stray_comma_in_let_statement{*comma_span});
        }
        break;
      }
      first_binding = false;
    }
  }

  bool is_let_token_a_variable_reference(token_type following_token) noexcept {
    switch (following_token) {
    QLJS_CASE_BINARY_ONLY_OPERATOR:
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
    case token_type::comma:
    case token_type::complete_template:
    case token_type::dot:
    case token_type::equal:
    case token_type::equal_greater:
    case token_type::incomplete_template:
    case token_type::kw_in:
    case token_type::left_paren:
    case token_type::minus:
    case token_type::minus_minus:
    case token_type::plus:
    case token_type::plus_plus:
    case token_type::question:
    case token_type::semicolon:
    case token_type::slash:
      return true;

    default:
      return false;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_binding_element(Visitor &v,
                                       variable_kind declaration_kind,
                                       bool allow_in_operator) {
    expression *ast = this->parse_expression(
        precedence{.commas = false, .in_operator = allow_in_operator});
    this->visit_binding_element(ast, v, declaration_kind);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void visit_binding_element(expression *ast, Visitor &v,
                             variable_kind declaration_kind) {
    switch (ast->kind()) {
    case expression_kind::array:
      for (int i = 0; i < ast->child_count(); ++i) {
        this->visit_binding_element(ast->child(i), v, declaration_kind);
      }
      break;
    case expression_kind::assignment:
      this->visit_expression(ast->child_1(), v, variable_context::rhs);
      this->visit_binding_element(ast->child_0(), v, declaration_kind);
      break;
    case expression_kind::variable: {
      identifier ident = ast->variable_identifier();
      if ((declaration_kind == variable_kind::_const ||
           declaration_kind == variable_kind::_import ||
           declaration_kind == variable_kind::_let) &&
          ast->variable_identifier_token_type() == token_type::kw_let) {
        // If this is an import, we would emit error_cannot_import_let instead.
        QLJS_ASSERT(declaration_kind != variable_kind::_import);
        this->error_reporter_->report(
            error_cannot_declare_variable_named_let_with_let{.name =
                                                                 ident.span()});
      }
      v.visit_variable_declaration(ident, declaration_kind);
      break;
    }
    case expression_kind::object:
      for (int i = 0; i < ast->object_entry_count(); ++i) {
        expression *value = ast->object_entry(i).value;
        this->visit_binding_element(value, v, declaration_kind);
      }
      break;
    case expression_kind::spread:
      this->visit_binding_element(ast->child_0(), v, declaration_kind);
      break;

    case expression_kind::await: {
      expression::await *await = expression_cast<expression::await>(ast);
      identifier ident(await->unary_operator_span());
      v.visit_variable_declaration(ident, declaration_kind);
      this->error_reporter_->report(
          error_cannot_declare_await_in_async_function{
              .name = ident,
          });
      break;
    }

    case expression_kind::yield_none: {
      identifier ident(ast->span());
      v.visit_variable_declaration(ident, declaration_kind);
      this->error_reporter_->report(
          error_cannot_declare_yield_in_generator_function{
              .name = ident,
          });
      break;
    }

    case expression_kind::_invalid:
      this->error_reporter_->report(error_invalid_binding_in_let_statement{
          .where = ast->span(),
      });
      break;

    default:
      QLJS_UNIMPLEMENTED();
      break;
    }
  }

  struct precedence {
    bool binary_operators = true;
    bool math_or_logical_or_assignment = true;
    bool commas = true;
    bool in_operator = true;
  };

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_expression(Visitor &v, precedence prec) {
    expression *ast = this->parse_expression(prec);
    this->visit_expression(ast, v, variable_context::rhs);
  }

  expression *parse_expression(precedence);

  expression *parse_async_expression(token async_token, precedence);
  expression *parse_async_expression_only(token async_token);

  expression *parse_expression_remainder(expression *, precedence);

  expression *parse_arrow_function_body(function_attributes,
                                        const char8 *parameter_list_begin);
  expression *parse_arrow_function_body(
      function_attributes, const char8 *parameter_list_begin,
      expression_arena::array_ptr<expression *> &&parameters);
  // Args is either of the following:
  // * expression_arena::array_ptr<expression*> &&parameters
  // * (none)
  template <class... Args>
  expression *parse_arrow_function_body_impl(function_attributes,
                                             const char8 *parameter_list_begin,
                                             Args &&... args);

  expression *parse_function_expression(function_attributes,
                                        const char8 *span_begin);

  expression *parse_object_literal();

  expression *parse_class_expression();

  expression *parse_template(std::optional<expression *> tag);

  function_attributes parse_generator_star(function_attributes);

  expression *maybe_wrap_erroneous_arrow_function(expression *arrow_function,
                                                  expression *lhs);

  void consume_semicolon();

  const token &peek() const noexcept { return this->lexer_.peek(); }
  void skip() noexcept { this->lexer_.skip(); }

  [[noreturn]] void crash_on_unimplemented_token(
      const char *qljs_file_name, int qljs_line,
      const char *qljs_function_name);

  template <class Expression, class... Args>
  expression *make_expression(Args &&... args) {
    return this->expressions_.make_expression<Expression>(
        std::forward<Args>(args)...);
  }

  class function_guard {
   public:
    explicit function_guard(parser *, bool was_in_async_function,
                            bool was_in_generator_function,
                            bool was_in_loop_statement,
                            bool was_in_switch_statement) noexcept;

    function_guard(const function_guard &) = delete;
    function_guard &operator=(const function_guard &) = delete;

    ~function_guard() noexcept;

   private:
    parser *parser_;
    bool was_in_async_function_;
    bool was_in_generator_function_;
    bool was_in_loop_statement_;
    bool was_in_switch_statement_;
  };

  class loop_guard {
   public:
    explicit loop_guard(parser *, bool was_in_loop_statement) noexcept;

    loop_guard(const loop_guard &) = delete;
    loop_guard &operator=(const loop_guard &) = delete;

    ~loop_guard() noexcept;

   private:
    parser *parser_;
    bool was_in_loop_statement_;
  };

  class switch_guard {
   public:
    explicit switch_guard(parser *, bool was_in_switch_statement) noexcept;

    switch_guard(const switch_guard &) = delete;
    switch_guard &operator=(const switch_guard &) = delete;

    ~switch_guard() noexcept;

   private:
    parser *parser_;
    bool was_in_switch_statement_;
  };

  quick_lint_js::lexer lexer_;
  error_reporter *error_reporter_;
  quick_lint_js::expression_arena expressions_;

  bool in_async_function_ = false;
  bool in_generator_function_ = false;
  bool in_loop_statement_ = false;
  bool in_switch_statement_ = false;
};
}

#undef QLJS_PARSER_UNIMPLEMENTED

#endif
