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
 public:
  explicit parser(padded_string_view input, error_reporter *error_reporter)
      : lexer_(input, error_reporter), error_reporter_(error_reporter) {}

  quick_lint_js::lexer &lexer() noexcept { return this->lexer_; }

  // For testing only.
  quick_lint_js::expression_arena &expression_arena() noexcept {
    return this->expressions_;
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_module(Visitor &v) {
    while (this->peek().type != token_type::end_of_file) {
      this->parse_and_visit_statement(v);
    }
    v.visit_end_of_module();
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_statement(Visitor &v) {
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

    // let x = 42;
    // function f() {}
    case token_type::kw_async:
    case token_type::kw_const:
    case token_type::kw_function:
    case token_type::kw_let:
    case token_type::kw_var:
      this->parse_and_visit_declaration(v);
      break;

    // import {bananas} from "Thailand";
    // import(url).then(loaded);
    case token_type::kw_import:
      this->parse_and_visit_import(v);
      break;

    // await settings.save();
    // [1, 2, 3].forEach(x => console.log(x));
    case token_type::bang:
    case token_type::complete_template:
    case token_type::incomplete_template:
    case token_type::kw_await:
    case token_type::kw_delete:
    case token_type::kw_false:
    case token_type::kw_new:
    case token_type::kw_null:
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
    case token_type::slash:
    case token_type::slash_equal:
    case token_type::string:
    case token_type::tilde:
      this->parse_and_visit_expression(v);
      this->consume_semicolon();
      break;

    // console.log("hello");
    // label: for(;;);
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
        expression_ptr ast = this->make_expression<expression::variable>(
            ident, token_type::identifier);
        ast = this->parse_expression_remainder(ast, precedence{});
        this->visit_expression(ast, v, variable_context::rhs);
        this->consume_semicolon();
        break;
      }
      break;
    }

    // class C {}
    case token_type::kw_class:
      this->parse_and_visit_class(v);
      break;

    // switch (x) { default: ; }
    case token_type::kw_switch:
      this->parse_and_visit_switch(v);
      break;

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
        this->consume_semicolon();
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
      this->consume_semicolon();
      break;

    // try { hard(); } catch (exhaustion) {}
    case token_type::kw_try:
      this->parse_and_visit_try(v);
      break;

    // do { } while (can);
    case token_type::kw_do:
      this->parse_and_visit_do_while(v);
      break;

    // for (let i = 0; i < length; ++i) {}
    // for (let x of xs) {}
    case token_type::kw_for:
      this->parse_and_visit_for(v);
      break;

    // while (cond) {}
    case token_type::kw_while:
      this->parse_and_visit_while(v);
      break;

    // with (o) { eek(); }
    case token_type::kw_with:
      this->parse_and_visit_with(v);
      break;

    // if (cond) { yay; } else { nay; }
    case token_type::kw_if:
      this->parse_and_visit_if(v);
      break;

    // break;
    case token_type::kw_break:
    case token_type::kw_continue:
      this->skip();
      this->consume_semicolon();
      break;

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

  expression_ptr parse_expression() {
    return this->parse_expression(precedence{});
  }

 private:
  enum class variable_context {
    lhs,
    rhs,
  };

  template <QLJS_PARSE_VISITOR Visitor>
  void visit_expression(expression_ptr ast, Visitor &v,
                        variable_context context) {
    auto visit_children = [&] {
      int child_count = ast->child_count();
      for (int i = 0; i < child_count; ++i) {
        this->visit_expression(ast->child(i), v, context);
      }
    };
    auto visit_parameters = [&](int parameter_count) {
      for (int i = 0; i < parameter_count; ++i) {
        expression_ptr parameter = ast->child(i);
        this->visit_binding_element(parameter, v, variable_kind::_parameter);
      }
    };
    switch (ast->kind()) {
    case expression_kind::_invalid:
    case expression_kind::import:
    case expression_kind::literal:
    case expression_kind::new_target:
    case expression_kind::super:
      break;
    case expression_kind::_new:
    case expression_kind::_template:
    case expression_kind::array:
    case expression_kind::binary_operator:
    case expression_kind::call:
    case expression_kind::tagged_template_literal:
      visit_children();
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
      expression_ptr lhs = ast->child_0();
      expression_ptr rhs = ast->child_1();
      this->visit_assignment_expression(lhs, rhs, v);
      break;
    }
    case expression_kind::compound_assignment: {
      expression_ptr lhs = ast->child_0();
      expression_ptr rhs = ast->child_1();
      this->visit_compound_assignment_expression(lhs, rhs, v);
      break;
    }
    case expression_kind::_typeof: {
      expression_ptr child = ast->child_0();
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
      expression_ptr child = ast->child_0();
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
  void visit_assignment_expression(expression_ptr lhs, expression_ptr rhs,
                                   Visitor &v) {
    this->visit_expression(lhs, v, variable_context::lhs);
    this->visit_expression(rhs, v, variable_context::rhs);
    this->maybe_visit_assignment(lhs, v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void visit_compound_assignment_expression(expression_ptr lhs,
                                            expression_ptr rhs, Visitor &v) {
    this->visit_expression(lhs, v, variable_context::rhs);
    this->visit_expression(rhs, v, variable_context::rhs);
    this->maybe_visit_assignment(lhs, v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void maybe_visit_assignment(expression_ptr ast, Visitor &v) {
    switch (ast->kind()) {
    case expression_kind::object:
      for (int i = 0; i < ast->object_entry_count(); ++i) {
        expression_ptr value = ast->object_entry(i).value;
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
      if (this->peek().type == token_type::kw_async ||
          this->peek().type == token_type::kw_class ||
          this->peek().type == token_type::kw_function) {
        this->parse_and_visit_declaration(v);
      } else {
        this->parse_and_visit_expression(v);
      }
      break;

    // export * from "module";
    case token_type::star:
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::kw_from);
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::string);
      this->skip();
      this->consume_semicolon();
      break;

    // export {a as default, b};
    // export {a, b, c} from "module";
    case token_type::left_curly: {
      null_visitor null_v;
      this->parse_and_visit_named_exports(null_v);
      if (this->peek().type == token_type::kw_from) {
        this->skip();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::string);
        this->skip();
      }
      this->consume_semicolon();
      break;
    }

    // export class C {}
    case token_type::kw_async:
    case token_type::kw_class:
    case token_type::kw_const:
    case token_type::kw_function:
    case token_type::kw_let:
    case token_type::kw_var:
      this->parse_and_visit_declaration(v);
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_declaration(Visitor &v) {
    switch (this->peek().type) {
    // async function f() {}
    case token_type::kw_async:
      this->skip();
      switch (this->peek().type) {
      case token_type::kw_function:
        this->parse_and_visit_function_declaration(v);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;

    // let x = 42;
    case token_type::kw_const:
    case token_type::kw_let:
    case token_type::kw_var:
      this->parse_and_visit_let_bindings(v, this->peek().type,
                                         /*allow_in_operator=*/true);
      this->consume_semicolon();
      break;

    // function f() {}
    case token_type::kw_function:
      this->parse_and_visit_function_declaration(v);
      break;

    // class C {}
    case token_type::kw_class:
      this->parse_and_visit_class(v);
      break;

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

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_declaration(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_function);
    this->skip();

    switch (this->peek().type) {
    case token_type::identifier:
    case token_type::kw_let:
    case token_type::kw_static:
      v.visit_variable_declaration(this->peek().identifier_name(),
                                   variable_kind::_function);
      this->skip();

      this->parse_and_visit_function_parameters_and_body(v);
      break;

    // export default function() {}
    case token_type::left_paren:
      // TODO(strager): Require name for function declarations. Functions names
      // are only optional for 'export default' and expressions.
      this->parse_and_visit_function_parameters_and_body(v);
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_parameters_and_body(Visitor &v) {
    v.visit_enter_function_scope();
    this->parse_and_visit_function_parameters_and_body_no_scope(v);
    v.visit_exit_function_scope();
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_parameters_and_body_no_scope(Visitor &v) {
    if (this->peek().type != token_type::left_paren) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();

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
      case token_type::dot_dot_dot:
      case token_type::identifier:
      case token_type::kw_let:
      case token_type::kw_static:
      case token_type::left_curly:
      case token_type::left_square:
        this->parse_and_visit_binding_element(v, variable_kind::_parameter,
                                              /*allow_in_operator=*/true);
        break;
      case token_type::right_paren:
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
  void parse_and_visit_class(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_class);
    this->skip();

    std::optional<identifier> optional_class_name;
    switch (this->peek().type) {
    case token_type::kw_let:
      this->error_reporter_->report(error_cannot_declare_class_named_let{
          .name = this->peek().identifier_name().span()});
      [[fallthrough]];
    case token_type::identifier:
      optional_class_name = this->peek().identifier_name();
      this->skip();
      break;

    // class { ... }
    case token_type::left_curly:
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    switch (this->peek().type) {
    case token_type::kw_extends:
      this->skip();
      switch (this->peek().type) {
      case token_type::identifier:
        // TODO(strager): Don't allow extending any ol' expression.
        this->parse_and_visit_expression(v, precedence{.commas = false});
        break;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
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
      // TODO(strager): Require class name for class declarations. Class names
      // are only optional for 'export default' and expressions.
    }

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

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class_body(Visitor &v) {
    while (this->peek().type != token_type::right_curly) {
      this->parse_and_visit_class_member(v);
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class_member(Visitor &v) {
    std::optional<identifier> last_ident;

  next:
    switch (this->peek().type) {
    // async f() {}
    case token_type::kw_async:
      last_ident = this->peek().identifier_name();
      this->skip();
      goto next;

    // static f() {}
    case token_type::kw_static:
      last_ident = this->peek().identifier_name();
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
    case token_type::kw_catch:
    case token_type::kw_class:
    case token_type::kw_default:
    case token_type::kw_try:
      v.visit_property_declaration(this->peek().identifier_name());
      this->skip();
      this->parse_and_visit_function_parameters_and_body(v);
      break;

    // async() {}
    // get() {}
    case token_type::left_paren:
      if (last_ident.has_value()) {
        v.visit_property_declaration(*last_ident);
        this->parse_and_visit_function_parameters_and_body(v);
      } else {
        QLJS_PARSER_UNIMPLEMENTED();
      }
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

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
    this->skip();

    this->parse_and_visit_expression(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    this->skip();

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

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
      this->skip();
      v.visit_enter_block_scope();

      switch (this->peek().type) {
      case token_type::identifier:
      case token_type::kw_let:
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

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
    this->skip();

    this->parse_and_visit_expression(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    this->skip();
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_for(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_for);
    this->skip();

    if (this->peek().type == token_type::kw_await) {
      this->skip();
    }

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
    this->skip();

    std::optional<expression_ptr> after_expression;
    auto parse_c_style_head_remainder = [&]() {
      if (this->peek().type != token_type::semicolon) {
        this->parse_and_visit_expression(v);
      }
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::semicolon);
      this->skip();

      if (this->peek().type != token_type::right_paren) {
        after_expression = this->parse_expression();
      }
    };

    bool entered_for_scope = false;

    switch (this->peek().type) {
    // for (;;) {}
    case token_type::semicolon:
      this->skip();
      parse_c_style_head_remainder();
      break;

    // for (let i = 0; i < length; ++length) {}
    // for (let x of xs) {}
    case token_type::kw_const:
    case token_type::kw_let:
      v.visit_enter_for_scope();
      entered_for_scope = true;
      [[fallthrough]];
    case token_type::kw_var: {
      token_type variable_token = this->peek().type;
      buffering_visitor lhs;
      this->parse_and_visit_let_bindings(lhs, this->peek().type,
                                         /*allow_in_operator=*/false);
      switch (this->peek().type) {
      // for (let i = 0; i < length; ++length) {}
      case token_type::semicolon:
        this->skip();
        lhs.move_into(v);
        parse_c_style_head_remainder();
        break;

      // for (let x of xs) {}
      case token_type::kw_in:
      case token_type::kw_of: {
        bool is_var_in = variable_token == token_type::kw_var &&
                         this->peek().type == token_type::kw_in;
        this->skip();
        expression_ptr rhs = this->parse_expression();
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

    // for (init; condition; update) {}
    default: {
      expression_ptr init_expression =
          this->parse_expression(precedence{.in_operator = false});
      switch (this->peek().type) {
      case token_type::semicolon:
        this->skip();
        this->visit_expression(init_expression, v, variable_context::rhs);
        parse_c_style_head_remainder();
        break;
      case token_type::kw_in:
      case token_type::kw_of: {
        this->skip();
        expression_ptr rhs = this->parse_expression();
        this->visit_assignment_expression(init_expression, rhs, v);
        break;
      }
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }
    }

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    this->skip();

    this->parse_and_visit_statement(v);

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

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
    this->skip();

    this->parse_and_visit_expression(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    this->skip();

    this->parse_and_visit_statement(v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_with(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_with);
    this->skip();

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
    this->skip();

    this->parse_and_visit_expression(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    this->skip();

    this->parse_and_visit_statement(v);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_if(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_if);
    this->skip();

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
    this->skip();

    this->parse_and_visit_expression(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    this->skip();

    this->parse_and_visit_statement(v);

    if (this->peek().type == token_type::kw_else) {
      this->skip();
      this->parse_and_visit_statement(v);
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_import(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::kw_import);
    source_code_span import_span = this->peek().span();
    this->skip();

    switch (this->peek().type) {
    // import fs from "fs";
    case token_type::identifier:
    case token_type::kw_let:
      this->parse_and_visit_binding_element(v, variable_kind::_import,
                                            /*allow_in_operator=*/true);
      break;

    // import {readFile} from "fs";
    case token_type::left_curly:
      parse_and_visit_named_exports(v);
      break;

    // import expression statement:
    //
    // import(url).then(() => { /* ... */ })
    case token_type::left_paren: {
      expression_ptr ast = this->parse_expression_remainder(
          this->make_expression<expression::import>(import_span), precedence{});
      this->visit_expression(ast, v, variable_context::rhs);
      this->consume_semicolon();
      return;
    }

    // import * as fs from "fs";
    case token_type::star:
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
        v.visit_variable_declaration(this->peek().identifier_name(),
                                     variable_kind::_import);
        this->skip();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;

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
  void parse_and_visit_named_exports(Visitor &v) {
    QLJS_ASSERT(this->peek().type == token_type::left_curly);
    this->skip();
    for (;;) {
      switch (this->peek().type) {
      case token_type::kw_let:
        this->error_reporter_->report(error_cannot_import_let{
            .import_name = this->peek().identifier_name().span()});
        [[fallthrough]];
      case token_type::kw_default:
        // TODO(strager): Is 'import {default} ...' allowed?
        [[fallthrough]];
      case token_type::identifier: {
        identifier imported_name = this->peek().identifier_name();
        this->skip();
        if (this->peek().type == token_type::kw_as) {
          this->skip();
          switch (this->peek().type) {
          case token_type::kw_default:
            // TODO(strager): Is 'import {x as default} ...' allowed?
          case token_type::identifier:
            imported_name = this->peek().identifier_name();
            this->skip();
            break;
          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
          }
        }
        v.visit_variable_declaration(imported_name, variable_kind::_import);
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
  void parse_and_visit_let_bindings(Visitor &v, token_type declaring_token,
                                    bool allow_in_operator) {
    variable_kind declaration_kind;
    switch (declaring_token) {
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
    this->parse_and_visit_let_bindings(v, declaration_kind,
                                       /*allow_in_operator=*/allow_in_operator);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_let_bindings(Visitor &v, variable_kind declaration_kind,
                                    bool allow_in_operator) {
    source_code_span let_span = this->peek().span();
    this->skip();
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
      case token_type::identifier:
      case token_type::kw_let:
      case token_type::kw_static:
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

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_binding_element(Visitor &v,
                                       variable_kind declaration_kind,
                                       bool allow_in_operator) {
    expression_ptr ast = this->parse_expression(
        precedence{.commas = false, .in_operator = allow_in_operator});
    this->visit_binding_element(ast, v, declaration_kind);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void visit_binding_element(expression_ptr ast, Visitor &v,
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
        if (declaration_kind == variable_kind::_import) {
          this->error_reporter_->report(
              error_cannot_import_let{.import_name = ident.span()});
        } else {
          this->error_reporter_->report(
              error_cannot_declare_variable_named_let_with_let{
                  .name = ident.span()});
        }
      }
      v.visit_variable_declaration(ident, declaration_kind);
      break;
    }
    case expression_kind::object:
      for (int i = 0; i < ast->object_entry_count(); ++i) {
        expression_ptr value = ast->object_entry(i).value;
        this->visit_binding_element(value, v, declaration_kind);
      }
      break;
    case expression_kind::spread:
      this->visit_binding_element(ast->child_0(), v, declaration_kind);
      break;
    default:
      QLJS_ASSERT(false && "Not yet implemented");
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
    expression_ptr ast = this->parse_expression(prec);
    this->visit_expression(ast, v, variable_context::rhs);
  }

  expression_ptr parse_expression(precedence);

  expression_ptr parse_expression_remainder(expression_ptr, precedence);

  // Args is either of the following:
  // * vector<expression_ptr> &&parameters
  // * (none)
  template <class... Args>
  expression_ptr parse_arrow_function_body(function_attributes,
                                           const char8 *parameter_list_begin,
                                           Args &&... args);

  expression_ptr parse_function_expression(function_attributes,
                                           const char8 *span_begin);

  expression_ptr parse_object_literal();

  expression_ptr parse_template(std::optional<expression_ptr> tag);

  void consume_semicolon();

  const token &peek() const noexcept { return this->lexer_.peek(); }
  void skip() noexcept { this->lexer_.skip(); }

  [[noreturn]] void crash_on_unimplemented_token(
      const char *qljs_file_name, int qljs_line,
      const char *qljs_function_name);

  template <class Expression, class... Args>
  expression_ptr make_expression(Args &&... args) {
    return this->expressions_.make_expression<Expression>(
        std::forward<Args>(args)...);
  }

  quick_lint_js::lexer lexer_;
  error_reporter *error_reporter_;
  quick_lint_js::expression_arena expressions_;
};
}

#undef QLJS_PARSER_UNIMPLEMENTED

#endif
