// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/container/pmr/memory_resource.hpp>
#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diag-reporter.h>
#include <quick-lint-js/diagnostic-types.h>
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
void parser::parse_and_visit_module(parse_visitor_base &v) {
  bool done = false;
  while (!done) {
    bool parsed_statement = this->parse_and_visit_statement(
        v, parse_statement_type::any_statement_in_block);
    if (!parsed_statement) {
      switch (this->peek().type) {
      case token_type::end_of_file:
        done = true;
        break;

      case token_type::right_curly:
        this->diag_reporter_->report(diag_unmatched_right_curly{
            .right_curly = this->peek().span(),
        });
        this->skip();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
  }
  v.visit_end_of_module();
}

bool parser::parse_and_visit_statement(
    parse_visitor_base &v, parser::parse_statement_type statement_type) {
  depth_guard d_guard(this);
  auto parse_expression_end = [this]() -> void {
    while (this->peek().type == token_type::right_paren) {
      this->diag_reporter_->report(diag_unmatched_parenthesis{
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
    } else if (this->is_let_token_a_variable_reference(
                   this->peek(), /*allow_declarations=*/statement_type !=
                                     parse_statement_type::no_declarations)) {
      // Expression.
      this->lexer_.roll_back_transaction(std::move(transaction));
      expression *ast =
          this->parse_expression(v, precedence{.in_operator = true});
      this->visit_expression(ast, v, variable_context::rhs);
      parse_expression_end();
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
    QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    case token_type::comma:
    case token_type::complete_template:
    case token_type::dot:
    case token_type::end_of_file:
    case token_type::equal:
    case token_type::equal_greater:
    case token_type::identifier:
    case token_type::incomplete_template:
    case token_type::kw_in:
    case token_type::kw_yield:
    case token_type::left_paren:
    case token_type::less:
    case token_type::minus:
    case token_type::minus_minus:
    case token_type::plus:
    case token_type::plus_plus:
    case token_type::question:
    case token_type::semicolon:
    case token_type::slash: {
      expression *ast =
          this->parse_async_expression(v, async_token, precedence{});
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
  case token_type::comma:
  case token_type::complete_template:
  case token_type::dot:
  case token_type::equal:
  case token_type::equal_greater:
  case token_type::incomplete_template:
  case token_type::kw_delete:
  case token_type::kw_false:
  case token_type::kw_in:
  case token_type::kw_new:
  case token_type::kw_null:
  case token_type::kw_super:
  case token_type::kw_this:
  case token_type::kw_true:
  case token_type::kw_typeof:
  case token_type::kw_void:
  case token_type::left_paren:
  case token_type::left_square:
  case token_type::less:
  case token_type::minus:
  case token_type::minus_minus:
  case token_type::number:
  case token_type::plus:
  case token_type::plus_plus:
  case token_type::private_identifier:
  case token_type::right_paren:
  case token_type::slash:
  case token_type::slash_equal:
  case token_type::string:
  case token_type::tilde: {
    if (this->peek().type == token_type::star) {
      // * 42; // Invalid (missing operand).
      // *function f() {} // Invalid (misplaced '*').
      token star_token = this->peek();
      std::optional<function_attributes> attributes =
          this->try_parse_function_with_leading_star();
      if (attributes.has_value()) {
        this->parse_and_visit_function_declaration(
            v, attributes.value(),
            /*begin=*/star_token.begin,
            /*require_name=*/
            name_requirement::required_for_statement);
        break;
      }
    }
    this->parse_and_visit_expression(v);
    parse_expression_end();
    break;
  }

    // await settings.save();
    // await = value;
    // await: for(;;);
  case token_type::kw_await: {
    token await_token = this->peek();
    this->skip();
    if (this->peek().type == token_type::colon) {
      // Labelled statement.
      if (this->in_async_function_) {
        this->diag_reporter_->report(
            diag_label_named_await_not_allowed_in_async_function{
                .await = await_token.span(), .colon = this->peek().span()});
      }
      this->skip();
      goto parse_statement;
    } else {
      expression *ast =
          this->parse_await_expression(v, await_token, precedence{});
      ast = this->parse_expression_remainder(v, ast, precedence{});
      this->visit_expression(ast, v, variable_context::rhs);
      parse_expression_end();
    }
    break;
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
  case token_type::kw_as:
  case token_type::kw_from:
  case token_type::kw_get:
  case token_type::kw_of:
  case token_type::kw_set:
  case token_type::kw_static:
  case token_type::identifier: {
    token_type ident_token_type = this->peek().type;
    identifier ident = this->peek().identifier_name();
    this->skip();
    switch (this->peek().type) {
      // Labelled statement.
    case token_type::colon:
      this->skip();
      goto parse_statement;

      // Expression statement.
    default:
      expression *ast =
          this->make_expression<expression::variable>(ident, ident_token_type);
      ast = this->parse_expression_remainder(v, ast, precedence{});
      this->visit_expression(ast, v, variable_context::rhs);
      parse_expression_end();
      break;
    }
    break;
  }

    // \u{69}\u{66} // 'if', but escaped.
  case token_type::reserved_keyword_with_escape_sequence:
    this->lexer_.peek().report_errors_for_escape_sequences_in_keyword(
        this->diag_reporter_);
    goto parse_loop_label_or_expression_starting_with_identifier;

  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    goto parse_loop_label_or_expression_starting_with_identifier;

    // class C {}
  case token_type::kw_class: {
    this->parse_and_visit_class(
        v,
        /*require_name=*/name_requirement::required_for_statement);
    break;
  }

    // switch (x) { default: ; }
  case token_type::kw_switch: {
    switch_guard s(this, std::exchange(this->in_switch_statement_, true));
    this->parse_and_visit_switch(v);
    break;
  }

    // return;
    // return 42;
  case token_type::kw_return: {
    source_code_span return_span = this->peek().span();
    this->skip();
    switch (this->peek().type) {
    case token_type::semicolon:
      this->skip();
      break;

    case token_type::right_curly:
      break;

    default:
      if (this->peek().has_leading_newline) {
        switch (this->peek().type) {
          // 'return' followed by a newline (ASI) followed by an expression.
        case token_type::bang:
        case token_type::complete_template:
        case token_type::identifier:
        case token_type::incomplete_template:
        case token_type::kw_await:
        case token_type::kw_false:
        case token_type::kw_function:
        case token_type::kw_new:
        case token_type::kw_null:
        case token_type::kw_super:
        case token_type::kw_this:
        case token_type::kw_true:
        case token_type::kw_typeof:
        case token_type::left_curly:  // Object literal.
        case token_type::left_paren:
        case token_type::left_square:  // Array literal.
        case token_type::less:
        case token_type::minus:
        case token_type::number:
        case token_type::plus:
        case token_type::slash:        // Regular expression.
        case token_type::slash_equal:  // Regular expression.
        case token_type::string:
        case token_type::tilde:
          if (statement_type == parse_statement_type::any_statement_in_block) {
            this->diag_reporter_->report(diag_return_statement_returns_nothing{
                .return_keyword = return_span,
            });
          }
          break;

        default:
          break;
        }
        // Insert a semicolon, then consume it.
      } else {
        this->parse_and_visit_expression(v);
        parse_expression_end();
      }
      break;
    }
    break;
  }

    // throw fit;
  case token_type::kw_throw:
    this->skip();
    if (this->peek().type == token_type::semicolon) {
      this->diag_reporter_->report(
          diag_expected_expression_before_semicolon{this->peek().span()});
      this->skip();
      break;
    }
    if (this->peek().has_leading_newline) {
      this->lexer_.insert_semicolon();
      this->diag_reporter_->report(
          diag_expected_expression_before_newline{this->peek().span()});
      this->skip();
      break;
    }
    this->parse_and_visit_expression(v);
    parse_expression_end();
    break;

    // try { hard(); } catch (exhaustion) {}
  case token_type::kw_try:
    this->parse_and_visit_try_maybe_catch_maybe_finally(v);
    break;

    // catch (e) { }  // Invalid.
  case token_type::kw_catch: {
    this->diag_reporter_->report(diag_catch_without_try{
        .catch_token = this->peek().span(),
    });
    bool parsed_catch = this->parse_and_visit_catch_or_finally_or_both(v);
    QLJS_ASSERT(parsed_catch);
    break;
  }

    // finally { }  // Invalid.
  case token_type::kw_finally: {
    this->diag_reporter_->report(diag_finally_without_try{
        .finally_token = this->peek().span(),
    });
    bool parsed_finally = this->parse_and_visit_catch_or_finally_or_both(v);
    QLJS_ASSERT(parsed_finally);
    break;
  }

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
  case token_type::kw_else: {
    this->diag_reporter_->report(diag_else_has_no_if{
        .else_token = this->peek().span(),
    });
    this->skip();

    bool parsed_else_body = this->parse_and_visit_statement(v);
    if (!parsed_else_body) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    break;
  }

    // break;
    // continue label;
  case token_type::kw_break:
  case token_type::kw_continue: {
    bool is_break = this->peek().type == token_type::kw_break;
    source_code_span token_span = this->peek().span();
    this->skip();
    switch (this->peek().type) {
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    case token_type::identifier:
    case token_type::kw_await:
    case token_type::kw_yield:
      if (this->peek().has_leading_newline) {
        // ASI.
        this->lexer_.insert_semicolon();
      } else {
        // Loop label.
        this->skip();
      }
      break;
    default:
      if (is_break) {
        if (!(this->in_switch_statement_ || this->in_loop_statement_)) {
          this->diag_reporter_->report(diag_invalid_break{token_span});
        }
      } else {
        if (!this->in_loop_statement_) {
          this->diag_reporter_->report(diag_invalid_continue{token_span});
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

    // enum E { a, b, c }  // TypeScript.
  case token_type::kw_enum: {
    this->diag_reporter_->report(diag_typescript_enum_not_implemented{
        .enum_keyword = this->peek().span(),
    });
    this->skip();
    break;
  }

    // { statement; statement; }
  case token_type::left_curly:
    v.visit_enter_block_scope();
    this->parse_and_visit_statement_block_no_scope(v);
    v.visit_exit_block_scope();
    break;

    // case 3:  // Invalid.
  case token_type::kw_case:
    this->diag_reporter_->report(diag_unexpected_case_outside_switch_statement{
        .case_token = this->peek().span(),
    });
    this->skip();
    this->parse_and_visit_expression(v);
    if (this->peek().type == token_type::colon) {
      this->skip();
    }
    break;

    // default:  // Invalid.
  case token_type::kw_default:
    this->diag_reporter_->report(
        diag_unexpected_default_outside_switch_statement{
            .default_token = this->peek().span(),
        });
    this->skip();
    if (this->peek().type == token_type::colon) {
      this->skip();
    }
    break;

  case token_type::colon:
  case token_type::kw_extends:
  case token_type::question:
    this->diag_reporter_->report(diag_unexpected_token{
        .token = this->peek().span(),
    });
    this->skip();
    break;

  case token_type::end_of_file:
  case token_type::right_curly:
    return false;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  return true;
}

void parser::parse_and_visit_export(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_export);
  source_code_span export_token_span = this->peek().span();
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
            this->parse_async_expression(v, async_token, precedence{});
        this->visit_expression(ast, v, variable_context::rhs);
        this->consume_semicolon();
      }
      break;
    }

      // export default class C {}
    case token_type::kw_class:
      this->parse_and_visit_class(v,
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
      this->diag_reporter_->report(diag_cannot_export_default_variable{
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
      case token_type::string:
        // TODO(strager): Check that the string is valid Unicode
        // (standard: IsStringWellFormedUnicode).
        [[fallthrough]];
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::reserved_keyword_with_escape_sequence:
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
    buffering_visitor &exports_visitor = this->buffering_visitor_stack_.emplace(
        boost::container::pmr::new_delete_resource());
    bump_vector<token, monotonic_allocator> exported_bad_tokens(
        "parse_and_visit_export exported_bad_tokens", &this->temporary_memory_);
    this->parse_and_visit_named_exports_for_export(
        exports_visitor, /*out_exported_bad_tokens=*/exported_bad_tokens);
    if (this->peek().type == token_type::kw_from) {
      // export {a, b, c} from "module";
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::string);
      this->skip();
      // Ignore exported_keywords.
    } else {
      // export {a as default, b};
      for (const token &exported_bad_token : exported_bad_tokens) {
        switch (exported_bad_token.type) {
        case token_type::reserved_keyword_with_escape_sequence:
          exported_bad_token.report_errors_for_escape_sequences_in_keyword(
              this->diag_reporter_);
          break;
        case token_type::string:
          this->diag_reporter_->report(
              diag_exporting_string_name_only_allowed_for_export_from{
                  .export_name = exported_bad_token.span(),
              });
          break;
        default:
          this->diag_reporter_->report(
              diag_cannot_export_variable_named_keyword{
                  .export_name = exported_bad_token.identifier_name(),
              });
          break;
        }
      }
      exports_visitor.move_into(v);
    }

    QLJS_ASSERT(&this->buffering_visitor_stack_.top() == &exports_visitor);
    this->buffering_visitor_stack_.pop();

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
    expression *ast = this->parse_expression(v);
    switch (ast->kind()) {
    case expression_kind::variable:
      this->diag_reporter_->report(diag_exporting_requires_curlies{
          .names = ast->span(),
      });
      break;
    default:
      this->diag_reporter_->report(diag_exporting_requires_default{
          .expression = ast->span(),
      });
      break;
    }
    this->visit_expression(ast, v, variable_context::rhs);
    this->consume_semicolon();
    break;
  }

  case token_type::end_of_file:
  case token_type::semicolon:
    this->diag_reporter_->report(diag_missing_token_after_export{
        .export_token = export_token_span,
    });
    break;

  default:
    this->diag_reporter_->report(diag_unexpected_token_after_export{
        .unexpected_token = this->peek().span(),
    });
    break;
  }
}

void parser::parse_and_visit_statement_block_no_scope(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  source_code_span left_curly_span = this->peek().span();
  this->skip();

  for (;;) {
    bool parsed_statement = this->parse_and_visit_statement(
        v, parse_statement_type::any_statement_in_block);
    if (!parsed_statement) {
      switch (this->peek().type) {
      case token_type::right_curly:
        this->skip();
        return;

      case token_type::end_of_file:
        this->diag_reporter_->report(diag_unclosed_code_block{
            .block_open = left_curly_span,
        });
        return;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
  }
}

void parser::parse_and_visit_function_declaration(
    parse_visitor_base &v, function_attributes attributes, const char8 *begin,
    parser::name_requirement require_name) {
  QLJS_ASSERT(this->peek().type == token_type::kw_function);
  source_code_span function_token_span = this->peek().span();
  const char8 *function_token_begin = function_token_span.begin();
  this->skip();
  attributes = this->parse_generator_star(attributes);

  switch (this->peek().type) {
  case token_type::kw_await:
    if (this->in_async_function_) {
      this->diag_reporter_->report(diag_cannot_declare_await_in_async_function{
          .name = this->peek().identifier_name(),
      });
    }
    goto named_function;

  case token_type::kw_yield:
    if (this->in_generator_function_) {
      this->diag_reporter_->report(
          diag_cannot_declare_yield_in_generator_function{
              .name = this->peek().identifier_name(),
          });
    }
    goto named_function;

  // function protected() {}
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    goto named_function;

  named_function:
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier: {
    if (this->peek().type == token_type::kw_let &&
        require_name == name_requirement::required_for_export) {
      this->diag_reporter_->report(diag_cannot_export_let{
          .export_name = this->peek().span(),
      });
    }
    identifier function_name = this->peek().identifier_name();
    v.visit_variable_declaration(function_name, variable_kind::_function,
                                 variable_init_kind::normal);
    this->skip();

    this->parse_and_visit_function_parameters_and_body(
        v, /*name=*/function_name.span(), attributes);
    break;
  }

    // export default function() {}
  case token_type::left_paren:
    switch (require_name) {
    case name_requirement::required_for_statement: {
      const char8 *left_paren_end = this->peek().end;

      // The function should have a name, but doesn't have a name. Perhaps the
      // user intended to include parentheses. Parse the function as an
      // expression instead of as a declaration.
      this->parse_and_visit_function_parameters_and_body(
          v, /*name=*/std::nullopt, attributes);
      const char8 *function_end = this->lexer_.end_of_previous_token();
      expression *function = this->make_expression<expression::function>(
          attributes, source_code_span(function_token_begin, function_end));
      expression *full_expression =
          this->parse_expression_remainder(v, function, precedence{});
      this->visit_expression(full_expression, v, variable_context::rhs);

      if (full_expression == function) {
        this->diag_reporter_->report(diag_missing_name_in_function_statement{
            .where = source_code_span(function_token_begin, left_paren_end),
        });
      } else {
        this->diag_reporter_->report(
            diag_missing_name_or_parentheses_for_function{
                .where = source_code_span(function_token_begin, left_paren_end),
                .function = source_code_span(begin, function->span().end()),
            });
      }
      break;
    }

    case name_requirement::required_for_export: {
      this->diag_reporter_->report(diag_missing_name_of_exported_function{
          .function_keyword = function_token_span,
      });
      this->parse_and_visit_function_parameters_and_body(
          v, /*name=*/std::nullopt, attributes);
      break;
    }

    case name_requirement::optional:
      this->parse_and_visit_function_parameters_and_body(
          v, /*name=*/std::nullopt, attributes);
      break;
    }
    break;

    // { function }  // Invalid.
  default:
    this->diag_reporter_->report(diag_missing_name_in_function_statement{
        .where = function_token_span,
    });
    break;
  }
}

void parser::parse_and_visit_function_parameters_and_body(
    parse_visitor_base &v, std::optional<source_code_span> name,
    function_attributes attributes) {
  v.visit_enter_function_scope();
  this->parse_and_visit_function_parameters_and_body_no_scope(v, name,
                                                              attributes);
  v.visit_exit_function_scope();
}

void parser::parse_and_visit_function_parameters_and_body_no_scope(
    parse_visitor_base &v, std::optional<source_code_span> name,
    function_attributes attributes) {
  function_guard guard = this->enter_function(attributes);

  if (this->peek().type == token_type::star) {
    if (!name.has_value()) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    // TODO(strager): Emit a different error if a star was already present
    // (e.g. function* f*() {}).
    this->diag_reporter_->report(
        diag_generator_function_star_belongs_before_name{
            .function_name = *name,
            .star = this->peek().span(),
        });
    this->in_generator_function_ = true;  // Restored by existing guard.
    this->skip();
  }

  switch (this->peek().type) {
    // function f(arg0, arg1) {}
  case token_type::left_paren:
    this->skip();

    this->parse_and_visit_function_parameters(v);

    if (this->peek().type != token_type::right_paren) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();

    if (this->peek().type == token_type::equal_greater) {
      this->diag_reporter_->report(
          diag_functions_or_methods_should_not_have_arrow_operator{
              .arrow_operator = this->peek().span(),
          });
      this->skip();
    }

    if (this->peek().type != token_type::left_curly) {
      const char8 *expected_body = this->lexer_.end_of_previous_token();
      this->diag_reporter_->report(diag_missing_function_body{
          .expected_body = source_code_span(expected_body, expected_body)});

      // Don't parse a function body.
      return;
    }
    break;

    // function f {}  // Invalid.
  case token_type::left_curly:
    this->diag_reporter_->report(diag_missing_function_parameter_list{
        .expected_parameter_list =
            source_code_span(this->lexer_.end_of_previous_token(),
                             this->lexer_.end_of_previous_token()),
    });
    break;

    // { function f }  // Invalid.
  case token_type::comma:
  case token_type::dot:
  case token_type::number:
  case token_type::right_curly:
    this->diag_reporter_->report(diag_missing_function_parameter_list{
        .expected_parameter_list =
            source_code_span(this->lexer_.end_of_previous_token(),
                             this->lexer_.end_of_previous_token()),
    });
    // Don't parse a function body.
    return;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  v.visit_enter_function_scope_body();

  this->parse_and_visit_statement_block_no_scope(v);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
void parser::parse_and_visit_function_parameters(parse_visitor_base &v) {
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
    QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
      [[fallthrough]];
    case token_type::kw_await:
      // TODO(#241): Disallow parameters named 'await' for async functions.
      [[fallthrough]];
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    case token_type::dot_dot_dot:
    case token_type::identifier:
    case token_type::kw_yield:
    case token_type::left_curly:
    case token_type::left_square:
    case token_type::number:
    case token_type::reserved_keyword_with_escape_sequence: {
      expression *parameter = this->parse_expression(
          v, precedence{.commas = false, .in_operator = true});
      this->visit_binding_element(parameter, v, variable_kind::_parameter,
                                  /*declaring_token=*/std::nullopt,
                                  /*init_kind=*/variable_init_kind::normal);
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
        this->diag_reporter_->report(
            diag_comma_not_allowed_after_spread_parameter{
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
done:;
}
QLJS_WARNING_POP

void parser::parse_and_visit_class(parse_visitor_base &v,
                                   parser::name_requirement require_name) {
  QLJS_ASSERT(this->peek().type == token_type::kw_class);

  this->parse_and_visit_class_heading(v, /*require_name=*/require_name);

  switch (this->peek().type) {
  case token_type::left_curly:
    v.visit_enter_class_scope();
    this->parse_and_visit_class_body(v);
    v.visit_exit_class_scope();
    break;

  default: {
    const char8 *here = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_missing_body_for_class{
        .class_keyword_and_name_and_heritage = source_code_span(here, here),
    });
    break;
  }
  }
}

void parser::parse_and_visit_class_heading(
    parse_visitor_base &v, parser::name_requirement require_name) {
  QLJS_ASSERT(this->peek().type == token_type::kw_class);
  source_code_span class_keyword_span = this->peek().span();
  this->skip();

  std::optional<identifier> optional_class_name;
  switch (this->peek().type) {
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    [[fallthrough]];
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
  case token_type::kw_await:
  case token_type::kw_yield:
    // TODO(#707): Disallow classes named 'await' in async functions.
    // TODO(#707): Disallow classes named 'yield' in generator functions.
    if (this->peek().type == token_type::kw_let) {
      this->diag_reporter_->report(diag_cannot_declare_class_named_let{
          .name = this->peek().identifier_name().span()});
    }
    optional_class_name = this->peek().identifier_name();
    this->skip();
    break;

    // class { ... }
  case token_type::left_curly:
    break;

    // class extends C { }
  case token_type::kw_extends:
    break;

    // { class }  // Invalid.
    // class;     // Invalid.
  default:
    // We'll report errors later.
    break;
  }

  switch (this->peek().type) {
  case token_type::kw_extends:
    this->skip();
    // TODO(strager): Error when extending things like '0' or 'true'.
    this->parse_and_visit_expression(v,
                                     precedence{
                                         .commas = false,
                                         .trailing_curly_is_arrow_body = false,
                                     });
    break;

  case token_type::left_curly:
    break;

    // class C;     // Invalid.
    // { class C }  // Invalid.
  default:
    // parse_and_visit_class or parse_class_expression will report an error.
    break;
  }

  if (optional_class_name.has_value()) {
    v.visit_variable_declaration(*optional_class_name, variable_kind::_class,
                                 variable_init_kind::normal);
  } else {
    switch (require_name) {
    case name_requirement::optional:
      break;
    case name_requirement::required_for_export:
      this->diag_reporter_->report(diag_missing_name_of_exported_class{
          .class_keyword = class_keyword_span,
      });
      break;
    case name_requirement::required_for_statement:
      this->diag_reporter_->report(diag_missing_name_in_class_statement{
          .class_keyword = class_keyword_span,
      });
      break;
    }
  }
}

void parser::parse_and_visit_class_body(parse_visitor_base &v) {
  class_guard g(this, std::exchange(this->in_class_, true));

  source_code_span left_curly_span = this->peek().span();
  this->skip();

  while (this->peek().type != token_type::right_curly) {
    this->parse_and_visit_class_member(v);
    if (this->peek().type == token_type::end_of_file) {
      this->diag_reporter_->report(diag_unclosed_class_block{
          .block_open = left_curly_span,
      });
      return;
    }
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
  this->skip();
}

void parser::parse_and_visit_class_member(parse_visitor_base &v) {
  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wshadow-local")

  std::optional<identifier> last_ident;
  const char8 *async_token_begin = nullptr;
  function_attributes method_attributes = function_attributes::normal;
  bool async_static = false;

  auto parse_and_visit_field_or_method_impl =
      [this, &v, &async_static, &async_token_begin](
          std::optional<identifier> property_name,
          source_code_span property_name_span,
          function_attributes method_attributes) -> void {
    if (async_static) {
      if (this->peek().type == token_type::star) {
        // async static *m() {}  // Invalid.
        method_attributes = function_attributes::async_generator;
        this->skip();
      }
      switch (this->peek().type) {
      QLJS_CASE_RESERVED_KEYWORD_EXCEPT_FUNCTION:
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case token_type::identifier:
      case token_type::private_identifier:
      case token_type::reserved_keyword_with_escape_sequence:
        // async static method() {}             // Invalid
        // async static *myAsyncGenerator() {}  // Invalid
        this->diag_reporter_->report(diag_async_static_method{
            .async_static =
                source_code_span(async_token_begin, property_name_span.end()),
        });
        property_name = this->peek().identifier_name();
        property_name_span = property_name->span();
        this->skip();
        break;
        // async static() {}
      default:
        break;
      }
    }

    switch (this->peek().type) {
      // method() { }
      // method { }    // Invalid (missing parameter list).
    case token_type::left_curly:
    case token_type::left_paren:
      v.visit_property_declaration(property_name);
      this->parse_and_visit_function_parameters_and_body(
          v, /*name=*/property_name_span, method_attributes);
      break;

      // field;
      // class C { field }
    case token_type::right_curly:
    case token_type::semicolon:
      v.visit_property_declaration(property_name);
      this->consume_semicolon();
      break;

      // field = initialValue;
    case token_type::equal:
      this->skip();
      this->parse_and_visit_expression(v);
      v.visit_property_declaration(property_name);
      this->consume_semicolon();
      break;

    case token_type::identifier:
    case token_type::private_identifier:
    case token_type::star:
      if (this->peek().has_leading_newline) {
        // class C {
        //   field        // ASI
        //   method() {}
        // }
        v.visit_property_declaration(property_name);
      } else {
        // class C {
        //   const field
        // }
        if (u8"const" == property_name_span.string_view()) {
          this->diag_reporter_->report(diag_typescript_style_const_field{
              .const_token = property_name_span,
          });
        } else {
          this->diag_reporter_->report(diag_unexpected_token{
              .token = property_name_span,
          });
        }
      }
      break;

    QLJS_CASE_KEYWORD:
    case token_type::left_square:
    case token_type::number:
    case token_type::string:
      if (this->peek().has_leading_newline) {
        // class C {
        //   field        // ASI
        //   [expr]() {}
        // }
        v.visit_property_declaration(property_name);
      } else {
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  };

  auto parse_and_visit_field_or_method =
      [&](identifier property_name,
          function_attributes method_attributes) -> void {
    parse_and_visit_field_or_method_impl(property_name, property_name.span(),
                                         method_attributes);
  };

  auto parse_and_visit_field_or_method_without_name =
      [&](source_code_span name_span,
          function_attributes method_attributes) -> void {
    parse_and_visit_field_or_method_impl(std::nullopt, name_span,
                                         method_attributes);
  };

  switch (this->peek().type) {
    // static f() {}
  case token_type::kw_static:
    last_ident = this->peek().identifier_name();
    this->skip();
    break;

  default:
    break;
  }

next:
  switch (this->peek().type) {
    // async f() {}
  case token_type::kw_async:
    last_ident = this->peek().identifier_name();
    async_token_begin = this->peek().begin;
    this->skip();
    if (this->peek().has_leading_newline) {
      switch (this->peek().type) {
        // 'async' is a field name:
        // class {
        //   async
        //   method() {}
        // }
      QLJS_CASE_KEYWORD:
      case token_type::left_square:
      case token_type::number:
      case token_type::string:
      case token_type::identifier:
      case token_type::private_identifier:
      case token_type::star:
        v.visit_property_declaration(last_ident);
        return;
      default:
        break;
      }
    } else {
      if (this->peek().type != token_type::left_paren) {
        method_attributes = function_attributes::async;
      }
      if (this->peek().type == token_type::star) {
        // async *g() {}
        method_attributes = function_attributes::async_generator;
        this->skip();
      }
      if (this->peek().type == token_type::kw_static) {
        // async static method() {}  // Invalid
        // async static() {}
        async_static = true;
      }
    }
    break;

    // *g() {}
  case token_type::star:
    method_attributes = function_attributes::generator;
    this->skip();
    break;

    // get prop() {}
  case token_type::kw_get:
  case token_type::kw_set:
    last_ident = this->peek().identifier_name();
    this->skip();
    break;

  default:
    break;
  }

  switch (this->peek().type) {
    // method() {}
    // static() {}
    // #method() {}
    // field;
    // field = initialValue;
    // #field = initialValue;
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_FUNCTION:
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
  case token_type::private_identifier:
  case token_type::reserved_keyword_with_escape_sequence: {
    identifier property_name = this->peek().identifier_name();
    this->skip();
    parse_and_visit_field_or_method(property_name, method_attributes);
    break;
  }

    // "method"() {}
    // 9001() {}
    // "fieldName" = init;
  case token_type::number:
  case token_type::string: {
    source_code_span name_span = this->peek().span();
    this->skip();
    parse_and_visit_field_or_method_without_name(name_span, method_attributes);
    break;
  }

    // [methodNameExpression]() {}
    // [fieldNameExpression] = initialValue;
  case token_type::left_square: {
    const char8 *name_begin = this->peek().begin;
    this->skip();

    this->parse_and_visit_expression(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
    const char8 *name_end = this->peek().end;
    this->skip();

    parse_and_visit_field_or_method_without_name(
        source_code_span(name_begin, name_end), method_attributes);
    break;
  }

    // function() {}
    // function f() {}  // Invalid.
  case token_type::kw_function: {
    token function_token = this->peek();
    this->skip();
    switch (this->peek().type) {
      // function() {}
      // class C { function }   // Field named 'function'.
      // class C { function; }  // Field named 'function'.
      // function = init;       // Field named 'function'.
    case token_type::equal:
    case token_type::left_paren:
    case token_type::right_curly:
    case token_type::semicolon:
      parse_and_visit_field_or_method(function_token.identifier_name(),
                                      method_attributes);
      break;

    default:
      // function f() {}  // Invalid.
      this->diag_reporter_->report(diag_methods_should_not_use_function_keyword{
          .function_token = function_token.span(),
      });
      goto next;
    }
    break;
  }

    // async;  // Field named 'async'.
    // ;       // Stray semicolon.
  case token_type::semicolon:
    if (last_ident.has_value()) {
      parse_and_visit_field_or_method(*last_ident, method_attributes);
    } else {
      this->skip();
    }
    break;

    // async() {}
    // get() {}
    // class C { get }  // Field named 'get'
    // get = init;
  case token_type::equal:
  case token_type::left_paren:
  case token_type::right_curly:
    if (last_ident.has_value()) {
      parse_and_visit_field_or_method(*last_ident, method_attributes);
    } else {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    break;

  case token_type::left_curly:
    this->diag_reporter_->report(diag_unexpected_token{
        .token = this->peek().span(),
    });
    this->skip();
    break;

  case token_type::comma:
    this->diag_reporter_->report(diag_comma_not_allowed_between_class_methods{
        .unexpected_comma = this->peek().span(),
    });
    this->skip();
    break;

  // class C {  // Invalid.
  case token_type::end_of_file:
    // An error is reported by parse_and_visit_class_body.
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  QLJS_WARNING_POP
}

void parser::parse_and_visit_switch(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_switch);
  source_code_span switch_token_span = this->peek().span();
  this->skip();

  if (this->peek().type == token_type::left_curly) {
    // switch { case 1: break; }  // Invalid.
    this->diag_reporter_->report(diag_missing_condition_for_switch_statement{
        .switch_keyword = switch_token_span,
    });
  } else {
    this->parse_and_visit_parenthesized_expression<
        diag_expected_parentheses_around_switch_condition,
        diag_expected_parenthesis_around_switch_condition,
        /*CheckForSketchyConditions=*/false>(v);
  }

  switch (this->peek().type) {
  case token_type::left_curly:
    this->skip();
    break;

  case token_type::kw_case:
  case token_type::kw_default: {
    const char8 *here = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_expected_left_curly{
        .expected_left_curly = source_code_span(here, here),
    });
    break;
  }

  default: {
    const char8 *here = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_missing_body_for_switch_statement{
        .switch_and_condition = source_code_span(here, here),
    });
    return;
  }
  }
  v.visit_enter_block_scope();

  bool keep_going = true;
  bool is_before_first_switch_case = true;
  while (keep_going) {
    switch (this->peek().type) {
    case token_type::right_curly:
      this->skip();
      keep_going = false;
      break;

    case token_type::kw_case: {
      is_before_first_switch_case = false;
      source_code_span case_token_span = this->peek().span();
      this->skip();
      if (this->peek().type == token_type::colon) {
        this->diag_reporter_->report(diag_expected_expression_for_switch_case{
            .case_token = case_token_span,
        });
        this->skip();
      } else {
        this->parse_and_visit_expression(v);
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
        this->skip();
      }
      break;
    }

    case token_type::kw_default:
      is_before_first_switch_case = false;
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
      this->skip();
      break;

    default: {
      if (is_before_first_switch_case) {
        this->diag_reporter_->report(diag_statement_before_first_switch_case{
            .unexpected_statement = this->peek().span(),
        });
      }
      bool parsed_statement = this->parse_and_visit_statement(
          v, parse_statement_type::any_statement_in_block);
      if (!parsed_statement) {
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;
    }
    }
  }

  v.visit_exit_block_scope();
}

void parser::parse_and_visit_try_maybe_catch_maybe_finally(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_try);
  source_code_span try_token_span = this->peek().span();
  this->skip();

  bool parsed_try_body = false;
  if (this->peek().type == token_type::left_curly) {
    parsed_try_body = true;
    v.visit_enter_block_scope();
    this->parse_and_visit_statement_block_no_scope(v);
    v.visit_exit_block_scope();
  } else {
    this->diag_reporter_->report(diag_missing_body_for_try_statement{
        .try_token = try_token_span,
    });
  }

  bool parsed_catch_or_finally =
      this->parse_and_visit_catch_or_finally_or_both(v);
  if (parsed_try_body && !parsed_catch_or_finally) {
    const char8 *expected_catch_or_finally =
        this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(
        diag_missing_catch_or_finally_for_try_statement{
            .expected_catch_or_finally = source_code_span(
                expected_catch_or_finally, expected_catch_or_finally),
            .try_token = try_token_span,
        });
  }
}

bool parser::parse_and_visit_catch_or_finally_or_both(parse_visitor_base &v) {
  bool parsed_catch = false;
  bool parsed_finally = false;

  if (this->peek().type == token_type::kw_catch) {
    parsed_catch = true;
    this->skip();

    v.visit_enter_block_scope();
    if (this->peek().type == token_type::left_paren) {
      source_code_span catch_left_paren_span = this->peek().span();
      this->skip();

      switch (this->peek().type) {
      case token_type::kw_await:
        if (this->in_async_function_) {
          this->diag_reporter_->report(
              diag_cannot_declare_await_in_async_function{
                  .name = this->peek().identifier_name(),
              });
        }
        goto catch_identifier;

      case token_type::kw_yield:
        if (this->in_generator_function_) {
          this->diag_reporter_->report(
              diag_cannot_declare_yield_in_generator_function{
                  .name = this->peek().identifier_name(),
              });
        }
        goto catch_identifier;

      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
        // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
        goto catch_identifier;

      catch_identifier:
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case token_type::identifier:
        v.visit_variable_declaration(this->peek().identifier_name(),
                                     variable_kind::_catch,
                                     variable_init_kind::normal);
        this->skip();
        break;

      case token_type::left_curly:
      case token_type::left_square: {
        expression *ast = this->parse_expression(
            v, precedence{.commas = false, .in_operator = false});
        this->visit_binding_element(ast, v, variable_kind::_catch,
                                    /*declaring_token=*/std::nullopt,
                                    /*init_kind=*/variable_init_kind::normal);
        break;
      }

      case token_type::right_paren:
        this->diag_reporter_->report(
            diag_missing_catch_variable_between_parentheses{
                .left_paren_to_right_paren = source_code_span(
                    catch_left_paren_span.begin(), this->peek().end),
                .left_paren = catch_left_paren_span,
                .right_paren = this->peek().span(),
            });
        break;

        // catch ("junk") {}
      case token_type::string:
        this->diag_reporter_->report(diag_expected_variable_name_for_catch{
            .unexpected_token = this->peek().span(),
        });
        this->skip();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
      this->skip();
    }

    if (this->peek().type == token_type::left_curly) {
      this->parse_and_visit_statement_block_no_scope(v);
    } else {
      const char8 *here = this->lexer_.end_of_previous_token();
      this->diag_reporter_->report(diag_missing_body_for_catch_clause{
          .catch_token = source_code_span(here, here),
      });
    }
    v.visit_exit_block_scope();
  }

  if (this->peek().type == token_type::kw_finally) {
    parsed_finally = true;
    source_code_span finally_token_span = this->peek().span();
    this->skip();

    if (this->peek().type == token_type::left_curly) {
      v.visit_enter_block_scope();
      this->parse_and_visit_statement_block_no_scope(v);
      v.visit_exit_block_scope();
    } else {
      this->diag_reporter_->report(diag_missing_body_for_finally_clause{
          .finally_token = finally_token_span,
      });
    }
  }

  return parsed_catch || parsed_finally;
}

void parser::parse_and_visit_do_while(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_do);
  source_code_span do_token_span = this->peek().span();
  this->skip();

  switch (this->peek().type) {
  default: {
    this->error_on_class_statement(statement_kind::do_while_loop);
    this->error_on_function_statement(statement_kind::do_while_loop);
    this->error_on_lexical_declaration(statement_kind::do_while_loop);
    bool parsed_statement = this->parse_and_visit_statement(v);
    if (!parsed_statement) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    break;
  }
  case token_type::kw_while:
    this->diag_reporter_->report(diag_missing_body_for_do_while_statement{
        .do_token = do_token_span,
    });
    break;
  }

  if (this->peek().type != token_type::kw_while) {
    const char8 *here = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(
        diag_missing_while_and_condition_for_do_while_statement{
            .do_token = do_token_span,
            .expected_while = source_code_span(here, here),
        });
    return;
  }
  this->skip();

  this->parse_and_visit_parenthesized_expression<
      diag_expected_parentheses_around_do_while_condition,
      diag_expected_parenthesis_around_do_while_condition,
      /*CheckForSketchyConditions=*/true>(v);

  if (this->peek().type == token_type::semicolon) {
    this->skip();
  }
}

void parser::parse_and_visit_for(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_for);
  source_code_span for_token_span = this->peek().span();
  this->skip();

  if (this->peek().type == token_type::kw_await) {
    this->skip();
  }

  if (this->peek().type != token_type::left_paren) {
    this->diag_reporter_->report(diag_missing_for_loop_header{
        .for_token = for_token_span,
    });
    return;
  }
  const char8 *left_paren_token_begin = this->peek().begin;
  this->skip();

  std::optional<expression *> after_expression;
  auto parse_c_style_head_remainder =
      [&](source_code_span first_semicolon_span) {
        if (this->peek().type != token_type::semicolon) {
          expression *ast = this->parse_expression(v);
          this->visit_expression(ast, v, variable_context::rhs);
          this->error_on_sketchy_condition(ast);
        }

        switch (this->peek().type) {
          // for (init; cond; update) {}
        semicolon:
        case token_type::semicolon:
          this->skip();
          if (this->peek().type != token_type::right_paren) {
            after_expression = this->parse_expression(v);
          }
          break;

          // for (init; cond update) {}  // Invalid.
        case token_type::identifier:
        default:
          this->lexer_.insert_semicolon();
          this->diag_reporter_->report(
              diag_missing_semicolon_between_for_loop_condition_and_update{
                  .expected_semicolon = this->peek().span(),
              });
          goto semicolon;

          // for (init; cond) {}  // Invalid.
        case token_type::right_paren:
          this->diag_reporter_->report(
              diag_c_style_for_loop_is_missing_third_component{
                  .expected_last_component = this->peek().span(),
                  .existing_semicolon = first_semicolon_span,
              });
          break;
        }
      };

  bool entered_for_scope = false;
  enum class loop_style {
    c_style,
    for_in,
    for_of,
    other,
  };
  loop_style for_loop_style;

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wshadow-local")
  auto parse_in_or_of_or_condition_update =
      [&, this](auto &v, expression *init_expression) -> void {
    QLJS_WARNING_POP
    switch (this->peek().type) {
      // for (init; condition; update) {}
    semicolon:
    case token_type::semicolon: {
      source_code_span first_semicolon_span = this->peek().span();
      this->skip();
      this->visit_expression(init_expression, v, variable_context::rhs);
      for_loop_style = loop_style::c_style;
      parse_c_style_head_remainder(first_semicolon_span);
      break;
    }

      // for (lhs rhs) {}                 // Invalid.
      // for (init condition; update) {}  // Invalid.
    case token_type::identifier:
    default:
      this->lexer_.insert_semicolon();
      this->diag_reporter_->report(
          diag_missing_semicolon_between_for_loop_init_and_condition{
              .expected_semicolon = this->peek().span(),
          });
      goto semicolon;

      // for (lhs in rhs) {}
      // for (lhs in rhs; condition; update) {}  // Invalid.
    case token_type::kw_in: {
      source_code_span in_token_span = this->peek().span();
      this->skip();

      expression *rhs = this->parse_expression(v);
      this->visit_assignment_expression(init_expression, rhs, v);

      if (this->peek().type == token_type::semicolon) {
        this->diag_reporter_->report(diag_in_disallowed_in_c_style_for_loop{
            .in_token = in_token_span,
        });
        source_code_span first_semicolon_span = this->peek().span();
        this->skip();
        for_loop_style = loop_style::for_in;
        parse_c_style_head_remainder(first_semicolon_span);
      }
      break;
    }

      // for (lhs of rhs) {}
    case token_type::kw_of: {
      this->skip();
      expression *rhs = this->parse_expression(v);
      this->visit_assignment_expression(init_expression, rhs, v);
      for_loop_style = loop_style::for_of;
      break;
    }

      // for (expression) {}    // Invalid.
    case token_type::right_paren:
      this->diag_reporter_->report(
          diag_missing_for_loop_rhs_or_components_after_expression{
              .header =
                  source_code_span(left_paren_token_begin, this->peek().end),
              .for_token = for_token_span,
          });
      this->visit_expression(init_expression, v, variable_context::rhs);
      for_loop_style = loop_style::c_style;
      break;
    }
  };
  switch (this->peek().type) {
    // for (;;) {}
  case token_type::semicolon: {
    source_code_span first_semicolon_span = this->peek().span();
    this->skip();
    for_loop_style = loop_style::c_style;
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
    buffering_visitor &lhs = this->buffering_visitor_stack_.emplace(
        boost::container::pmr::new_delete_resource());
    if (declaring_token.type == token_type::kw_let &&
        this->is_let_token_a_variable_reference(this->peek(),
                                                /*allow_declarations=*/true)) {
      // for (let = expression; cond; up) {}
      // for (let(); cond; up) {}
      // for (let; cond; up) {}
      // for (let in myArray) {}
      this->lexer_.roll_back_transaction(std::move(transaction));
      expression *ast =
          this->parse_expression(v, precedence{.in_operator = false});
      this->visit_expression(ast, lhs, variable_context::lhs);
      this->maybe_visit_assignment(ast, lhs);
    } else if (declaring_token.type == token_type::kw_let &&
               this->peek().type == token_type::kw_of) {
      this->skip();
      switch (this->peek().type) {
        // for (let of xs) {}  // Invalid.
      case token_type::identifier:
        this->lexer_.roll_back_transaction(std::move(transaction));
        this->skip();  // Re-parse 'let'.
        this->diag_reporter_->report(diag_let_with_no_bindings{
            .where = declaring_token.span(),
        });
        break;

        // for (let of of xs) {}
        // for (let of in xs) {}
        // for (let of = 3; cond; update) {}
        // for (let of; cond; update) {}
        // for (let of, x; cond; update) {}
      default:
        this->lexer_.roll_back_transaction(std::move(transaction));
        this->skip();  // Re-parse 'let'.
        this->parse_and_visit_let_bindings(
            lhs, declaring_token,
            /*allow_in_operator=*/false,
            /*allow_const_without_initializer=*/false,
            /*is_in_for_initializer=*/true);
        break;
      }
    } else {
      // for (let i = 0; i < length; ++length) {}
      // for (let x of xs) {}
      this->lexer_.commit_transaction(std::move(transaction));
      this->parse_and_visit_let_bindings(
          lhs, declaring_token,
          /*allow_in_operator=*/false,
          /*allow_const_without_initializer=*/true,
          /*is_in_for_initializer=*/true);
    }
    switch (this->peek().type) {
      // for (let i = 0; i < length; ++length) {}
    case token_type::semicolon: {
      source_code_span first_semicolon_span = this->peek().span();
      this->skip();
      lhs.move_into(v);
      for_loop_style = loop_style::c_style;
      parse_c_style_head_remainder(first_semicolon_span);
      break;
    }

      // for (let x of xs) {}
    case token_type::kw_in:
    case token_type::kw_of: {
      for_loop_style = this->peek().type == token_type::kw_in
                           ? loop_style::for_in
                           : loop_style::for_of;
      bool is_var_in = declaring_token.type == token_type::kw_var &&
                       for_loop_style == loop_style::for_in;
      this->skip();
      expression *rhs = this->parse_expression(v);
      if (is_var_in) {
        // In the following code, 'init' is evaluated before 'array':
        //
        //   for (var x = init in array) {}
        lhs.move_into(v);
      }
      this->visit_expression(rhs, v, variable_context::rhs);
      if (!is_var_in) {
        // In the following code, 'array' is evaluated before 'x' is declared:
        //
        //   for (let x in array) {}
        lhs.move_into(v);
      }
      break;
    }

      // for (let myVariable) {}    // Invalid.
    case token_type::right_paren:
      this->diag_reporter_->report(
          diag_missing_for_loop_rhs_or_components_after_declaration{
              .header =
                  source_code_span(left_paren_token_begin, this->peek().end),
              .for_token = for_token_span,
          });
      lhs.move_into(v);
      for_loop_style = loop_style::for_of;
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    QLJS_ASSERT(&this->buffering_visitor_stack_.top() == &lhs);
    this->buffering_visitor_stack_.pop();

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
      this->diag_reporter_->report(
          diag_cannot_assign_to_variable_named_async_in_for_of_loop{
              .async_identifier = async_token.identifier_name(),
          });

      this->skip();
      QLJS_ASSERT(this->peek().type == token_type::kw_of);
      init_expression = this->make_expression<expression::variable>(
          async_token.identifier_name(), async_token.type);
    } else {
      init_expression =
          this->parse_expression(v, precedence{.in_operator = false});
    }
    parse_in_or_of_or_condition_update(v, init_expression);
    break;
  }

    // for (init; condition; update) {}
    // for (item of things) {}
    // for (item in things) {}
  default: {
    expression *init_expression =
        this->parse_expression(v, precedence{.in_operator = false});
    parse_in_or_of_or_condition_update(v, init_expression);
    break;
  }

    // for () {}  // Invalid.
  case token_type::right_paren:
    this->diag_reporter_->report(diag_missing_header_of_for_loop{
        .where = source_code_span(left_paren_token_begin, this->peek().end),
    });
    for_loop_style = loop_style::other;
    break;
  }

  // for (;;;) {}  // Invalid.
  // for (x of y; z) {}  // Invalid.
  while (this->peek().type == token_type::semicolon) {
    switch (for_loop_style) {
    case loop_style::c_style:
    case loop_style::other:
      this->diag_reporter_->report(
          diag_unexpected_semicolon_in_c_style_for_loop{
              .semicolon = this->peek().span(),
          });
      break;
    case loop_style::for_in:
      this->diag_reporter_->report(diag_unexpected_semicolon_in_for_in_loop{
          .semicolon = this->peek().span(),
      });
      break;
    case loop_style::for_of:
      this->diag_reporter_->report(diag_unexpected_semicolon_in_for_of_loop{
          .semicolon = this->peek().span(),
      });
      break;
    }
    this->skip();
    switch (this->peek().type) {
    case token_type::semicolon:
    case token_type::right_paren:
      break;
    default:
      this->parse_and_visit_expression(v);
      break;
    }
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
  this->skip();

  this->error_on_class_statement(statement_kind::for_loop);
  this->error_on_function_statement(statement_kind::for_loop);
  this->error_on_lexical_declaration(statement_kind::for_loop);
  bool parsed_body =
      this->parse_and_visit_statement(v, parse_statement_type::no_declarations);
  if (!parsed_body) {
    const char8 *here = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_missing_body_for_for_statement{
        .for_and_header = source_code_span(here, here),
    });
  }

  if (after_expression.has_value()) {
    this->visit_expression(*after_expression, v, variable_context::rhs);
  }
  if (entered_for_scope) {
    v.visit_exit_for_scope();
  }
}

void parser::parse_and_visit_while(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_while);
  source_code_span while_token_span = this->peek().span();
  this->skip();

  if (this->peek().type == token_type::left_curly) {
    // while { body; }  // Invalid.
    this->diag_reporter_->report(diag_missing_condition_for_while_statement{
        .while_keyword = while_token_span,
    });
  } else {
    this->parse_and_visit_parenthesized_expression<
        diag_expected_parentheses_around_while_condition,
        diag_expected_parenthesis_around_while_condition,
        /*CheckForSketchyConditions=*/true>(v);
  }

  this->error_on_class_statement(statement_kind::while_loop);
  this->error_on_function_statement(statement_kind::while_loop);
  this->error_on_lexical_declaration(statement_kind::while_loop);
  bool parsed_body =
      this->parse_and_visit_statement(v, parse_statement_type::no_declarations);
  if (!parsed_body) {
    const char8 *here = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_missing_body_for_while_statement{
        .while_and_condition = source_code_span(here, here),
    });
  }
}

void parser::parse_and_visit_with(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_with);
  this->skip();

  this->parse_and_visit_parenthesized_expression<
      diag_expected_parentheses_around_with_expression,
      diag_expected_parenthesis_around_with_expression,
      /*CheckForSketchyConditions=*/false>(v);

  this->error_on_class_statement(statement_kind::with_statement);
  this->error_on_function_statement(statement_kind::with_statement);
  this->error_on_lexical_declaration(statement_kind::with_statement);

  v.visit_enter_with_scope();
  bool parsed_body =
      this->parse_and_visit_statement(v, parse_statement_type::no_declarations);
  if (!parsed_body) {
    QLJS_PARSER_UNIMPLEMENTED();
  }
  v.visit_exit_with_scope();
}

void parser::parse_and_visit_if(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_if);
  source_code_span if_token_span = this->peek().span();
  this->skip();

  if (this->peek().type == token_type::left_curly) {
    // if { body; }  // Invalid.
    this->diag_reporter_->report(diag_missing_condition_for_if_statement{
        .if_keyword = if_token_span,
    });
  } else {
    this->parse_and_visit_parenthesized_expression<
        diag_expected_parentheses_around_if_condition,
        diag_expected_parenthesis_around_if_condition,
        /*CheckForSketchyConditions=*/true>(v);
  }

  auto parse_and_visit_body = [this, &v]() -> void {
    bool entered_block_scope = false;

    this->error_on_class_statement(statement_kind::if_statement);
    this->error_on_lexical_declaration(statement_kind::if_statement);
    if (this->is_maybe_function_statement()) {
      v.visit_enter_block_scope();
      entered_block_scope = true;
    }

    bool parsed_if_body = this->parse_and_visit_statement(
        v, parse_statement_type::no_declarations);
    if (!parsed_if_body) {
      QLJS_PARSER_UNIMPLEMENTED();
    }

    if (entered_block_scope) {
      v.visit_exit_block_scope();
    }
  };

  switch (this->peek().type) {
  default:
    parse_and_visit_body();
    break;

  case token_type::end_of_file:
  case token_type::kw_else:
  case token_type::right_curly:
    const char8 *end_of_if_condition = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_missing_body_for_if_statement{
        .expected_body =
            source_code_span(end_of_if_condition, end_of_if_condition),
    });
    break;
  }

parse_maybe_else:
  if (this->peek().type == token_type::kw_else) {
    this->skip();
    const char8 *end_of_else = this->lexer_.end_of_previous_token();
    bool has_left_paren = this->peek().type == token_type::left_paren;
    if (has_left_paren) {
      this->parse_and_visit_expression(
          v, precedence{
                 .trailing_curly_is_arrow_body = false,
             });
    } else {
      parse_and_visit_body();
    }
    bool has_left_curly = this->peek().type == token_type::left_curly;
    if (!this->peek().has_leading_newline && has_left_paren && has_left_curly) {
      // if (cond) {} else (cond) {} // Invalid
      this->diag_reporter_->report(diag_missing_if_after_else{
          .expected_if = source_code_span(end_of_else, end_of_else),
      });
      parse_and_visit_body();
      goto parse_maybe_else;
    }
  }
}

void parser::parse_and_visit_import(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_import);
  source_code_span import_span = this->peek().span();
  this->skip();

  switch (this->peek().type) {
    // import var from "module";  // Invalid.
  QLJS_CASE_STRICT_RESERVED_KEYWORD:
    this->diag_reporter_->report(diag_cannot_import_variable_named_keyword{
        .import_name = this->peek().identifier_name(),
    });
    goto identifier;

    // import \u{76}ar from "module";  // Invalid.
  case token_type::reserved_keyword_with_escape_sequence:
    this->lexer_.peek().report_errors_for_escape_sequences_in_keyword(
        this->diag_reporter_);
    goto identifier;

    // import let from "module";
    // import fs from "fs";
  identifier:
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
    if (this->peek().type == token_type::kw_let) {
      this->diag_reporter_->report(
          diag_cannot_import_let{.import_name = this->peek().span()});
    }
    v.visit_variable_declaration(this->peek().identifier_name(),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    this->skip();
    if (this->peek().type == token_type::comma) {
      this->skip();
      switch (this->peek().type) {
        // import fs, {readFile} from "fs";
      case token_type::left_curly:
        this->parse_and_visit_named_exports_for_import(v);
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
    this->parse_and_visit_named_exports_for_import(v);
    break;

    // import expression statement:
    //
    // import(url).then(() => { /* ... */ })
    // import.meta
  case token_type::dot:
  case token_type::left_paren: {
    expression *ast = this->parse_expression_remainder(
        v, this->make_expression<expression::import>(import_span),
        precedence{});
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

  switch (this->peek().type) {
  case token_type::kw_from:
    this->skip();
    break;

  case token_type::string:
    this->diag_reporter_->report(diag_expected_from_before_module_specifier{
        .module_specifier = this->peek().span(),
    });
    break;

  default: {
    const char8 *where = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_expected_from_and_module_specifier{
        .where = source_code_span(where, where),
    });
    return;
  }
  }

  if (this->peek().type != token_type::string) {
    QLJS_PARSER_UNIMPLEMENTED();
  }
  this->skip();

  if (this->peek().type == token_type::semicolon) {
    this->skip();
  }
}

void parser::parse_and_visit_name_space_import(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::star);
  source_code_span star_span = this->peek().span();
  this->skip();

  switch (this->peek().type) {
  case token_type::kw_as:
    this->skip();
    break;

  case token_type::identifier:
    this->diag_reporter_->report(
        diag_expected_as_before_imported_namespace_alias{
            .star_through_alias_token =
                source_code_span(star_span.begin(), this->peek().end),
            .alias = this->peek().span(),
            .star_token = star_span,
        });
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  switch (this->peek().type) {
    // import * as var from "module";  // Invalid.
  QLJS_CASE_STRICT_RESERVED_KEYWORD:
    this->diag_reporter_->report(diag_cannot_import_variable_named_keyword{
        .import_name = this->peek().identifier_name(),
    });
    goto identifier;

    // import * as \u{76}ar from "module";  // Invalid.
  case token_type::reserved_keyword_with_escape_sequence:
    this->lexer_.peek().report_errors_for_escape_sequences_in_keyword(
        this->diag_reporter_);
    goto identifier;

  identifier:
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
    if (this->peek().type == token_type::kw_let) {
      this->diag_reporter_->report(diag_cannot_import_let{
          .import_name = this->peek().identifier_name().span()});
    }
    v.visit_variable_declaration(this->peek().identifier_name(),
                                 variable_kind::_import,
                                 variable_init_kind::normal);
    this->skip();
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

void parser::parse_and_visit_named_exports_for_export(
    parse_visitor_base &v,
    bump_vector<token, monotonic_allocator> &out_exported_bad_tokens) {
  this->parse_and_visit_named_exports(
      v, /*out_exported_bad_tokens=*/&out_exported_bad_tokens);
}

void parser::parse_and_visit_named_exports_for_import(parse_visitor_base &v) {
  this->parse_and_visit_named_exports(v, /*out_exported_bad_tokens=*/nullptr);
}

void parser::parse_and_visit_named_exports(
    parse_visitor_base &v,
    bump_vector<token, monotonic_allocator> *out_exported_bad_tokens) {
  bool is_export = out_exported_bad_tokens != nullptr;
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  this->skip();
  for (;;) {
    bool left_is_keyword = false;
    switch (this->peek().type) {
    QLJS_CASE_STRICT_RESERVED_KEYWORD:
    case token_type::reserved_keyword_with_escape_sequence:
      if (out_exported_bad_tokens) {
        out_exported_bad_tokens->emplace_back(this->peek());
      }
      left_is_keyword = true;
      [[fallthrough]];
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    case token_type::identifier: {
      identifier left_name = this->peek().identifier_name();
      token right_token = this->peek();
      this->skip();
      bool has_as = this->peek().type == token_type::kw_as;
      if (has_as) {
        this->skip();
        switch (this->peek().type) {
        case token_type::string:
          // TODO(strager): Check that the string is valid Unicode
          // (standard: IsStringWellFormedUnicode).
          [[fallthrough]];
        QLJS_CASE_KEYWORD:
        case token_type::identifier:
        case token_type::reserved_keyword_with_escape_sequence:
          right_token = this->peek();
          this->skip();
          break;
        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
      }
      if (is_export) {
        if (left_is_keyword) {
          // Ignore. We will emit diag_cannot_export_variable_named_keyword
          // later.
        } else {
          v.visit_variable_export_use(left_name);
        }
      } else {
        switch (right_token.type) {
          // import {myFunc} from 'other';
          // import {myFunc as let} from 'other';  // Invalid.
          // import {myFunc as static} from 'other';
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        case token_type::identifier:
          if (right_token.type == token_type::kw_let) {
            this->diag_reporter_->report(
                diag_cannot_import_let{.import_name = right_token.span()});
          }
          v.visit_variable_declaration(right_token.identifier_name(),
                                       variable_kind::_import,
                                       variable_init_kind::normal);
          break;

          // import {var} from 'other';  // Invalid.
        QLJS_CASE_STRICT_RESERVED_KEYWORD:
          this->diag_reporter_->report(
              diag_cannot_import_variable_named_keyword{
                  .import_name = right_token.identifier_name(),
              });
          // FIXME(strager): Declaring a variable with a keyword name is
          // sketchy. Delete this?
          v.visit_variable_declaration(right_token.identifier_name(),
                                       variable_kind::_import,
                                       variable_init_kind::normal);
          break;

          // import {\u{76}ar} from 'other';  // Invalid.
        case token_type::reserved_keyword_with_escape_sequence:
          right_token.report_errors_for_escape_sequences_in_keyword(
              this->diag_reporter_);
          // FIXME(strager): Declaring a variable with a keyword name is
          // sketchy. Delete this?
          v.visit_variable_declaration(right_token.identifier_name(),
                                       variable_kind::_import,
                                       variable_init_kind::normal);
          break;

        case token_type::string:
          QLJS_ASSERT(has_as);
          this->diag_reporter_->report(
              diag_expected_variable_name_for_import_as{
                  .unexpected_token = right_token.span(),
              });
          break;

        default:
          QLJS_UNIMPLEMENTED();
          break;
        }
      }
      break;
    }

      // import {"export name" as varName} from "other";
      // export {"export name"} from "other";
    case token_type::string:
      // TODO(strager): Check that the string is valid Unicode
      // (standard: IsStringWellFormedUnicode).
      if (is_export) {
        if (out_exported_bad_tokens) {
          out_exported_bad_tokens->emplace_back(this->peek());
        }
        this->skip();
      } else {
        this->skip();

        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::kw_as);
        this->skip();

        switch (this->peek().type) {
          // import {'name' as bread} from 'other';
          // import {'name' as let} from 'other';  // Invalid.
          // import {'name' as static} from 'other';
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        case token_type::identifier:
          if (this->peek().type == token_type::kw_let) {
            this->diag_reporter_->report(
                diag_cannot_import_let{.import_name = this->peek().span()});
          }
          v.visit_variable_declaration(this->peek().identifier_name(),
                                       variable_kind::_import,
                                       variable_init_kind::normal);
          this->skip();
          break;

          // import {'name' as debugger} from 'other';  // Invalid.
        QLJS_CASE_STRICT_RESERVED_KEYWORD:
          this->diag_reporter_->report(
              diag_cannot_import_variable_named_keyword{
                  .import_name = this->peek().identifier_name(),
              });
          v.visit_variable_declaration(this->peek().identifier_name(),
                                       variable_kind::_import,
                                       variable_init_kind::normal);
          this->skip();
          break;

          // import {\u{76}ar} from 'other';  // Invalid.
        case token_type::reserved_keyword_with_escape_sequence:
          this->peek().report_errors_for_escape_sequences_in_keyword(
              this->diag_reporter_);
          v.visit_variable_declaration(this->peek().identifier_name(),
                                       variable_kind::_import,
                                       variable_init_kind::normal);
          this->skip();
          break;

        case token_type::string:
          this->diag_reporter_->report(
              diag_expected_variable_name_for_import_as{
                  .unexpected_token = this->peek().span(),
              });
          this->skip();
          break;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
      }
      break;

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

void parser::parse_and_visit_variable_declaration_statement(
    parse_visitor_base &v) {
  token declaring_token = this->peek();
  QLJS_ASSERT(declaring_token.type == token_type::kw_const ||
              declaring_token.type == token_type::kw_let ||
              declaring_token.type == token_type::kw_var);
  this->skip();
  this->parse_and_visit_let_bindings(v, declaring_token,
                                     /*allow_in_operator=*/true);
  this->consume_semicolon();
}

void parser::parse_and_visit_let_bindings(parse_visitor_base &v,
                                          const token &declaring_token,
                                          bool allow_in_operator,
                                          bool allow_const_without_initializer,
                                          bool is_in_for_initializer) {
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
  this->parse_and_visit_let_bindings(
      v, declaring_token, declaration_kind,
      /*allow_in_operator=*/allow_in_operator,
      /*allow_const_without_initializer=*/allow_const_without_initializer,
      /*is_in_for_initializer=*/is_in_for_initializer);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
void parser::parse_and_visit_let_bindings(parse_visitor_base &v,
                                          const token &declaring_token,
                                          variable_kind declaration_kind,
                                          bool allow_in_operator,
                                          bool allow_const_without_initializer,
                                          bool is_in_for_initializer) {
  source_code_span let_span = declaring_token.span();
  bool first_binding = true;
  for (;;) {
    std::optional<source_code_span> comma_span = std::nullopt;
    if (!first_binding) {
      switch (this->peek().type) {
      case token_type::comma:
        comma_span = this->peek().span();
        this->skip();
        break;

      case token_type::identifier:
      case token_type::left_curly:
      case token_type::left_square:
        if (this->peek().has_leading_newline) {
          // Caller will insert our semicolon if needed.
          return;
        } else {
          // let x y
          const char8 *here = this->lexer_.end_of_previous_token();
          this->diag_reporter_->report(
              diag_missing_comma_between_variable_declarations{
                  .expected_comma = source_code_span(here, here),
              });
        }
        break;

      default:
        // Caller will insert our semicolon if needed.
        return;
      }
    }

    switch (this->peek().type) {
    case token_type::kw_await:
      if (this->in_async_function_) {
        this->diag_reporter_->report(
            diag_cannot_declare_await_in_async_function{
                .name = this->peek().identifier_name(),
            });
      }
      goto variable_name;

    case token_type::kw_yield:
      if (this->in_generator_function_) {
        this->diag_reporter_->report(
            diag_cannot_declare_yield_in_generator_function{
                .name = this->peek().identifier_name(),
            });
      }
      goto variable_name;

    // let protected = 42;
    QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
      goto variable_name;

      // let x;
      // let x = 42;
    variable_name:
    case token_type::identifier:
    case token_type::kw_as:
    case token_type::kw_async:
    case token_type::kw_from:
    case token_type::kw_get:
    case token_type::kw_let:
    case token_type::kw_of:
    case token_type::kw_set:
    case token_type::kw_static: {
      expression *variable = this->make_expression<expression::variable>(
          this->peek().identifier_name(), this->peek().type);
      this->skip();
      switch (this->peek().type) {
        // let x = 3;
        // let x += 42;  // Invalid.
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      case token_type::equal: {
        token equal_token = this->peek();
        auto *assignment_ast = static_cast<expression::assignment *>(
            this->parse_expression_remainder(
                v, variable,
                precedence{.commas = false, .in_operator = allow_in_operator}));

        if (is_in_for_initializer && this->peek().type == token_type::kw_in) {
          // for (var x = "initial" in obj)
          // for (let x = "prop" in obj)  // Invalid.
          // for (let x = "prop" in obj; i < 10; ++i)  // Invalid.
          source_code_span in_token_span = this->peek().span();
          QLJS_ASSERT(!allow_in_operator);

          parser_transaction transaction = this->begin_transaction();
          expression *in_ast = this->parse_expression_remainder(
              v, assignment_ast->child_1(), precedence{.commas = false});
          if (this->peek().type == token_type::semicolon) {
            // for (let x = "prop" in obj; i < 10; ++i)  // Invalid.
            this->commit_transaction(std::move(transaction));
            assignment_ast->children_[1] = in_ast;
            this->diag_reporter_->report(diag_in_disallowed_in_c_style_for_loop{
                .in_token = in_token_span,
            });
          } else {
            this->roll_back_transaction(std::move(transaction));
            if (declaration_kind == variable_kind::_var) {
              // for (var x = "initial" in obj)
            } else {
              // for (let x = "prop" in obj)  // Invalid.
              this->diag_reporter_->report(
                  diag_cannot_assign_to_loop_variable_in_for_of_or_in_loop{
                      .equal_token = equal_token.span()});
            }
          }
        } else if (is_in_for_initializer &&
                   this->peek().type == token_type::kw_of) {
          // for (var x = "initial" of obj)  // Invalid.
          this->diag_reporter_->report(
              diag_cannot_assign_to_loop_variable_in_for_of_or_in_loop{
                  .equal_token = equal_token.span()});
        }

        this->visit_binding_element(
            assignment_ast, v, declaration_kind,
            /*declaring_token=*/declaring_token.span(),
            /*init_kind=*/variable_init_kind::initialized_with_equals);
        break;
      }

      case token_type::kw_await:
      case token_type::kw_class:
      case token_type::kw_function:
      case token_type::kw_new:
      case token_type::kw_null:
      case token_type::kw_this:
      case token_type::kw_typeof: {
        if (this->peek().has_leading_newline) {
          // let x  // ASI
          // null;
          this->visit_binding_element(
              variable, v, declaration_kind,
              /*declaring_token=*/declaring_token.span(),
              /*init_kind=*/variable_init_kind::normal);
          this->lexer_.insert_semicolon();
          return;
        }
        // let x null;  // ERROR
        const char8 *here = this->lexer_.end_of_previous_token();
        this->diag_reporter_->report(diag_missing_equal_after_variable{
            .expected_equal = source_code_span(here, here),
        });
        this->parse_and_visit_expression(
            v, precedence{.commas = false, .in_operator = allow_in_operator});
        // TODO(strager): Would variable_init_kind::initialized_with_equals make
        // more sense?
        this->visit_binding_element(variable, v, declaration_kind,
                                    /*declaring_token=*/declaring_token.span(),
                                    /*init_kind=*/variable_init_kind::normal);
        break;
      }

        // let x;
        // let x, y;
      default:
        if (declaration_kind == variable_kind::_const) {
          if (!allow_const_without_initializer) {
            this->diag_reporter_->report(
                diag_missing_initializer_in_const_declaration{
                    .variable_name = variable->span()});
          }
        }
        this->visit_binding_element(variable, v, declaration_kind,
                                    /*declaring_token=*/declaring_token.span(),
                                    /*init_kind=*/variable_init_kind::normal);
        break;
      }
      break;
    }

      // \u{69}\u{66} // 'if', but escaped.
    case token_type::reserved_keyword_with_escape_sequence:
      this->lexer_.peek().report_errors_for_escape_sequences_in_keyword(
          this->diag_reporter_);
      goto variable_name;

      // let {x} = xs;
      // let [head, ...tail] = xs;
      // for (let {prop} of xs) {}
    case token_type::left_curly:
    case token_type::left_square: {
      expression *ast = this->parse_expression(
          v, precedence{.commas = false, .in_operator = allow_in_operator});
      // TODO(strager): Report error if initializer is missing.
      this->visit_binding_element(ast, v, declaration_kind,
                                  /*declaring_token=*/declaring_token.span(),
                                  /*init_kind=*/variable_init_kind::normal);
      break;
    }

      // let switch = 3;  // Invalid.
      // let if (x) {}    // Invalid.
    QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD : {
      source_code_span keyword_span = this->peek().span();
      lexer_transaction transaction = this->lexer_.begin_transaction();
      this->skip();

      switch (this->peek().type) {
        // let switch = 3;  // Invalid.
      case token_type::end_of_file:
      case token_type::equal:
      case token_type::semicolon:
        this->lexer_.commit_transaction(std::move(transaction));
        this->diag_reporter_->report(
            diag_cannot_declare_variable_with_keyword_name{
                .keyword = keyword_span,
            });
        this->skip();
        this->parse_and_visit_expression(
            v, precedence{.commas = false, .in_operator = allow_in_operator});
        break;

        // let if (x) {}    // Invalid.
      default:
        this->lexer_.roll_back_transaction(std::move(transaction));
        if (this->peek().has_leading_newline) {
          this->diag_reporter_->report(diag_let_with_no_bindings{let_span});
        } else {
          this->diag_reporter_->report(
              diag_unexpected_token_in_variable_declaration{keyword_span});
          this->lexer_.insert_semicolon();
        }
        break;
      }
      break;
    }

      // let 42;  // Invalid.
    case token_type::complete_template:
    case token_type::number:
      this->diag_reporter_->report(
          diag_unexpected_token_in_variable_declaration{
              .unexpected_token = this->peek().span(),
          });
      this->lexer_.insert_semicolon();
      break;

      // let v, `hello${world}`;  // Invalid.
    case token_type::incomplete_template:
      // TODO(strager): Improve the span.
      this->diag_reporter_->report(
          diag_unexpected_token_in_variable_declaration{
              .unexpected_token = this->peek().span(),
          });
      this->lexer_.insert_semicolon();
      break;

    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
    case token_type::comma:
    case token_type::dot:
    case token_type::equal_greater:
    case token_type::left_paren:
    case token_type::minus:
    case token_type::plus:
    case token_type::question:
    case token_type::slash:
      QLJS_PARSER_UNIMPLEMENTED();
      break;

    case token_type::equal:
      this->diag_reporter_->report(diag_missing_variable_name_in_declaration{
          .equal_token = this->peek().span(),
      });
      this->skip();
      this->parse_and_visit_expression(
          v, precedence{.commas = false, .in_operator = allow_in_operator});
      break;

    case token_type::semicolon:
    default:
      if (first_binding) {
        this->diag_reporter_->report(diag_let_with_no_bindings{let_span});
      } else {
        QLJS_ASSERT(comma_span.has_value());
        this->diag_reporter_->report(
            diag_stray_comma_in_let_statement{*comma_span});
      }
      break;
    }
    first_binding = false;
  }
}
QLJS_WARNING_POP

void parser::visit_binding_element(
    expression *ast, parse_visitor_base &v, variable_kind declaration_kind,
    std::optional<source_code_span> declaring_token,
    variable_init_kind init_kind) {
  switch (declaration_kind) {
  case variable_kind::_const:
  case variable_kind::_let:
  case variable_kind::_var:
    break;
  default:
    QLJS_ASSERT(init_kind == variable_init_kind::normal);
    break;
  }

  auto visit_variable_declaration = [&](const identifier &ident) -> void {
    v.visit_variable_declaration(ident, declaration_kind, init_kind);
  };

  switch (ast->kind()) {
  case expression_kind::array:
    for (expression *item : ast->children()) {
      this->visit_binding_element(item, v, declaration_kind,
                                  /*declaring_token=*/declaring_token,
                                  /*init_kind=*/init_kind);
    }
    break;

  case expression_kind::compound_assignment:
    if (declaring_token.has_value()) {
      auto *assignment = static_cast<expression::assignment *>(ast);
      this->diag_reporter_->report(
          diag_cannot_update_variable_during_declaration{
              .declaring_token = *declaring_token,
              .updating_operator = assignment->operator_span_,
          });
    } else {
      this->diag_reporter_->report(diag_invalid_parameter{
          .parameter = ast->span(),
      });
    }
    [[fallthrough]];
  case expression_kind::assignment: {
    this->visit_expression(ast->child_1(), v, variable_context::rhs);
    variable_init_kind lhs_init_kind;
    switch (declaration_kind) {
    case variable_kind::_const:
    case variable_kind::_let:
    case variable_kind::_var:
      lhs_init_kind = variable_init_kind::initialized_with_equals;
      break;
    default:
      lhs_init_kind = variable_init_kind::normal;
      break;
    }
    this->visit_binding_element(ast->child_0(), v, declaration_kind,
                                /*declaring_token=*/declaring_token,
                                /*init_kind=*/lhs_init_kind);
    break;
  }

  case expression_kind::variable: {
    identifier ident = ast->variable_identifier();
    if ((declaration_kind == variable_kind::_const ||
         declaration_kind == variable_kind::_import ||
         declaration_kind == variable_kind::_let) &&
        ast->variable_identifier_token_type() == token_type::kw_let) {
      // If this is an import, we would emit diag_cannot_import_let
      // instead.
      QLJS_ASSERT(declaration_kind != variable_kind::_import);
      this->diag_reporter_->report(
          diag_cannot_declare_variable_named_let_with_let{.name =
                                                              ident.span()});
    }
    visit_variable_declaration(ident);
    break;
  }
  case expression_kind::object:
    for (int i = 0; i < ast->object_entry_count(); ++i) {
      expression *value = ast->object_entry(i).value;
      this->visit_binding_element(value, v, declaration_kind,
                                  /*declaring_token=*/declaring_token,
                                  /*init_kind=*/init_kind);
    }
    break;
  case expression_kind::spread:
    this->visit_binding_element(ast->child_0(), v, declaration_kind,
                                /*declaring_token=*/declaring_token,
                                /*init_kind=*/init_kind);
    break;

  case expression_kind::await: {
    auto *await = expression_cast<expression::await>(ast);
    identifier ident(await->unary_operator_span());
    visit_variable_declaration(ident);
    this->diag_reporter_->report(diag_cannot_declare_await_in_async_function{
        .name = ident,
    });
    break;
  }

  case expression_kind::yield_none: {
    identifier ident(ast->span());
    visit_variable_declaration(ident);
    this->diag_reporter_->report(
        diag_cannot_declare_yield_in_generator_function{
            .name = ident,
        });
    break;
  }

  case expression_kind::_class:
  case expression_kind::_delete:
  case expression_kind::_new:
  case expression_kind::_template:
  case expression_kind::_typeof:
  case expression_kind::arrow_function:
  case expression_kind::binary_operator:
  case expression_kind::conditional:
  case expression_kind::conditional_assignment:
  case expression_kind::dot:
  case expression_kind::function:
  case expression_kind::import:
  case expression_kind::index:
  case expression_kind::jsx_element:
  case expression_kind::jsx_element_with_members:
  case expression_kind::jsx_element_with_namespace:
  case expression_kind::jsx_fragment:
  case expression_kind::named_function:
  case expression_kind::new_target:
  case expression_kind::rw_unary_prefix:
  case expression_kind::rw_unary_suffix:
  case expression_kind::super:
  case expression_kind::tagged_template_literal:
  case expression_kind::unary_operator:
  case expression_kind::yield_many:
  case expression_kind::yield_one:
    this->diag_reporter_->report(diag_invalid_parameter{
        .parameter = ast->span(),
    });
    break;

    // function f([(p,)]) {}  // Invalid.
  case expression_kind::trailing_comma:
    this->diag_reporter_->report(diag_stray_comma_in_parameter{
        .comma = static_cast<expression::trailing_comma *>(ast)->comma_span(),
    });
    this->visit_binding_element(ast->child_0(), v, declaration_kind,
                                /*declaring_token=*/declaring_token,
                                /*init_kind=*/init_kind);
    break;

    // function f(#bananas) {}  // Invalid.
    // function f(:) {}  // Invalid.
  case expression_kind::_invalid:
  case expression_kind::_missing:
  case expression_kind::private_variable:
    // parse_expression already reported an error. Don't report another error
    // here.
    break;

  case expression_kind::call:
    this->diag_reporter_->report(diag_invalid_parameter{
        .parameter = ast->span(),
    });
    break;

    // function f([(arg)]) {}  // Invalid.
  case expression_kind::paren:
    // TODO(strager): Report an error.
    this->visit_binding_element(ast->child_0(), v, declaration_kind,
                                /*declaring_token=*/declaring_token,
                                /*init_kind=*/init_kind);
    break;

  case expression_kind::literal:
    this->diag_reporter_->report(diag_unexpected_literal_in_parameter_list{
        .literal = ast->span(),
    });
    break;
  }
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
