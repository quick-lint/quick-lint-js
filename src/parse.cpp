// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-error-reporter.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/warning.h>
#include <utility>

#if QLJS_HAVE_SETJMP
#include <csetjmp>
#endif

// parser is a recursive-descent parser.
//
// The parser class currently does not build an abstract syntax tree (AST) for
// statements. This allows the parser to send partial information to the lexer
// incrementally, enabling single-pass parsing and linting.
//
// The parser class currently builds an AST for expressions. (See expression.h.)
// Therefore, parsing and linting are not truly single-pass. This detail is not
// exposed to the linter, however; the linter does not see the expression ASTs.
//
// Each parser stores a lexer object internally. From the caller's perspective,
// the parser class takes characters as input.

#define QLJS_PARSER_UNIMPLEMENTED() \
  (this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__))

namespace quick_lint_js {
parser::function_guard parser::enter_function(function_attributes attributes) {
  bool was_in_top_level = this->in_top_level_;
  bool was_in_async_function = this->in_async_function_;
  bool was_in_generator_function = this->in_generator_function_;
  bool was_in_loop_statement = this->in_loop_statement_;
  bool was_in_switch_statement = this->in_switch_statement_;
  switch (attributes) {
  case function_attributes::async:
    this->in_async_function_ = true;
    this->in_generator_function_ = false;
    break;
  case function_attributes::async_generator:
    this->in_async_function_ = true;
    this->in_generator_function_ = true;
    break;
  case function_attributes::generator:
    this->in_async_function_ = false;
    this->in_generator_function_ = true;
    break;
  case function_attributes::normal:
    this->in_async_function_ = false;
    this->in_generator_function_ = false;
    break;
  }
  this->in_top_level_ = false;
  this->in_loop_statement_ = false;
  this->in_switch_statement_ = false;
  return function_guard(this, was_in_top_level, was_in_async_function,
                        was_in_generator_function, was_in_loop_statement,
                        was_in_switch_statement);
}

parser::loop_guard parser::enter_loop() {
  return loop_guard(this, std::exchange(this->in_loop_statement_, true));
}

expression* parser::parse_expression(precedence prec) {
  depth_guard guard(this);
  expression* ast = this->parse_primary_expression(prec);
  if (!prec.binary_operators && prec.math_or_logical_or_assignment) {
    return ast;
  }
  return this->parse_expression_remainder(ast, prec);
}

// TODO(strager): Why do we need precedence here? Could we get rid of prec?
expression* parser::parse_primary_expression(precedence prec) {
  switch (this->peek().type) {
  // f  // Variable name.
  identifier:
  case token_type::identifier:
  case token_type::kw_as:
  case token_type::kw_from:
  case token_type::kw_get:
  case token_type::kw_let:
  case token_type::kw_of:
  case token_type::kw_set:
  case token_type::kw_static: {
    expression* ast = this->make_expression<expression::variable>(
        this->peek().identifier_name(), this->peek().type);
    this->skip();
    return ast;
  }

  // \u{69}\u{66} // 'if', but escaped.
  case token_type::reserved_keyword_with_escape_sequence:
    this->lexer_.peek().report_errors_for_escape_sequences_in_keyword(
        this->error_reporter_);
    goto identifier;

  // false
  // `hello`
  // 42
  case token_type::kw_false:
  case token_type::kw_null:
  case token_type::kw_this:
  case token_type::kw_true:
  case token_type::complete_template:
  case token_type::number:
  case token_type::string: {
    expression* ast =
        this->make_expression<expression::literal>(this->peek().span());
    this->skip();
    return ast;
  }

  // import.meta
  case token_type::kw_import: {
    expression* ast =
        this->make_expression<expression::import>(this->peek().span());
    this->skip();
    return ast;
  }

  // super()
  case token_type::kw_super: {
    expression* ast =
        this->make_expression<expression::super>(this->peek().span());
    this->skip();
    return ast;
  }

  // `hello${world}`
  case token_type::incomplete_template: {
    expression* ast = this->parse_template(/*tag=*/std::nullopt);
    return ast;
  }

  // await            // Identifier.
  // await myPromise
  case token_type::kw_await: {
    token await_token = this->peek();
    this->skip();
    return this->parse_await_expression(await_token, prec);
  }

  // yield       // Identifier.
  // yield       // Operator.
  // yield item
  case token_type::kw_yield: {
    if (this->in_generator_function_) {
      // yield is a unary operator.
      source_code_span operator_span = this->peek().span();
      this->skip();
      switch (this->peek().type) {
      case token_type::colon:
      case token_type::end_of_file:
      case token_type::right_curly:
      case token_type::right_paren:
      case token_type::right_square:
      case token_type::semicolon:
        return this->make_expression<expression::yield_none>(operator_span);

      case token_type::comma:
      case token_type::kw_in:
      case token_type::question:
        return this->make_expression<expression::yield_none>(operator_span);

      case token_type::star: {
        this->skip();
        expression* child = this->parse_expression(prec);
        return this->make_expression<expression::yield_many>(child,
                                                             operator_span);
      }

      default: {
        expression* child = this->parse_expression(prec);
        return this->make_expression<expression::yield_one>(child,
                                                            operator_span);
      }
      }
    } else {
      // yield is an identifier.
      goto identifier;
    }
  }

  // ...args  // Spread operator.
  case token_type::dot_dot_dot: {
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* child = this->parse_expression(precedence{.commas = false});
    return this->make_expression<expression::spread>(child, operator_span);
  }

  // !x
  // delete o[key]
  case token_type::bang:
  case token_type::kw_delete:
  case token_type::kw_typeof:
  case token_type::kw_void:
  case token_type::minus:
  case token_type::plus:
  case token_type::tilde: {
    token_type type = this->peek().type;
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* child = this->parse_expression(
        precedence{.binary_operators = true,
                   .math_or_logical_or_assignment = false,
                   .commas = false,
                   .is_typeof = (type == token_type::kw_typeof)});
    if (child->kind() == expression_kind::_invalid) {
      this->error_reporter_->report(error_missing_operand_for_operator{
          .where = operator_span,
      });
    }
    expression* ast =
        type == token_type::kw_typeof
            ? this->make_expression<expression::_typeof>(child, operator_span)
            : this->make_expression<expression::unary_operator>(child,
                                                                operator_span);
    if (type == token_type::kw_delete &&
        child->kind() == expression_kind::variable) {
      this->error_reporter_->report(
          error_redundant_delete_statement_on_variable{
              .delete_expression = ast->span(),
          });
    }
    return ast;
  }

  // --x
  case token_type::minus_minus:
  case token_type::plus_plus: {
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* child = this->parse_expression(
        precedence{.binary_operators = false,
                   .math_or_logical_or_assignment = false,
                   .commas = false});
    if (child->kind() == expression_kind::_invalid) {
      this->error_reporter_->report(error_missing_operand_for_operator{
          .where = operator_span,
      });
    }
    return this->make_expression<expression::rw_unary_prefix>(child,
                                                              operator_span);
  }

  // () => {}     // Arrow function.
  // (x) => {}    // Arrow function.
  // (x + y * z)  // Parenthesized expression.
  case token_type::left_paren: {
    source_code_span left_paren_span = this->peek().span();
    this->skip();

    if (this->peek().type == token_type::right_paren) {
      source_code_span right_paren_span = this->peek().span();
      this->skip();
      if (this->peek().type == token_type::equal_greater) {
        this->skip();
        // Arrow function: () => expression-or-block
        expression* ast = this->parse_arrow_function_body(
            function_attributes::normal, left_paren_span.begin());
        return ast;
      } else {
        // ()  // Invalid.
        this->error_reporter_->report(
            error_missing_expression_between_parentheses{
                .left_paren = left_paren_span,
                .right_paren = right_paren_span,
            });
        expression* child = this->parse_expression();
        return child;
      }
    }

    expression* child =
        this->parse_expression(precedence{.trailing_identifiers = true});
    switch (this->peek().type) {
    case token_type::right_paren:
      this->skip();
      break;
    default:
      this->error_reporter_->report(
          error_unmatched_parenthesis{left_paren_span});
      break;
    }
    return child;
  }

  // async           // Identifer.
  // async () => {}  // Arrow function.
  case token_type::kw_async: {
    token async_token = this->peek();
    this->skip();
    return this->parse_async_expression(async_token, prec);
  }

  // [x, 3, f()]  // Array literal.
  case token_type::left_square: {
    const char8* left_square_begin = this->peek().begin;
    const char8* right_square_end;
    this->skip();

    vector<expression*> children("parse_expression array children",
                                 &this->temporary_memory_);
    for (;;) {
      if (this->peek().type == token_type::right_square) {
        right_square_end = this->peek().end;
        this->skip();
        break;
      }
      // TODO(strager): Require commas between expressions.
      if (this->peek().type == token_type::comma) {
        this->skip();
        continue;
      }
      const char8* child_begin = this->peek().begin;
      expression* child = this->parse_expression(precedence{.commas = false});
      if (this->peek().begin == child_begin) {
        // parse_expression parsed nothing.
        // TODO(strager): Should parse_expression return nullptr if it sees a
        // keyword (instead of returning _invalid and forcing us to check if it
        // parsed anything)?
        const char8* expected_right_square =
            this->lexer_.end_of_previous_token();
        this->error_reporter_->report(error_missing_array_close{
            .left_square =
                source_code_span(left_square_begin, left_square_begin + 1),
            .expected_right_square =
                source_code_span(expected_right_square, expected_right_square),
        });
        right_square_end = expected_right_square;
        break;
      }
      children.emplace_back(child);
    }
    expression* ast = this->make_expression<expression::array>(
        this->expressions_.make_array(std::move(children)),
        source_code_span(left_square_begin, right_square_end));
    return ast;
  }

  // {k: v}  // Object literal.
  case token_type::left_curly: {
    expression* ast = this->parse_object_literal();
    return ast;
  }

  // function() {}  // Function expression.
  case token_type::kw_function: {
    expression* function = this->parse_function_expression(
        function_attributes::normal, this->peek().begin);
    return function;
  }

  // class {}
  case token_type::kw_class: {
    expression* class_expression = this->parse_class_expression();
    return class_expression;
  }

  // new XMLHttpRequest()
  // new.target
  case token_type::kw_new: {
    source_code_span operator_span = this->peek().span();
    this->skip();

    switch (this->peek().type) {
    // new XMLHttpRequest()
    default: {
      expression* target = this->parse_expression(prec);
      vector<expression*> children("parse_expression new children",
                                   &this->temporary_memory_);
      if (target->kind() == expression_kind::call) {
        for (int i = 0; i < target->child_count(); ++i) {
          children.emplace_back(target->child(i));
        }
      } else {
        children.emplace_back(target);
      }
      return this->make_expression<expression::_new>(
          this->expressions_.make_array(std::move(children)),
          source_code_span(operator_span.begin(), target->span().end()));
    }

    // new.target
    case token_type::dot: {
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
      // TODO(strager): Check that the given identifier is 'target'.
      // * Are \u{} escapes allowed?
      source_code_span target_span = this->peek().identifier_name().span();
      this->skip();
      expression* ast = this->make_expression<expression::new_target>(
          source_code_span(operator_span.begin(), target_span.end()));
      return ast;
    }
    }
    QLJS_UNREACHABLE();
  }

  // /regexp/    // RegExp literal.
  // /=regexp/  // RegExp literal.
  case token_type::slash:
  case token_type::slash_equal: {
    this->lexer_.reparse_as_regexp();
    expression* regexp =
        this->make_expression<expression::literal>(this->peek().span());
    this->skip();
    return regexp;
  }

  QLJS_CASE_BINARY_ONLY_OPERATOR:
  case token_type::comma:
  case token_type::dot:
  case token_type::equal:
  case token_type::kw_in:
  case token_type::question: {
    expression* ast =
        this->make_expression<expression::_invalid>(this->peek().span());
    if (prec.binary_operators) {
      this->error_reporter_->report(
          error_missing_operand_for_operator{this->peek().span()});
    }
    return ast;
  }

  // => expr  // Invalid. Treat as arrow function.
  // => {}    // Invalid. Treat as arrow function.
  case token_type::equal_greater: {
    source_code_span arrow_span = this->peek().span();
    this->error_reporter_->report(error_missing_arrow_function_parameter_list{
        .arrow = arrow_span,
    });
    this->skip();

    expression* arrow_function = this->parse_arrow_function_body(
        function_attributes::normal,
        /*parameter_list_begin=*/arrow_span.begin());
    return arrow_function;
  }

  case token_type::private_identifier: {
    this->error_reporter_->report(
        error_cannot_refer_to_private_variable_without_object{
            .private_identifier = this->peek().identifier_name(),
        });
    expression* ast = this->make_expression<expression::private_variable>(
        this->peek().identifier_name());
    this->skip();
    return ast;
  }

  case token_type::colon:
  case token_type::kw_debugger: {
    source_code_span token_span = this->peek().span();
    this->error_reporter_->report(error_unexpected_token{token_span});
    this->skip();
    return this->make_expression<expression::_invalid>(token_span);
  }

  case token_type::end_of_file:
  case token_type::kw_enum:
  case token_type::kw_for:
  case token_type::kw_if:
  case token_type::kw_return:
  case token_type::kw_switch:
  case token_type::kw_throw:
  case token_type::kw_while:
  case token_type::right_curly:
  case token_type::right_paren:
  case token_type::right_square:
  case token_type::semicolon:
    return this->make_expression<expression::_invalid>(this->peek().span());

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

expression* parser::parse_async_expression(token async_token, precedence prec) {
  expression* ast = this->parse_async_expression_only(async_token);
  if (!prec.binary_operators) {
    return ast;
  }
  return this->parse_expression_remainder(ast, prec);
}

expression* parser::parse_async_expression_only(token async_token) {
  const char8* async_begin = async_token.begin;

  auto parse_arrow_function_arrow_and_body = [this,
                                              async_begin](auto&& parameters) {
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::equal_greater);
    this->skip();

    expression* ast = this->parse_arrow_function_body(
        function_attributes::async, async_begin,
        this->expressions_.make_array(std::move(parameters)));
    return ast;
  };

  switch (this->peek().type) {
  // async () => {}  // Arrow function.
  // async()         // Function call.
  case token_type::left_paren: {
    bool newline_after_async = this->peek().has_leading_newline;

    vector<expression*> parameters(
        "parse_expression async arrow function parameters",
        &this->temporary_memory_);
    source_code_span left_paren_span = this->peek().span();
    this->skip();

    while (this->peek().type != token_type::right_paren) {
      if (this->peek().type == token_type::comma) {
        // TODO(strager): Emit a different error if this is an arrow function.
        // error_extra_comma_not_allowed_between_arguments only makes sense if
        // this is a function call.
        this->error_reporter_->report(
            error_extra_comma_not_allowed_between_arguments{
                .comma = this->peek().span(),
            });
        this->skip();
        continue;
      }
      parameters.emplace_back(
          this->parse_expression(precedence{.commas = false}));
      if (this->peek().type != token_type::comma) {
        break;
      }
      this->skip();
    }

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    source_code_span right_paren_span = this->peek().span();
    this->skip();

    if (this->peek().type == token_type::equal_greater) {
      if (newline_after_async) {
        this->error_reporter_->report(
            error_newline_not_allowed_between_async_and_parameter_list{
                .async = async_token.span(),
                .arrow = this->peek().span(),
            });
      }
      // TODO(strager): Should we call maybe_wrap_erroneous_arrow_function?
      return parse_arrow_function_arrow_and_body(std::move(parameters));
    } else {
      // async as an identifier (variable reference)
      // Function call: async(arg)
      // TODO(strager): Reduce copying of the arguments.
      vector<expression*> call_children("parse_expression async call children",
                                        &this->temporary_memory_);
      call_children.emplace_back(this->make_expression<expression::variable>(
          async_token.identifier_name(), async_token.type));
      for (std::size_t i = 0; i < parameters.size(); ++i) {
        if (parameters.data()[i]->kind() == expression_kind::_invalid) {
        } else {
          call_children.emplace_back(parameters.data()[i]);
        }
      }

      expression* call_ast = this->make_expression<expression::call>(
          this->expressions_.make_array(std::move(call_children)),
          /*left_paren_span=*/left_paren_span,
          /*span_end=*/right_paren_span.end());
      return call_ast;
    }

    QLJS_UNREACHABLE();
  }

  // async parameter => expression-or-block  // Arrow function.
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
  case token_type::kw_await:
  case token_type::kw_yield: {
    if (this->peek().has_leading_newline) {
      goto variable_reference;
    }

    std::array<expression*, 1> parameters = {
        this->make_expression<expression::variable>(
            identifier(this->peek().span()), this->peek().type)};
    this->skip();
    return parse_arrow_function_arrow_and_body(std::move(parameters));
  }

  // async function f(parameters) { statements; }
  case token_type::kw_function: {
    expression* function = this->parse_function_expression(
        function_attributes::async, async_begin);
    return function;
  }

  // async  // Identifier (variable reference).
  variable_reference:
  default: {
    expression* ast = this->make_expression<expression::variable>(
        async_token.identifier_name(), async_token.type);
    return ast;
  }
  }

  QLJS_UNREACHABLE();
}

expression* parser::parse_await_expression(token await_token, precedence prec) {
  bool is_identifier = [&]() -> bool {
    if (this->in_async_function_) {
      return false;
    } else {
      // await is a unary operator (in modules) or an identifier (in scripts).
      switch (this->peek().type) {
      QLJS_CASE_BINARY_ONLY_OPERATOR:
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL:
      QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      case token_type::colon:
      case token_type::comma:
      case token_type::dot:
      case token_type::end_of_file:
      case token_type::equal:
      case token_type::equal_greater:
      case token_type::kw_in:
      case token_type::question:
      case token_type::question_dot:
      case token_type::right_curly:
      case token_type::right_paren:
      case token_type::right_square:
      case token_type::semicolon:
        return true;

      // await /regexp/;
      // await / rhs;
      case token_type::slash:
      case token_type::slash_equal: {
        buffering_error_reporter temp_error_reporter;
        error_reporter* old_error_reporter =
            std::exchange(this->error_reporter_, &temp_error_reporter);
        lexer_transaction transaction = this->lexer_.begin_transaction();

        if (this->in_top_level_) {
          // Try to parse the / as a regular expression literal.
          [[maybe_unused]] expression* ast = this->parse_expression(prec);
        } else {
          // Try to parse the / as a binary division operator.
          [[maybe_unused]] expression* ast = this->parse_expression_remainder(
              this->make_expression<expression::variable>(
                  await_token.identifier_name(), await_token.type),
              prec);
        }
        bool parsed_ok = temp_error_reporter.empty() &&
                         !this->lexer_.transaction_has_lex_errors(transaction);

        this->lexer_.roll_back_transaction(std::move(transaction));
        this->error_reporter_ = old_error_reporter;

        if (this->in_top_level_) {
          bool parsed_slash_as_regexp = parsed_ok;
          return !parsed_slash_as_regexp;
        } else {
          bool parsed_slash_as_divide = parsed_ok;
          return parsed_slash_as_divide;
        }
      }

      case token_type::kw_of:
        // HACK(strager): This works around for-of parsing. Remove this case
        // when for-of parsing is fixed.
        [[fallthrough]];
      case token_type::minus_minus:
      case token_type::plus_plus:
        // TODO(strager): Parse 'await--x' and 'await--;' correctly.
        [[fallthrough]];
      case token_type::complete_template:
      case token_type::incomplete_template:
      case token_type::left_paren:
      case token_type::left_square:
      case token_type::minus:
      case token_type::plus:
        return !this->in_top_level_;

      case token_type::bang:
      case token_type::dot_dot_dot:
      case token_type::identifier:
      case token_type::kw_as:
      case token_type::kw_async:
      case token_type::kw_await:
      case token_type::kw_from:
      case token_type::kw_function:
      case token_type::kw_get:
      case token_type::kw_let:
      case token_type::kw_set:
      case token_type::kw_static:
      case token_type::kw_yield:
      case token_type::left_curly:
      case token_type::number:
      case token_type::private_identifier:
      case token_type::regexp:
      case token_type::reserved_keyword_with_escape_sequence:
      case token_type::string:
      case token_type::tilde:
      default:
        return false;
      }
    }
  }();

  if (is_identifier) {
    return this->make_expression<expression::variable>(
        await_token.identifier_name(), await_token.type);
  } else {
    source_code_span operator_span = await_token.span();
    if (!(this->in_async_function_ || this->in_top_level_)) {
      this->error_reporter_->report(error_await_operator_outside_async{
          .await_operator = operator_span,
      });
    }

    expression* child = this->parse_expression(prec);
    if (child->kind() == expression_kind::_invalid) {
      this->error_reporter_->report(error_missing_operand_for_operator{
          .where = operator_span,
      });
    }
    return this->make_expression<expression::await>(child, operator_span);
  }
}

expression* parser::parse_expression_remainder(expression* ast,
                                               precedence prec) {
  if (prec.commas) {
    QLJS_ASSERT(prec.binary_operators);
  }

  vector<expression*, /*InSituCapacity=*/2> children(
      "parse_expression_remainder children", &this->temporary_memory_, &ast,
      &ast + 1);
  auto build_expression = [&]() {
    if (children.size() == 1) {
      return children.front();
    } else {
      QLJS_ASSERT(children.size() >= 2);
      return this->make_expression<expression::binary_operator>(
          this->expressions_.make_array(std::move(children)));
    }
  };

next:
  switch (this->peek().type) {
  // x, y, z  // Sequence operator or parameter separator.
  case token_type::comma: {
    if (!prec.commas) {
      break;
    }
    source_code_span comma_span = this->peek().span();
    this->skip();

    if (this->peek().type == token_type::right_paren) {
      // Probably an arrow function with a trailing comma in its parameter list:
      // (parameters, go, here, ) => expression-or-block
      return this->make_expression<expression::trailing_comma>(
          this->expressions_.make_array(std::move(children)), comma_span);
    } else {
      // Comma expression: a, b, c
      expression* rhs = children.emplace_back(
          this->parse_expression(precedence{.commas = false}));
      if (rhs->kind() == expression_kind::_invalid) {
        this->error_reporter_->report(
            error_missing_operand_for_operator{comma_span});
      }
    }
    goto next;
  }

  // x + y
  QLJS_CASE_BINARY_ONLY_OPERATOR:
  case token_type::minus:
  case token_type::plus:
  case token_type::slash: {
    if (!prec.math_or_logical_or_assignment) {
      break;
    }
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* rhs = children.emplace_back(this->parse_expression(
        precedence{.binary_operators = false, .commas = false}));
    if (rhs->kind() == expression_kind::_invalid) {
      this->error_reporter_->report(
          error_missing_operand_for_operator{operator_span});
    }
    goto next;
  }

  // f(x, y, z)  // Function call.
  case token_type::left_paren:
    children.back() = this->parse_call_expression_remainder(children.back());
    goto next;

  // x += y
  // f().prop = other
  // x[y] &&= z
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
  QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
  case token_type::equal: {
    if (!prec.math_or_logical_or_assignment) {
      break;
    }
    expression_kind kind;
    switch (this->peek().type) {
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      kind = expression_kind::compound_assignment;
      break;
    QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      kind = expression_kind::conditional_assignment;
      break;
    case token_type::equal:
      kind = expression_kind::assignment;
      break;
    default:
      QLJS_UNREACHABLE();
    }
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* lhs = build_expression();
    switch (lhs->kind()) {
    default:
      this->error_reporter_->report(
          error_invalid_expression_left_of_assignment{lhs->span()});
      break;
    case expression_kind::_invalid:
      // An error should have been reported elsewhere.
      break;
    case expression_kind::array:
    case expression_kind::dot:
    case expression_kind::index:
    case expression_kind::object:
    case expression_kind::variable:
      break;
    }
    expression* rhs = this->parse_expression(
        precedence{.commas = false, .in_operator = prec.in_operator});
    if (rhs->kind() == expression_kind::_invalid) {
      this->error_reporter_->report(error_missing_operand_for_operator{
          .where = operator_span,
      });
    }
    children.clear();
    children.emplace_back(
        this->make_expression<expression::assignment>(kind, lhs, rhs));
    goto next;
  }

  // x.y
  case token_type::dot: {
    source_code_span dot_span = this->peek().span();
    this->skip();
    switch (this->peek().type) {
    case token_type::identifier:
    case token_type::private_identifier:
    case token_type::reserved_keyword_with_escape_sequence:
    QLJS_CASE_KEYWORD:
      children.back() = this->make_expression<expression::dot>(
          children.back(), this->peek().identifier_name());
      this->skip();
      goto next;

    case token_type::string: {
      this->error_reporter_->report(error_invalid_rhs_for_dot_operator{
          .dot = dot_span,
      });
      children.emplace_back(
          this->make_expression<expression::literal>(this->peek().span()));
      this->skip();
      goto next;
    }

    QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL:
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
    QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
    case token_type::colon:
    case token_type::comma:
    case token_type::end_of_file:
    case token_type::equal:
    case token_type::minus:
    case token_type::plus:
    case token_type::question:
    case token_type::right_paren: {
      source_code_span empty_property_name(dot_span.end(), dot_span.end());
      children.back() = this->make_expression<expression::dot>(
          children.back(), identifier(empty_property_name));
      this->error_reporter_->report(
          error_missing_property_name_for_dot_operator{
              .dot = dot_span,
          });
      goto next;
    }

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    break;
  }

  // x?.y
  // x?.#y
  // array?.[index]
  // f?.(x, y)
  case token_type::question_dot: {
    this->skip();
    switch (this->peek().type) {
    // x?.y
    case token_type::identifier:
    case token_type::private_identifier:
    case token_type::reserved_keyword_with_escape_sequence:
    QLJS_CASE_KEYWORD:
      children.back() = this->make_expression<expression::dot>(
          children.back(), this->peek().identifier_name());
      this->skip();
      goto next;

    // tag?.`template`
    // tag?.`template${goes}here`
    case token_type::complete_template:
    case token_type::incomplete_template:
      children.back() = this->parse_template(children.back());
      goto next;

    // f?.(x, y)
    case token_type::left_paren:
      children.back() = this->parse_call_expression_remainder(children.back());
      goto next;

    // array?.[index]
    case token_type::left_square:
      children.back() = this->parse_index_expression_remainder(children.back());
      goto next;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    break;
  }

  // o[key]  // Indexing expression.
  case token_type::left_square: {
    children.back() = parse_index_expression_remainder(children.back());
    goto next;
  }

  // x++  // Suffix unary operator.
  case token_type::minus_minus:
  case token_type::plus_plus:
    if (this->peek().has_leading_newline) {
      // Newline is not allowed before suffix ++ or --. Let
      // parse_and_visit_statement insert a semicolon for us.
      break;
    } else {
      source_code_span operator_span = this->peek().span();
      this->skip();
      children.back() = this->make_expression<expression::rw_unary_suffix>(
          children.back(), operator_span);
    }
    goto next;

  // key in o
  case token_type::kw_in:
    if (!prec.in_operator) {
      break;
    }
    this->skip();
    children.emplace_back(this->parse_expression(prec));
    goto next;

  // x ? y : z  // Conditional operator.
  case token_type::question: {
    if (prec.is_typeof) {
      break;
    }
    source_code_span question_span = this->peek().span();
    this->skip();

    expression* condition = build_expression();

    expression* true_expression;
    if (this->peek().type == token_type::colon) {
      this->error_reporter_->report(error_missing_operand_for_operator{
          .where = question_span,
      });
      true_expression =
          this->make_expression<expression::_invalid>(source_code_span(
              this->lexer_.end_of_previous_token(), this->peek().begin));
    } else {
      true_expression = this->parse_expression();
    }

    if (this->peek().type != token_type::colon) {
      source_code_span expected_colon(this->lexer_.end_of_previous_token(),
                                      this->lexer_.end_of_previous_token());
      this->error_reporter_->report(
          error_missing_colon_in_conditional_expression{
              .expected_colon = expected_colon,
              .question = question_span,
          });
      expression* false_expression =
          this->make_expression<expression::_invalid>(expected_colon);
      return this->make_expression<expression::conditional>(
          condition, true_expression, false_expression);
    }
    source_code_span colon_span = this->peek().span();
    this->skip();

    expression* false_expression = this->parse_expression(prec);
    if (false_expression->kind() == expression_kind::_invalid) {
      this->error_reporter_->report(error_missing_operand_for_operator{
          .where = colon_span,
      });
    }

    return this->make_expression<expression::conditional>(
        condition, true_expression, false_expression);
  }

  // (parameters, go, here) => expression-or-block // Arrow function.
  case token_type::equal_greater: {
    this->parse_arrow_function_expression_remainder(children);
    goto next;
  }

  // html`<h1>My Website</h1>  // Template call.
  // html`<h1>${title}</h1>`   // Template call.
  case token_type::complete_template:
  case token_type::incomplete_template: {
    expression* tag = children.back();
    children.back() = this->parse_template(tag);
    goto next;
  }

  // x y    // Invalid.
  //
  // x    // ASI.
  // y
  case token_type::identifier:
    if (prec.trailing_identifiers) {
      this->error_reporter_->report(error_unexpected_identifier_in_expression{
          .unexpected = this->peek().identifier_name(),
      });

      // Behave as if a comma appeared before the identifier.
      expression* rhs = children.emplace_back(this->parse_expression(
          precedence{.binary_operators = false, .commas = false}));
      QLJS_ASSERT(rhs->kind() != expression_kind::_invalid);
      goto next;
    }
    break;

  case token_type::bang:
  case token_type::colon:
  case token_type::end_of_file:
  case token_type::kw_as:
  case token_type::kw_async:
  case token_type::kw_await:
  case token_type::kw_break:
  case token_type::kw_case:
  case token_type::kw_class:
  case token_type::kw_const:
  case token_type::kw_continue:
  case token_type::kw_debugger:
  case token_type::kw_default:
  case token_type::kw_delete:
  case token_type::kw_do:
  case token_type::kw_else:
  case token_type::kw_enum:
  case token_type::kw_export:
  case token_type::kw_false:
  case token_type::kw_for:
  case token_type::kw_from:
  case token_type::kw_function:
  case token_type::kw_get:
  case token_type::kw_if:
  case token_type::kw_import:
  case token_type::kw_let:
  case token_type::kw_new:
  case token_type::kw_null:
  case token_type::kw_of:
  case token_type::kw_return:
  case token_type::kw_set:
  case token_type::kw_static:
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
  case token_type::left_curly:
  case token_type::number:
  case token_type::private_identifier:
  case token_type::right_curly:
  case token_type::right_paren:
  case token_type::right_square:
  case token_type::semicolon:
  case token_type::string:
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  return build_expression();
}

void parser::parse_arrow_function_expression_remainder(
    vector<expression*, /*InSituCapacity=*/2>& children) {
  source_code_span arrow_span = this->peek().span();
  this->skip();
  if (children.size() != 1) {
    // TODO(strager): We should report an error for code like this:
    // a + b => c
  }
  expression* lhs = children.back();
  vector<expression*> parameters("parse_arrow_function_expression_remainder",
                                 &this->temporary_memory_);
  const char8* left_paren_begin = nullptr;
  switch (lhs->kind()) {
  case expression_kind::binary_operator:
  case expression_kind::trailing_comma:
    // TODO(strager): Only allow comma expressions, not '(2+3) => 5', for
    // example.
    for (int i = 0; i < lhs->child_count(); ++i) {
      expression* parameter = lhs->child(i);
      switch (parameter->kind()) {
      case expression_kind::literal:
        this->error_reporter_->report(
            error_unexpected_literal_in_parameter_list{
                .literal = parameter->span(),
            });
        break;

      // TODO(strager): Error on other kinds of invalid parameters.
      default:
        parameters.emplace_back(parameter);
        break;
      }
    }
    break;
  case expression_kind::array:
  case expression_kind::assignment:
  case expression_kind::object:
  case expression_kind::spread:
  case expression_kind::variable:
    parameters.emplace_back(lhs);
    break;

  // f(x, y) => {}
  case expression_kind::call:
    if (this->peek().type == token_type::left_curly) {
      left_paren_begin =
          expression_cast<expression::call>(lhs)->left_paren_span().begin();
      for (int i = 1; i < lhs->child_count(); ++i) {
        parameters.emplace_back(lhs->child(i));
      }
      // We will report
      // error_missing_operator_between_expression_and_arrow_function
      // elsewhere.
      break;
    }
    [[fallthrough]];

  // f() => z
  // 42 => {}
  case expression_kind::dot:
  case expression_kind::literal: {
    source_code_span lhs_span = lhs->span();
    left_paren_begin = lhs_span.begin();
    switch (lhs->kind()) {
    case expression_kind::call:
    case expression_kind::dot:
      this->error_reporter_->report(error_unexpected_arrow_after_expression{
          .arrow = arrow_span,
          .expression = lhs_span,
      });
      break;
    case expression_kind::literal:
      this->error_reporter_->report(error_unexpected_arrow_after_literal{
          .arrow = arrow_span,
          .literal_parameter = lhs_span,
      });
      break;
    default:
      QLJS_UNREACHABLE();
    }

    if (this->peek().type != token_type::left_curly) {
      // Treat the '=>' as if it was a binary operator (like '>=').
      children.emplace_back(this->parse_expression(
          precedence{.binary_operators = false, .commas = false}));
      return;
    }
    break;
  }

  default:
    QLJS_UNIMPLEMENTED();
    break;
  }

  expression* arrow_function = this->parse_arrow_function_body(
      function_attributes::normal,
      /*parameter_list_begin=*/left_paren_begin,
      this->expressions_.make_array(std::move(parameters)));
  children.back() =
      this->maybe_wrap_erroneous_arrow_function(arrow_function, /*lhs=*/lhs);
}

expression* parser::parse_call_expression_remainder(expression* callee) {
  source_code_span left_paren_span = this->peek().span();
  vector<expression*, 4> call_children(
      "parse_expression_remainder call children", &this->temporary_memory_,
      &callee, &callee + 1);
  this->skip();
  while (this->peek().type != token_type::right_paren) {
    if (this->peek().type == token_type::comma) {
      this->error_reporter_->report(
          error_extra_comma_not_allowed_between_arguments{
              .comma = this->peek().span(),
          });
      this->skip();
      continue;
    }
    call_children.emplace_back(this->parse_expression(
        precedence{.commas = false, .trailing_identifiers = true}));
    if (this->peek().type != token_type::comma) {
      break;
    }
    this->skip();
  }
  const char8* call_span_end;
  if (this->peek().type == token_type::right_paren) {
    call_span_end = this->peek().end;
    this->skip();
  } else {
    // { f(x }  // Invalid.
    // f(x;     // Invalid.
    call_span_end = this->lexer_.end_of_previous_token();
    this->error_reporter_->report(error_expected_right_paren_for_function_call{
        .expected_right_paren = source_code_span(call_span_end, call_span_end),
        .left_paren = left_paren_span,
    });
  }
  return this->make_expression<expression::call>(
      this->expressions_.make_array(std::move(call_children)),
      /*left_paren_span=*/left_paren_span,
      /*span_end=*/call_span_end);
}

expression* parser::parse_index_expression_remainder(expression* lhs) {
  QLJS_ASSERT(this->peek().type == token_type::left_square);
  source_code_span left_square_span = this->peek().span();
  this->skip();
  expression* subscript =
      this->parse_expression(precedence{.trailing_identifiers = true});
  switch (this->peek().type) {
  case token_type::right_square:
    if (subscript->kind() == expression_kind::_invalid) {
      // expr[]  // Invalid.
      source_code_span right_square_span = this->peek().span();
      this->error_reporter_->report(error_indexing_requires_expression{
          .squares = source_code_span(left_square_span.begin(),
                                      right_square_span.end()),
      });
    }
    break;
  case token_type::end_of_file:
    this->error_reporter_->report(
        error_unmatched_indexing_bracket{.left_square = left_square_span});
    break;
  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  const char8* end = this->peek().end;
  this->skip();
  return this->make_expression<expression::index>(lhs, subscript, end);
}

expression* parser::parse_arrow_function_body(
    function_attributes attributes, const char8* parameter_list_begin) {
  return this->parse_arrow_function_body_impl(attributes, parameter_list_begin);
}

expression* parser::parse_arrow_function_body(
    function_attributes attributes, const char8* parameter_list_begin,
    expression_arena::array_ptr<expression*>&& parameters) {
  return this->parse_arrow_function_body_impl(attributes, parameter_list_begin,
                                              std::move(parameters));
}

template <class... Args>
expression* parser::parse_arrow_function_body_impl(
    function_attributes attributes, const char8* parameter_list_begin,
    Args&&... args) {
  function_guard guard = this->enter_function(attributes);
  if (this->peek().type == token_type::left_curly) {
    buffering_visitor* v = this->expressions_.make_buffering_visitor();
    this->parse_and_visit_statement_block_no_scope(*v);
    const char8* span_end = this->lexer_.end_of_previous_token();
    return this->make_expression<expression::arrow_function_with_statements>(
        attributes, std::forward<Args>(args)..., v, parameter_list_begin,
        span_end);
  } else {
    expression* body = this->parse_expression(precedence{.commas = false});
    return this->make_expression<expression::arrow_function_with_expression>(
        attributes, std::forward<Args>(args)..., body, parameter_list_begin);
  }
}

expression* parser::parse_function_expression(function_attributes attributes,
                                              const char8* span_begin) {
  QLJS_ASSERT(this->peek().type == token_type::kw_function);
  this->skip();
  attributes = this->parse_generator_star(attributes);

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
  std::optional<identifier> function_name = std::nullopt;
  QLJS_WARNING_POP
  switch (this->peek().type) {
  case token_type::kw_await:
  case token_type::kw_yield:
    // NOTE(strager): A function expression named 'await' or 'yield' is allowed
    // even within async functions and generator functions.
    [[fallthrough]];
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
    function_name = this->peek().identifier_name();
    this->skip();
    break;
  default:
    break;
  }
  buffering_visitor* v = this->expressions_.make_buffering_visitor();
  this->parse_and_visit_function_parameters_and_body_no_scope(
      *v,
      /*name=*/function_name.has_value()
          ? std::optional<source_code_span>(function_name->span())
          : std::nullopt,
      attributes);
  const char8* span_end = this->lexer_.end_of_previous_token();
  return function_name.has_value()
             ? this->make_expression<expression::named_function>(
                   attributes, *function_name, v,
                   source_code_span(span_begin, span_end))
             : this->make_expression<expression::function>(
                   attributes, v, source_code_span(span_begin, span_end));
}

expression* parser::parse_object_literal() {
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  const char8* left_curly_begin = this->peek().begin;
  const char8* right_curly_end;
  this->skip();

  vector<object_property_value_pair> entries("parse_object_literal entries",
                                             &this->temporary_memory_);
  auto parse_value_expression = [&]() {
    return this->parse_expression(precedence{.commas = false});
  };
  auto parse_computed_property_name = [this]() -> expression* {
    QLJS_ASSERT(this->peek().type == token_type::left_square);
    this->skip();
    expression* property_name = this->parse_expression();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
    this->skip();
    return property_name;
  };
  auto parse_equal = [&](token key_token, expression* key) {
    expression* lhs;
    bool missing_key;
    switch (key_token.type) {
    case token_type::number:
    case token_type::string:
      lhs = this->make_expression<expression::literal>(key_token.span());
      missing_key = true;
      break;
    default:
      lhs = this->make_expression<expression::variable>(
          key_token.identifier_name(), key_token.type);
      missing_key = false;
      break;
    }
    this->skip();
    expression* rhs = this->parse_expression(precedence{.commas = false});
    expression* value = this->make_expression<expression::assignment>(
        expression_kind::assignment, lhs, rhs);
    if (missing_key) {
      this->error_reporter_->report(error_missing_key_for_object_entry{
          .expression = value->span(),
      });
    }
    entries.emplace_back(key, value);
  };
  auto parse_method_entry = [&](const char8* key_span_begin, expression* key,
                                function_attributes attributes) -> void {
    buffering_visitor* v = this->expressions_.make_buffering_visitor();
    switch (this->peek().type) {
    default: {
      this->parse_and_visit_function_parameters_and_body_no_scope(
          *v,
          /*name=*/
          source_code_span(key_span_begin,
                           this->lexer_.end_of_previous_token()),
          attributes);
      break;
    }
    case token_type::right_curly:
      this->error_reporter_->report(error_missing_function_parameter_list{
          .function_name = key->span(),
      });
      break;
    }

    const char8* span_end = this->lexer_.end_of_previous_token();
    expression* func = this->make_expression<expression::function>(
        attributes, v, source_code_span(key_span_begin, span_end));
    entries.emplace_back(key, func);
  };

  bool expect_comma_or_end = false;
  for (;;) {
    if (this->peek().type == token_type::right_curly) {
      right_curly_end = this->peek().end;
      this->skip();
      break;
    }
    if (this->peek().type == token_type::comma) {
      this->skip();
      expect_comma_or_end = false;
      continue;
    }
    if (this->peek().type == token_type::less ||
        this->peek().type == token_type::semicolon) {
      // { k1: v1; k2() {}< k3: v3 }  // Invalid.
      this->error_reporter_->report(
          error_expected_comma_to_separate_object_literal_entries{
              .unexpected_token = this->peek().span(),
          });
      this->skip();
      expect_comma_or_end = false;
      continue;
    }
    switch (this->peek().type) {
    // ({x) // Invalid.
    case token_type::end_of_file:
    case token_type::right_paren:
    case token_type::right_square:
      right_curly_end = this->lexer_.end_of_previous_token();
      this->error_reporter_->report(error_unclosed_object_literal{
          .object_open =
              source_code_span(left_curly_begin, left_curly_begin + 1),
          .expected_object_close =
              source_code_span(right_curly_end, right_curly_end),
      });
      goto done;

    default:
      break;
    }
    if (expect_comma_or_end) {
      const char8* comma_location = this->lexer_.end_of_previous_token();
      this->error_reporter_->report(
          error_missing_comma_between_object_literal_entries{
              source_code_span(comma_location, comma_location)});
    }

  parse_entry:
    switch (this->peek().type) {
    case token_type::comma:
    case token_type::end_of_file:
    case token_type::right_curly:
    case token_type::semicolon:
      QLJS_ASSERT(false);
      break;

    // {#key: value}
    case token_type::private_identifier:
      this->error_reporter_->report(
          error_private_properties_are_not_allowed_in_object_literals{
              .private_identifier = this->peek().identifier_name(),
          });
      [[fallthrough]];
    // {key: value}
    // {"key": value}
    // {10: value}
    // {keyAndValue}
    QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET:
    QLJS_CASE_RESERVED_KEYWORD:
    case token_type::identifier:
    case token_type::number:
    case token_type::reserved_keyword_with_escape_sequence:
    case token_type::string: {
      token key_token = this->peek();
      expression* key =
          this->make_expression<expression::literal>(key_token.span());
      this->skip();
      switch (this->peek().type) {
      // {x y}  // Invalid.
      // {function f() {}}  // Invalid.
      case token_type::identifier:
        if (key_token.type == token_type::kw_function) {
          this->error_reporter_->report(
              error_methods_should_not_use_function_keyword{
                  .function_token = key_token.span(),
              });
          goto parse_entry;
        } else {
          // We'll report error_missing_comma_between_object_literal_entries on
          // the next iteration of the loop.
          goto single_token_key_and_value;
        }

      single_token_key_and_value:
      case token_type::comma:
      case token_type::less:
      case token_type::right_curly:
      case token_type::semicolon: {
        // Name and value are the same: {keyandvalue}

        switch (key_token.type) {
        case token_type::number:
        case token_type::string: {
          expression* value =
              this->make_expression<expression::_invalid>(key_token.span());
          this->error_reporter_->report(
              error_invalid_lone_literal_in_object_literal{key_token.span()});
          entries.emplace_back(key, value);
          break;
        }

        QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD : {
          expression* value =
              this->make_expression<expression::_invalid>(key_token.span());
          this->error_reporter_->report(
              error_missing_value_for_object_literal_entry{
                  .key = key_token.span()});
          entries.emplace_back(key, value);
          break;
        }

        case token_type::kw_await:
        case token_type::kw_yield:
          // TODO(strager): Disallow referencing a variable named 'await' for
          // async functions, or a variable named 'yield' for generator
          // functions.
          goto single_token_key_and_value_identifier;

        single_token_key_and_value_identifier:
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        case token_type::identifier: {
          expression* value = this->make_expression<expression::variable>(
              key_token.identifier_name(), key_token.type);
          entries.emplace_back(key, value);
          break;
        }

        // { \u{69}f }  // Invalid.
        case token_type::reserved_keyword_with_escape_sequence:
          key_token.report_errors_for_escape_sequences_in_keyword(
              this->error_reporter_);
          goto single_token_key_and_value_identifier;

        // { #privateName }  // Invalid.
        case token_type::private_identifier:
          // We already reported an error. Ignore.
          break;

        default:
          QLJS_UNIMPLEMENTED();
          break;
        }
        break;
      }
      case token_type::colon:
        this->skip();
        entries.emplace_back(key, parse_value_expression());
        break;

      case token_type::equal: {
        parse_equal(key_token, key);
        break;
      }

      // {x += y}  // Invalid.
      expression_without_key:
      QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_LESS_AND_STAR:
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      case token_type::dot:
      case token_type::minus:
      case token_type::minus_minus:
      case token_type::plus:
      case token_type::plus_plus:
      case token_type::question_dot: {
        expression* lhs;
        switch (key_token.type) {
        case token_type::number:
        case token_type::string:
          lhs = this->make_expression<expression::literal>(key_token.span());
          break;
        default:
          lhs = this->make_expression<expression::variable>(
              key_token.identifier_name(), key_token.type);
          break;
        }
        expression* value =
            this->parse_expression_remainder(lhs, precedence{.commas = false});
        entries.emplace_back(key, value);
        this->error_reporter_->report(error_missing_key_for_object_entry{
            .expression = value->span(),
        });
        break;
      }

      case token_type::left_paren:
        parse_method_entry(key_token.begin, key, function_attributes::normal);
        break;

      case token_type::star:
        if (key_token.type == token_type::kw_function) {
          // { function *f() {} }  // Invalid.
          this->error_reporter_->report(
              error_methods_should_not_use_function_keyword{
                  .function_token = key_token.span(),
              });
          this->skip();
          switch (this->peek().type) {
          QLJS_CASE_KEYWORD:
          case token_type::identifier:
          case token_type::number:
          case token_type::reserved_keyword_with_escape_sequence:
          case token_type::string: {
            source_code_span real_key_span = this->peek().span();
            expression* real_key =
                this->make_expression<expression::literal>(real_key_span);
            this->skip();
            parse_method_entry(real_key_span.begin(), real_key,
                               function_attributes::generator);
            break;
          }

          // { get [expr]() {} }
          case token_type::left_square: {
            source_code_span left_square_span = this->peek().span();
            expression* real_key = parse_computed_property_name();
            parse_method_entry(left_square_span.begin(), real_key,
                               function_attributes::generator);
            break;
          }

          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
          }
        } else {
          lexer_transaction transaction = this->lexer_.begin_transaction();
          this->skip();
          if (this->peek().type == token_type::left_paren) {
            // {method*() {}}  // Invalid.
            this->lexer_.roll_back_transaction(std::move(transaction));
            parse_method_entry(key_token.begin, key,
                               function_attributes::normal);
          } else {
            this->skip();
            if (this->peek().type == token_type::left_paren) {
              // {someName *method() {}}  // Invalid.
              this->lexer_.roll_back_transaction(std::move(transaction));
              // We'll report error_missing_comma_between_object_literal_entries
              // on the next iteration of the loop.
              goto single_token_key_and_value;
            } else {
              // {a * b + c}  // Invalid.
              this->lexer_.roll_back_transaction(std::move(transaction));
              goto expression_without_key;
            }
          }
        }
        break;

      // {x  // Invalid.
      case token_type::end_of_file:
        // We'll report error_unclosed_object_literal later when we look for the
        // comma or closing '}'.
        goto single_token_key_and_value;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // { async methodName() { } }
    // { async *generatorName() { } }
    // { get propertyName() { } }
    case token_type::kw_async:
    case token_type::kw_get:
    case token_type::kw_set: {
      bool is_async = this->peek().type == token_type::kw_async;
      function_attributes method_attributes =
          is_async ? function_attributes::async : function_attributes::normal;
      token keyword_token = this->peek();
      source_code_span keyword_span = this->peek().span();
      token_type keyword_type = this->peek().type;
      this->skip();

      if (this->peek().type == token_type::kw_function) {
        // { set function() { } }
        // { async function f() { } }  // Invalid.
        lexer_transaction transaction = this->lexer_.begin_transaction();
        source_code_span function_keyword_span = this->peek().span();
        this->skip();
        switch (this->peek().type) {
        // { set function() { } }
        case token_type::left_paren:
          this->lexer_.roll_back_transaction(std::move(transaction));
          break;

        // { async function f() { } }  // Invalid.
        case token_type::identifier:
        default:
          this->lexer_.commit_transaction(std::move(transaction));
          this->error_reporter_->report(
              error_methods_should_not_use_function_keyword{
                  .function_token = function_keyword_span,
              });
          break;
        }
      }

      if (is_async && this->peek().type == token_type::star) {
        // { async *generatorName() { } }
        method_attributes = is_async ? function_attributes::async_generator
                                     : function_attributes::generator;
        this->skip();
      }

      switch (this->peek().type) {
      // get #method() {}
      case token_type::private_identifier:
        this->error_reporter_->report(
            error_private_properties_are_not_allowed_in_object_literals{
                .private_identifier = this->peek().identifier_name(),
            });
        [[fallthrough]];
      // get method() {}
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::number:
      case token_type::reserved_keyword_with_escape_sequence:
      case token_type::string: {
        source_code_span key_span = this->peek().span();
        expression* key = this->make_expression<expression::literal>(key_span);
        this->skip();
        parse_method_entry(keyword_span.begin(), key, method_attributes);
        break;
      }

      // { get [expr]() {} }
      case token_type::left_square: {
        source_code_span left_square_span = this->peek().span();
        expression* key = parse_computed_property_name();
        parse_method_entry(left_square_span.begin(), key, method_attributes);
        break;
      }

      case token_type::equal: {
        expression* key =
            this->make_expression<expression::literal>(keyword_span);
        parse_equal(keyword_token, key);
        break;
      }

      // { get: value }
      // { async: value }
      case token_type::colon: {
        this->skip();
        expression* key =
            this->make_expression<expression::literal>(keyword_span);
        entries.emplace_back(key, parse_value_expression());
        break;
      }

      // { get() {} }
      case token_type::left_paren: {
        expression* key =
            this->make_expression<expression::literal>(keyword_span);
        parse_method_entry(keyword_span.begin(), key,
                           function_attributes::normal);
        break;
      }

      // { get }
      case token_type::comma:
      case token_type::less:
      case token_type::right_curly:
      case token_type::semicolon: {
        expression* key =
            this->make_expression<expression::literal>(keyword_span);
        expression* value = this->make_expression<expression::variable>(
            identifier(keyword_span), keyword_type);
        entries.emplace_back(key, value);
        break;
      }

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // {[keyExpression]: value}
    case token_type::left_square: {
      source_code_span left_square_span = this->peek().span();
      expression* key = parse_computed_property_name();
      switch (this->peek().type) {
      case token_type::colon:
        this->skip();
        entries.emplace_back(key, parse_value_expression());
        break;

      case token_type::left_paren:
        parse_method_entry(left_square_span.begin(), key,
                           function_attributes::normal);
        break;

      case token_type::comma:
      case token_type::less:
      case token_type::right_curly:
      case token_type::semicolon: {
        source_code_span key_span(left_square_span.begin(),
                                  this->lexer_.end_of_previous_token());
        expression* value =
            this->make_expression<expression::_invalid>(key_span);
        this->error_reporter_->report(
            error_missing_value_for_object_literal_entry{.key = key_span});
        entries.emplace_back(key, value);
        break;
      }

      // {[key]*() {}}  // Invalid.
      case token_type::star:
        parse_method_entry(left_square_span.begin(), key,
                           function_attributes::normal);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;
    }

    // *generatorMethod() {}
    case token_type::star: {
      this->skip();
      switch (this->peek().type) {
      // *#method() {}
      case token_type::private_identifier:
        this->error_reporter_->report(
            error_private_properties_are_not_allowed_in_object_literals{
                .private_identifier = this->peek().identifier_name(),
            });
        [[fallthrough]];
      // *method() {}
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::number:
      case token_type::reserved_keyword_with_escape_sequence:
      case token_type::string: {
        source_code_span method_name_span = this->peek().span();
        expression* method_name =
            this->make_expression<expression::literal>(method_name_span);
        this->skip();
        parse_method_entry(method_name_span.begin(), method_name,
                           function_attributes::generator);
        break;
      }

      case token_type::left_square: {
        source_code_span left_square_span = this->peek().span();
        expression* key = parse_computed_property_name();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
        parse_method_entry(left_square_span.begin(), key,
                           function_attributes::generator);
        break;
      }

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // {...other}  // Spread operator.
    case token_type::dot_dot_dot:
      entries.emplace_back(parse_value_expression());
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    expect_comma_or_end = true;
  }
done:
  return this->make_expression<expression::object>(
      this->expressions_.make_array(std::move(entries)),
      source_code_span(left_curly_begin, right_curly_end));
}

expression* parser::parse_class_expression() {
  QLJS_ASSERT(this->peek().type == token_type::kw_class);
  const char8* span_begin = this->peek().begin;

  buffering_visitor* v = this->expressions_.make_buffering_visitor();
  this->parse_and_visit_class_heading(
      *v, /*require_name=*/name_requirement::optional);

  const char8* span_end;
  if (this->peek().type == token_type::left_curly) {
    this->skip();

    this->parse_and_visit_class_body(*v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
    span_end = this->peek().end;
    this->skip();
  } else {
    span_end = this->lexer_.end_of_previous_token();
    this->error_reporter_->report(error_missing_body_for_class{
        .class_keyword_and_name_and_heritage =
            source_code_span(span_begin, span_end),
    });
  }

  return this->make_expression<expression::_class>(
      v, source_code_span(span_begin, span_end));
}

expression* parser::parse_template(std::optional<expression*> tag) {
  if (this->peek().type == token_type::complete_template) {
    if (!tag.has_value()) {
      QLJS_UNIMPLEMENTED();
    }
    source_code_span template_span = this->peek().span();
    this->skip();
    return this->make_expression<expression::tagged_template_literal>(
        this->expressions_.make_array(&*tag, &*tag + 1), template_span);
  }

  const char8* template_begin = this->peek().begin;
  vector<expression*> children("parse_template children",
                               &this->temporary_memory_);
  if (tag.has_value()) {
    children.emplace_back(*tag);
  }
  for (;;) {
    QLJS_ASSERT(this->peek().type == token_type::incomplete_template);
    this->skip();
    children.emplace_back(this->parse_expression());
    switch (this->peek().type) {
    case token_type::right_curly:
      this->lexer_.skip_in_template(template_begin);
      switch (this->peek().type) {
      case token_type::complete_template: {
        const char8* template_end = this->peek().end;
        this->skip();

        expression_arena::array_ptr<expression*> children_array =
            this->expressions_.make_array(std::move(children));
        source_code_span template_span(template_begin, template_end);
        if (tag.has_value()) {
          return this->make_expression<expression::tagged_template_literal>(
              children_array, template_span);
        } else {
          return this->make_expression<expression::_template>(children_array,
                                                              template_span);
        }
      }

      case token_type::incomplete_template:
        continue;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }
}

function_attributes parser::parse_generator_star(
    function_attributes original_attributes) {
  bool is_generator = this->peek().type == token_type::star;
  if (is_generator) {
    this->skip();
    switch (original_attributes) {
    case function_attributes::async:
      return function_attributes::async_generator;
    case function_attributes::async_generator:
      QLJS_ASSERT(false);
      return function_attributes::async_generator;
    case function_attributes::generator:
      QLJS_ASSERT(false);
      return function_attributes::generator;
    case function_attributes::normal:
      return function_attributes::generator;
    }
    QLJS_UNREACHABLE();
  } else {
    return original_attributes;
  }
}

expression* parser::maybe_wrap_erroneous_arrow_function(
    expression* arrow_function, expression* lhs) {
  switch (lhs->kind()) {
  default:
    return arrow_function;

  case expression_kind::trailing_comma: {
    expression::trailing_comma* parameter_list =
        expression_cast<expression::trailing_comma>(lhs);
    expression* last_parameter =
        parameter_list->child(parameter_list->child_count() - 1);
    if (last_parameter->kind() == expression_kind::spread) {
      this->error_reporter_->report(
          error_comma_not_allowed_after_spread_parameter{
              .comma = parameter_list->comma_span(),
              .spread = last_parameter->span(),
          });
    }
    return arrow_function;
  }

  case expression_kind::call: {
    expression::call* call = expression_cast<expression::call>(lhs);
    this->error_reporter_->report(
        error_missing_operator_between_expression_and_arrow_function{
            .where = source_code_span(call->span().begin(),
                                      call->left_paren_span().end()),
        });
    std::array<expression*, 2> children{lhs->child_0(), arrow_function};
    return this->make_expression<expression::binary_operator>(
        this->expressions_.make_array(std::move(children)));
  }
  }
}

bool parser::has_potential_side_effects(expression* ast) {
  switch (ast->kind()) {
  case expression_kind::_class:
  case expression_kind::_new:
  case expression_kind::assignment:
  case expression_kind::await:
  case expression_kind::binary_operator:  // TODO(keyehzh): '===' and '!==' are
                                          // side-effect-free
  case expression_kind::call:
  case expression_kind::compound_assignment:
  case expression_kind::conditional_assignment:
  case expression_kind::dot:
  case expression_kind::import:
  case expression_kind::index:
  case expression_kind::rw_unary_prefix:
  case expression_kind::rw_unary_suffix:
  case expression_kind::spread:
  case expression_kind::tagged_template_literal:
  case expression_kind::unary_operator:
  case expression_kind::yield_many:
  case expression_kind::yield_none:
  case expression_kind::yield_one:
    return true;

  case expression_kind::_invalid:
  case expression_kind::function:
  case expression_kind::literal:
  case expression_kind::named_function:
  case expression_kind::new_target:
  case expression_kind::private_variable:
  case expression_kind::super:
  case expression_kind::variable:
    return false;

  case expression_kind::_typeof:
    return has_potential_side_effects(ast->child(0));

  case expression_kind::_template:
  case expression_kind::array:
  case expression_kind::arrow_function_with_expression:
  case expression_kind::arrow_function_with_statements:
  case expression_kind::trailing_comma:
    for (int i = 0; i < ast->child_count(); i++) {
      if (has_potential_side_effects(ast->child(i))) return true;
    }
    return false;

  case expression_kind::conditional:
    return has_potential_side_effects(ast->child_0()) ||
           has_potential_side_effects(ast->child_1()) ||
           has_potential_side_effects(ast->child_2());

  case expression_kind::object: {
    for (int i = 0; i < ast->object_entry_count(); i++) {
      auto entry = ast->object_entry(i);
      if (entry.property.has_value()) {
        return has_potential_side_effects(*entry.property) ||
               has_potential_side_effects(entry.value);
      }
    }
    return false;
  }
  }
  QLJS_UNREACHABLE();
}

void parser::consume_semicolon() {
  switch (this->peek().type) {
  case token_type::semicolon:
    this->skip();
    break;
  case token_type::end_of_file:
  case token_type::right_curly:
    // Automatically insert a semicolon, then consume it.
    break;
  default:
    if (this->peek().has_leading_newline) {
      // Automatically insert a semicolon, then consume it.
    } else {
      this->lexer_.insert_semicolon();
      this->error_reporter_->report(
          error_missing_semicolon_after_statement{this->peek().span()});
      this->skip();
    }
    break;
  }
}

void parser::crash_on_unimplemented_token(const char* qljs_file_name,
                                          int qljs_line,
                                          const char* qljs_function_name) {
#if QLJS_HAVE_SETJMP
  if (this->have_fatal_parse_error_jmp_buf_) {
    this->error_reporter_->report(error_unexpected_token{
        .token = this->peek().span(),
    });
    std::longjmp(this->fatal_parse_error_jmp_buf_, 1);
    QLJS_UNREACHABLE();
  }
#endif

  std::fprintf(stderr, "%s:%d: fatal: token not implemented in %s: %s",
               qljs_file_name, qljs_line, qljs_function_name,
               to_string(this->peek().type));
  cli_locator locator(this->lexer_.original_input());
  cli_source_position token_position = locator.position(this->peek().begin);
  std::fprintf(stderr, " on line %d column %d", token_position.line_number,
               token_position.column_number);
  std::fprintf(stderr, "\n");

  QLJS_CRASH_DISALLOWING_CORE_DUMP();
}

void parser::crash_on_depth_limit_exceeded() {
#if QLJS_HAVE_SETJMP
  if (this->have_fatal_parse_error_jmp_buf_) {
    this->error_reporter_->report(error_depth_limit_exceeded{
        .token = this->peek().span(),
    });
    std::longjmp(this->fatal_parse_error_jmp_buf_, 1);
    QLJS_UNREACHABLE();
  }
#endif

  std::fprintf(stderr, "Error: parser depth limit exceeded\n");

  QLJS_CRASH_DISALLOWING_CORE_DUMP();
}

parser::function_guard::function_guard(parser* p, bool was_in_top_level,
                                       bool was_in_async_function,
                                       bool was_in_generator_function,
                                       bool was_in_loop_statement,
                                       bool was_in_switch_statement) noexcept
    : parser_(p),
      was_in_top_level_(was_in_top_level),
      was_in_async_function_(was_in_async_function),
      was_in_generator_function_(was_in_generator_function),
      was_in_loop_statement_(was_in_loop_statement),
      was_in_switch_statement_(was_in_switch_statement) {}

parser::function_guard::~function_guard() noexcept {
  this->parser_->in_top_level_ = this->was_in_top_level_;
  this->parser_->in_async_function_ = this->was_in_async_function_;
  this->parser_->in_generator_function_ = this->was_in_generator_function_;
  this->parser_->in_loop_statement_ = this->was_in_loop_statement_;
  this->parser_->in_switch_statement_ = this->was_in_switch_statement_;
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
