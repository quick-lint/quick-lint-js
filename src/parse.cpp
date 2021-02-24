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

#include <cstdlib>
#include <iostream>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/warning.h>
#include <utility>

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
namespace {
struct arrow_function_parameters {
  vector<expression*> parameters;
  const char8* left_paren_begin = nullptr;
};

arrow_function_parameters arrow_function_parameters_from_lhs(expression*);
}

parser::function_guard parser::enter_function(function_attributes attributes) {
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
  this->in_loop_statement_ = false;
  this->in_switch_statement_ = false;
  return function_guard(this, was_in_async_function, was_in_generator_function,
                        was_in_loop_statement, was_in_switch_statement);
}

parser::loop_guard parser::enter_loop() {
  return loop_guard(this, std::exchange(this->in_loop_statement_, true));
}

expression* parser::parse_expression(precedence prec) {
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
    if (!prec.binary_operators) {
      return ast;
    }
    return this->parse_expression_remainder(ast, prec);
  }

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
    if (!prec.binary_operators) {
      return ast;
    }
    return this->parse_expression_remainder(ast, prec);
  }

  // import.meta
  case token_type::kw_import: {
    expression* ast =
        this->make_expression<expression::import>(this->peek().span());
    this->skip();
    if (!prec.binary_operators) {
      return ast;
    }
    return this->parse_expression_remainder(ast, prec);
  }

  // super()
  case token_type::kw_super: {
    expression* ast =
        this->make_expression<expression::super>(this->peek().span());
    this->skip();
    if (!prec.binary_operators) {
      return ast;
    }
    return this->parse_expression_remainder(ast, prec);
  }

  // `hello${world}`
  case token_type::incomplete_template:
    return this->parse_template(/*tag=*/std::nullopt);

  // await            // Identifier.
  // await myPromise
  case token_type::kw_await: {
    if (this->in_async_function_) {
      // await is a unary operator.
      source_code_span operator_span = this->peek().span();
      this->skip();
      expression* child = this->parse_expression(prec);
      if (child->kind() == expression_kind::_invalid) {
        this->error_reporter_->report(error_missing_operand_for_operator{
            .where = operator_span,
        });
      }
      return this->parse_expression_remainder(
          this->make_expression<expression::await>(child, operator_span), prec);
    } else {
      // await is an identifier.
      goto identifier;
    }
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
        return this->parse_expression_remainder(
            this->make_expression<expression::yield_none>(operator_span), prec);

      case token_type::star: {
        this->skip();
        expression* child = this->parse_expression();
        return this->parse_expression_remainder(
            this->make_expression<expression::yield_many>(child, operator_span),
            prec);
      }

      default: {
        expression* child = this->parse_expression();
        return this->parse_expression_remainder(
            this->make_expression<expression::yield_one>(child, operator_span),
            prec);
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
    return this->parse_expression_remainder(
        this->make_expression<expression::spread>(child, operator_span), prec);
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
                   .commas = false});
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
    return this->parse_expression_remainder(ast, prec);
  }

  // --x
  case token_type::minus_minus:
  case token_type::plus_plus: {
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* child = this->parse_expression(
        precedence{.binary_operators = false, .commas = false});
    if (child->kind() == expression_kind::_invalid) {
      this->error_reporter_->report(error_missing_operand_for_operator{
          .where = operator_span,
      });
    }
    return this->parse_expression_remainder(
        this->make_expression<expression::rw_unary_prefix>(child,
                                                           operator_span),
        prec);
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
        return this->parse_expression_remainder(ast, prec);
      } else {
        // ()  // Invalid.
        this->error_reporter_->report(
            error_missing_expression_between_parentheses{
                .left_paren = left_paren_span,
                .right_paren = right_paren_span,
            });
        expression* child = this->parse_expression();
        if (!prec.binary_operators) {
          return child;
        }
        return this->parse_expression_remainder(child, prec);
      }
    }

    expression* child = this->parse_expression();
    switch (this->peek().type) {
    case token_type::right_paren:
      this->skip();
      break;
    default:
      this->error_reporter_->report(
          error_unmatched_parenthesis{left_paren_span});
      break;
    }
    if (!prec.binary_operators) {
      return child;
    }
    return this->parse_expression_remainder(child, prec);
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

    vector<expression*> children("parse_expression array children");
    for (;;) {
      if (this->peek().type == token_type::right_square) {
        right_square_end = this->peek().end;
        this->skip();
        break;
      }
      if (this->peek().type == token_type::comma) {
        this->skip();
        continue;
      }
      if (this->peek().type == token_type::end_of_file) {
        QLJS_PARSER_UNIMPLEMENTED();
      }
      children.emplace_back(
          this->parse_expression(precedence{.commas = false}));
    }
    expression* ast = this->make_expression<expression::array>(
        this->expressions_.make_array(std::move(children)),
        source_code_span(left_square_begin, right_square_end));
    return this->parse_expression_remainder(ast, prec);
  }

  // {k: v}  // Object literal.
  case token_type::left_curly: {
    expression* ast = this->parse_object_literal();
    return this->parse_expression_remainder(ast, prec);
  }

  // function() {}  // Function expression.
  case token_type::kw_function: {
    expression* function = this->parse_function_expression(
        function_attributes::normal, this->peek().begin);
    return this->parse_expression_remainder(function, prec);
  }

  // class {}
  case token_type::kw_class: {
    expression* class_expression = this->parse_class_expression();
    return this->parse_expression_remainder(class_expression, prec);
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
      vector<expression*> children("parse_expression new children");
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
      return this->parse_expression_remainder(ast, prec);
    }
    }
    QLJS_UNREACHABLE();
  }

  case token_type::end_of_file:
  case token_type::right_paren:
    return this->make_expression<expression::_invalid>(this->peek().span());

  // /regexp/    // RegExp literal.
  // /=regexp/  // RegExp literal.
  case token_type::slash:
  case token_type::slash_equal: {
    this->lexer_.reparse_as_regexp();
    expression* regexp =
        this->make_expression<expression::literal>(this->peek().span());
    this->skip();
    if (!prec.binary_operators) {
      return regexp;
    }
    return this->parse_expression_remainder(regexp, prec);
  }

  QLJS_CASE_BINARY_ONLY_OPERATOR:
  case token_type::dot:
  case token_type::equal: {
    expression* ast =
        this->make_expression<expression::_invalid>(this->peek().span());
    if (!prec.binary_operators) {
      return ast;
    }
    this->error_reporter_->report(
        error_missing_operand_for_operator{this->peek().span()});
    return this->parse_expression_remainder(ast, prec);
  }

  case token_type::colon:
  case token_type::kw_debugger: {
    source_code_span token_span = this->peek().span();
    this->error_reporter_->report(error_unexpected_token{token_span});
    this->skip();
    return this->make_expression<expression::_invalid>(token_span);
  }

  case token_type::comma:
  case token_type::kw_for:
  case token_type::kw_if:
  case token_type::kw_return:
  case token_type::kw_switch:
  case token_type::kw_throw:
  case token_type::kw_while:
  case token_type::right_curly:
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
    vector<expression*> parameters(
        "parse_expression async arrow function parameters");
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
      // TODO(strager): Should we call maybe_wrap_erroneous_arrow_function?
      return parse_arrow_function_arrow_and_body(std::move(parameters));
    } else {
      // async as an identifier (variable reference)
      // Function call: async(arg)
      // TODO(strager): Reduce copying of the arguments.
      vector<expression*> call_children("parse_expression async call children");
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
          /*right_paren_span=*/right_paren_span);
      return call_ast;
    }

    QLJS_UNREACHABLE();
  }

  // async parameter => expression-or-block  // Arrow function.
  case token_type::identifier:
  case token_type::kw_as:
  case token_type::kw_async:
  case token_type::kw_await:
  case token_type::kw_from:
  case token_type::kw_get:
  case token_type::kw_let:
  case token_type::kw_of:
  case token_type::kw_set:
  case token_type::kw_static:
  case token_type::kw_yield: {
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
  default: {
    expression* ast = this->make_expression<expression::variable>(
        async_token.identifier_name(), async_token.type);
    return ast;
  }
  }

  QLJS_UNREACHABLE();
}

expression* parser::parse_expression_remainder(expression* ast,
                                               precedence prec) {
  if (prec.commas) {
    QLJS_ASSERT(prec.binary_operators);
  }

  vector<expression*, /*InSituCapacity=*/2> children(
      "parse_expression_remainder children", &ast, &ast + 1);
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
  case token_type::left_paren: {
    source_code_span left_paren_span = this->peek().span();
    vector<expression*, 4> call_children(
        "parse_expression_remainder call children", &children.back(),
        &children.back() + 1);
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
      call_children.emplace_back(
          this->parse_expression(precedence{.commas = false}));
      if (this->peek().type != token_type::comma) {
        break;
      }
      this->skip();
    }
    QLJS_ASSERT(this->peek().type == token_type::right_paren);
    source_code_span right_paren_span = this->peek().span();
    this->skip();
    children.back() = this->make_expression<expression::call>(
        this->expressions_.make_array(std::move(call_children)),
        /*left_paren_span=*/left_paren_span,
        /*right_paren_span=*/right_paren_span);
    goto next;
  }

  // x += y
  // f().prop = other
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
  case token_type::equal: {
    if (!prec.math_or_logical_or_assignment) {
      break;
    }
    bool is_plain_assignment = this->peek().type == token_type::equal;
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
    children.clear();
    children.emplace_back(this->make_expression<expression::assignment>(
        is_plain_assignment ? expression_kind::assignment
                            : expression_kind::compound_assignment,
        lhs, rhs));
    goto next;
  }

  // x.y
  case token_type::dot: {
    source_code_span dot_span = this->peek().span();
    this->skip();
    switch (this->peek().type) {
    case token_type::identifier:
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

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    break;
  }

  // o[key]  // Indexing expression.
  case token_type::left_square: {
    source_code_span left_square_span = this->peek().span();
    this->skip();
    expression* subscript = this->parse_expression();
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

    children.back() = this->make_expression<expression::index>(
        children.back(), subscript, this->peek().end);
    this->skip();
    goto next;
  }

  // x++  // Suffix unary operator.
  case token_type::minus_minus:
  case token_type::plus_plus:
    if (this->peek().has_leading_newline) {
      // Newline is not allowed before suffix ++ or --. Treat as a semicolon.
      this->lexer_.insert_semicolon();
      goto semicolon;
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
    this->skip();

    expression* condition = build_expression();
    expression* true_expression = this->parse_expression();

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
    this->skip();

    expression* false_expression = this->parse_expression(prec);

    return this->make_expression<expression::conditional>(
        condition, true_expression, false_expression);
  }

  // (parameters, go, here) => expression-or-block // Arrow function.
  case token_type::equal_greater: {
    this->skip();
    if (children.size() != 1) {
      QLJS_UNIMPLEMENTED();
    }
    expression* lhs = children.back();
    arrow_function_parameters parameters =
        arrow_function_parameters_from_lhs(lhs);
    expression* arrow_function = this->parse_arrow_function_body(
        function_attributes::normal,
        /*parameter_list_begin=*/parameters.left_paren_begin,
        this->expressions_.make_array(std::move(parameters.parameters)));
    children.back() =
        this->maybe_wrap_erroneous_arrow_function(arrow_function, /*lhs=*/lhs);
    goto next;
  }

  // html`<h1>My Website</h1>  // Template call.
  case token_type::complete_template: {
    source_code_span template_span = this->peek().span();
    this->skip();
    expression* tag = children.back();
    children.back() =
        this->make_expression<expression::tagged_template_literal>(
            this->expressions_.make_array(&tag, &tag + 1), template_span);
    goto next;
  }

  // html`<h1>${title}</h1>`  // Template call.
  case token_type::incomplete_template: {
    expression* tag = children.back();
    children.back() = this->parse_template(tag);
    goto next;
  }

  case token_type::bang:
  case token_type::colon:
  case token_type::end_of_file:
  case token_type::identifier:
  case token_type::kw_async:
  case token_type::kw_await:
  case token_type::kw_const:
  case token_type::kw_debugger:
  case token_type::kw_delete:
  case token_type::kw_do:
  case token_type::kw_else:
  case token_type::kw_export:
  case token_type::kw_false:
  case token_type::kw_for:
  case token_type::kw_from:
  case token_type::kw_function:
  case token_type::kw_if:
  case token_type::kw_import:
  case token_type::kw_let:
  case token_type::kw_null:
  case token_type::kw_of:
  case token_type::kw_return:
  case token_type::kw_switch:
  case token_type::kw_this:
  case token_type::kw_throw:
  case token_type::kw_true:
  case token_type::kw_try:
  case token_type::kw_var:
  case token_type::kw_while:
  case token_type::kw_with:
  case token_type::left_curly:
  case token_type::number:
  case token_type::right_curly:
  case token_type::right_paren:
  case token_type::right_square:
  case token_type::semicolon:
  case token_type::string:
  semicolon:
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  return build_expression();
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
  case token_type::identifier:
  case token_type::kw_as:
  case token_type::kw_async:
  case token_type::kw_from:
  case token_type::kw_get:
  case token_type::kw_let:
  case token_type::kw_of:
  case token_type::kw_set:
  case token_type::kw_static:
    function_name = this->peek().identifier_name();
    this->skip();
    break;
  default:
    break;
  }
  buffering_visitor* v = this->expressions_.make_buffering_visitor();
  this->parse_and_visit_function_parameters_and_body_no_scope(*v, attributes);
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

  vector<object_property_value_pair> entries("parse_object_literal entries");
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
  auto parse_method_entry = [&](const char8* key_span_begin, expression* key,
                                function_attributes attributes) -> void {
    buffering_visitor* v = this->expressions_.make_buffering_visitor();
    switch (this->peek().type) {
    default:
      this->parse_and_visit_function_parameters_and_body_no_scope(*v,
                                                                  attributes);
      break;
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
    if (expect_comma_or_end) {
      const char8* comma_location = this->lexer_.end_of_previous_token();
      this->error_reporter_->report(
          error_missing_comma_between_object_literal_entries{
              source_code_span(comma_location, comma_location)});
    }
    if (this->peek().type == token_type::end_of_file) {
      QLJS_PARSER_UNIMPLEMENTED();
    }

  parse_entry:
    switch (this->peek().type) {
    case token_type::comma:
    case token_type::end_of_file:
    case token_type::right_curly:
      QLJS_ASSERT(false);
      break;

    // {key: value}
    // {"key": value}
    // {10: value}
    // {keyAndValue}
    QLJS_CASE_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET:
    case token_type::identifier:
    case token_type::number:
    case token_type::string: {
      token_type key_type = this->peek().type;
      source_code_span key_span = this->peek().span();
      expression* key = this->make_expression<expression::literal>(key_span);
      this->skip();
      switch (this->peek().type) {
      // {x y}  // Invalid.
      // {function f() {}}  // Invalid.
      case token_type::identifier:
        if (key_type == token_type::kw_function) {
          this->error_reporter_->report(
              error_methods_should_not_use_function_keyword{
                  .function_token = key_span,
              });
          goto parse_entry;
        } else {
          // We'll report error_missing_comma_between_object_literal_entries on
          // the next iteration of the loop.
          goto single_token_key_and_value;
        }

      single_token_key_and_value:
      case token_type::comma:
      case token_type::right_curly: {
        // Name and value are the same: {keyandvalue}

        switch (key_type) {
        case token_type::number:
        case token_type::string: {
          expression* value =
              this->make_expression<expression::_invalid>(key_span);
          this->error_reporter_->report(
              error_invalid_lone_literal_in_object_literal{key_span});
          entries.emplace_back(key, value);
          break;
        }

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
        case token_type::kw_function:
        case token_type::kw_if:
        case token_type::kw_import:
        case token_type::kw_in:
        case token_type::kw_instanceof:
        case token_type::kw_new:
        case token_type::kw_null:
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
        case token_type::kw_with: {
          expression* value =
              this->make_expression<expression::_invalid>(key_span);
          this->error_reporter_->report(
              error_missing_value_for_object_literal_entry{.key = key_span});
          entries.emplace_back(key, value);
          break;
        }

        case token_type::kw_await:
        case token_type::kw_yield:
          // TODO(strager): Disallow referencing a variable named 'await' for
          // async functions, or a variable named 'yield' for generator
          // functions.
          [[fallthrough]];
        case token_type::identifier:
        case token_type::kw_as:
        case token_type::kw_async:
        case token_type::kw_from:
        case token_type::kw_get:
        case token_type::kw_let:
        case token_type::kw_of:
        case token_type::kw_set:
        case token_type::kw_static: {
          expression* value = this->make_expression<expression::variable>(
              identifier(key_span), key_type);
          entries.emplace_back(key, value);
          break;
        }

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
        // TODO(strager): Only allow this for identifiers, not numbers or
        // strings.
        expression* value = this->parse_expression_remainder(
            this->make_expression<expression::variable>(identifier(key_span),
                                                        key_type),
            precedence{.commas = false});
        entries.emplace_back(key, value);
        break;
      }

      case token_type::left_paren:
        parse_method_entry(key_span.begin(), key, function_attributes::normal);
        break;

      case token_type::star:
        if (key_type == token_type::kw_function) {
          // { function *f() {} }  // Invalid.
          this->error_reporter_->report(
              error_methods_should_not_use_function_keyword{
                  .function_token = key_span,
              });
          this->skip();
          switch (this->peek().type) {
          QLJS_CASE_KEYWORD:
          case token_type::identifier:
          case token_type::number:
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
          QLJS_PARSER_UNIMPLEMENTED();
        }
        break;

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
      source_code_span keyword_span = this->peek().span();
      token_type keyword_type = this->peek().type;
      this->skip();

      if (this->peek().type == token_type::kw_function) {
        // { async function f() { } }  // Invalid.
        this->error_reporter_->report(
            error_methods_should_not_use_function_keyword{
                .function_token = this->peek().span(),
            });
        this->skip();
      }

      if (is_async && this->peek().type == token_type::star) {
        // { async *generatorName() { } }
        this->skip();
      }

      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::number:
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
      case token_type::right_curly: {
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

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;
    }

    // *generatorMethod() {}
    case token_type::star: {
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::number:
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

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_curly);
  this->skip();

  this->parse_and_visit_class_body(*v);

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
  const char8* span_end = this->peek().end;
  this->skip();

  return this->make_expression<expression::_class>(
      v, source_code_span(span_begin, span_end));
}

expression* parser::parse_template(std::optional<expression*> tag) {
  const char8* template_begin = this->peek().begin;
  vector<expression*> children("parse_template children");
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
  this->error_reporter_->report_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/this->peek().type,
      /*token_begin=*/this->peek().begin);
  QLJS_CRASH_DISALLOWING_CORE_DUMP();
}

parser::function_guard::function_guard(parser* p, bool was_in_async_function,
                                       bool was_in_generator_function,
                                       bool was_in_loop_statement,
                                       bool was_in_switch_statement) noexcept
    : parser_(p),
      was_in_async_function_(was_in_async_function),
      was_in_generator_function_(was_in_generator_function),
      was_in_loop_statement_(was_in_loop_statement),
      was_in_switch_statement_(was_in_switch_statement) {}

parser::function_guard::~function_guard() noexcept {
  this->parser_->in_async_function_ = this->was_in_async_function_;
  this->parser_->in_generator_function_ = this->was_in_generator_function_;
  this->parser_->in_loop_statement_ = this->was_in_loop_statement_;
  this->parser_->in_switch_statement_ = this->was_in_switch_statement_;
}

parser::loop_guard::loop_guard(parser* p, bool was_in_loop_statement) noexcept
    : parser_(p), was_in_loop_statement_(was_in_loop_statement) {}

parser::loop_guard::~loop_guard() noexcept {
  this->parser_->in_loop_statement_ = this->was_in_loop_statement_;
}

parser::switch_guard::switch_guard(parser* p,
                                   bool was_in_switch_statement) noexcept
    : parser_(p), was_in_switch_statement_(was_in_switch_statement) {}

parser::switch_guard::~switch_guard() noexcept {
  this->parser_->in_switch_statement_ = this->was_in_switch_statement_;
}

namespace {
arrow_function_parameters arrow_function_parameters_from_lhs(expression* lhs) {
  arrow_function_parameters result{
      .parameters = vector<expression*>("arrow_function_parameters_from_lhs"),
  };
  switch (lhs->kind()) {
  case expression_kind::binary_operator:
  case expression_kind::trailing_comma:
    // TODO(strager): Validate the parameter list. Disallow '(2+3) => 5',
    // for example.
    for (int i = 0; i < lhs->child_count(); ++i) {
      result.parameters.emplace_back(lhs->child(i));
    }
    break;
  case expression_kind::array:
  case expression_kind::assignment:
  case expression_kind::object:
  case expression_kind::spread:
  case expression_kind::variable:
    result.parameters.emplace_back(lhs);
    break;

  // f(x, y) => {}
  case expression_kind::call:
    result.left_paren_begin =
        expression_cast<expression::call>(lhs)->left_paren_span().begin();
    for (int i = 1; i < lhs->child_count(); ++i) {
      result.parameters.emplace_back(lhs->child(i));
    }
    break;

  default:
    QLJS_UNIMPLEMENTED();
    break;
  }
  return result;
}
}
}
