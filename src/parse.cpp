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
#include <quick-lint-js/vector.h>
#include <quick-lint-js/warning.h>

// parser is a recursive-descent parser.
//
// The parser class currently does not build an abstract syntax tree (AST) for
// statements. This allows the parser to send partial information to the lexer
// incrementally, enabling single-pass parsing and linting [1].
//
// The parser class currently builds an AST for expressions. (See expression.h.)
// Therefore, parsing and linting are not truly single-pass. This detail is not
// exposed to the linter, however; the linter does not see the expression ASTs.
//
// Each parser stores a lexer object internally. From the caller's perspective,
// the parser class takes characters as input.

#define QLJS_PARSER_UNIMPLEMENTED() \
  (this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__))

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

namespace quick_lint_js {
namespace {
vector<expression_ptr> arrow_function_parameters_from_lhs(expression_ptr);
}

expression_ptr parser::parse_expression(precedence prec) {
  switch (this->peek().type) {
    case token_type::identifier: {
      expression_ptr ast = this->make_expression<expression::variable>(
          this->peek().identifier_name());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return ast;
      }
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::kw_false:
    case token_type::kw_null:
    case token_type::kw_this:
    case token_type::kw_true:
    case token_type::complete_template:
    case token_type::number:
    case token_type::string: {
      expression_ptr ast =
          this->make_expression<expression::literal>(this->peek().span());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return ast;
      }
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::kw_import: {
      expression_ptr ast =
          this->make_expression<expression::import>(this->peek().span());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return ast;
      }
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::kw_super: {
      expression_ptr ast =
          this->make_expression<expression::super>(this->peek().span());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return ast;
      }
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::incomplete_template:
      return this->parse_template();

    case token_type::kw_await: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child = this->parse_expression();
      return this->parse_expression_remainder(
          this->make_expression<expression::await>(child, operator_span), prec);
    }

    case token_type::dot_dot_dot: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child = this->parse_expression(prec);
      return this->parse_expression_remainder(
          this->make_expression<expression::spread>(child, operator_span),
          prec);
    }

    case token_type::bang:
    case token_type::kw_delete:
    case token_type::kw_typeof:
    case token_type::kw_void:
    case token_type::minus:
    case token_type::plus:
    case token_type::tilde: {
      token_type type = this->peek().type;
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child = this->parse_expression(
          precedence{.binary_operators = true,
                     .math_or_logical_or_assignment = false,
                     .commas = false});
      expression_ptr ast =
          type == token_type::kw_typeof
              ? this->make_expression<expression::_typeof>(child, operator_span)
              : this->make_expression<expression::unary_operator>(
                    child, operator_span);
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::minus_minus:
    case token_type::plus_plus: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child = this->parse_expression(
          precedence{.binary_operators = false, .commas = true});
      return this->parse_expression_remainder(
          this->make_expression<expression::rw_unary_prefix>(child,
                                                             operator_span),
          prec);
    }

    case token_type::left_paren: {
      source_code_span left_paren_span = this->peek().span();
      this->lexer_.skip();

      if (this->peek().type == token_type::right_paren) {
        this->lexer_.skip();
        if (this->peek().type == token_type::equal_greater) {
          this->lexer_.skip();
          // Arrow function: () => expression-or-block
          expression_ptr ast = this->parse_arrow_function_body(
              function_attributes::normal, left_paren_span.begin());
          return this->parse_expression_remainder(ast, prec);
        } else {
          QLJS_PARSER_UNIMPLEMENTED();
        }
      }

      expression_ptr child = this->parse_expression();
      switch (this->peek().type) {
        case token_type::right_paren:
          this->lexer_.skip();
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

    case token_type::kw_async: {
      const char8 *async_begin = this->peek().begin;
      this->lexer_.skip();

      vector<expression_ptr> parameters(
          "parse_expression async arrow function parameters");
      switch (this->peek().type) {
        case token_type::left_paren:
          this->lexer_.skip();

          if (this->peek().type == token_type::right_paren) {
            // Arrow function: async () => expression-or-block
          } else {
            // Arrow function: async (parameters, go, here) =>
            // expression-or-block
            expression_ptr parenthesized_parameters = this->parse_expression();
            QLJS_ASSERT(parameters.empty());
            parameters =
                arrow_function_parameters_from_lhs(parenthesized_parameters);
          }

          QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
          this->lexer_.skip();

          break;

        // Arrow function: async parameter => expression-or-block
        case token_type::identifier:
          parameters.emplace_back(this->make_expression<expression::variable>(
              identifier(this->peek().span())));
          this->lexer_.skip();
          break;

        // async function f(parameters) { statements; }
        case token_type::kw_function: {
          expression_ptr function = this->parse_function_expression(
              function_attributes::async, async_begin);
          return this->parse_expression_remainder(function, prec);
        }

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
      }

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::equal_greater);
      this->lexer_.skip();

      expression_ptr ast = this->parse_arrow_function_body(
          function_attributes::async, async_begin,
          this->expressions_.make_array(std::move(parameters)));
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::left_square: {
      const char8 *left_square_begin = this->peek().begin;
      const char8 *right_square_end;
      this->lexer_.skip();

      vector<expression_ptr> children("parse_expression array children");
      for (;;) {
        if (this->peek().type == token_type::right_square) {
          right_square_end = this->peek().end;
          this->lexer_.skip();
          break;
        }
        if (this->peek().type == token_type::comma) {
          this->lexer_.skip();
          continue;
        }
        if (this->peek().type == token_type::end_of_file) {
          QLJS_PARSER_UNIMPLEMENTED();
        }
        children.emplace_back(
            this->parse_expression(precedence{.commas = false}));
      }
      expression_ptr ast = this->make_expression<expression::array>(
          this->expressions_.make_array(std::move(children)),
          source_code_span(left_square_begin, right_square_end));
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::left_curly: {
      expression_ptr ast = this->parse_object_literal();
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::kw_function: {
      expression_ptr function = this->parse_function_expression(
          function_attributes::normal, this->peek().begin);
      return this->parse_expression_remainder(function, prec);
    }

    case token_type::kw_new: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr target = this->parse_expression(prec);
      vector<expression_ptr> children("parse_expression new children");
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
    case token_type::end_of_file:
    case token_type::right_paren:
      return this->make_expression<expression::_invalid>();

    case token_type::slash: {
      this->lexer_.reparse_as_regexp();
      expression_ptr regexp =
          this->make_expression<expression::literal>(this->peek().span());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return regexp;
      }
      return this->parse_expression_remainder(regexp, prec);
    }

    QLJS_CASE_BINARY_ONLY_OPERATOR : {
      expression_ptr ast = this->make_expression<expression::_invalid>();
      if (!prec.binary_operators) {
        return ast;
      }
      this->error_reporter_->report(
          error_missing_operand_for_operator{this->peek().span()});
      return this->parse_expression_remainder(ast, prec);
    }
    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
  }
}

expression_ptr parser::parse_expression_remainder(expression_ptr ast,
                                                  precedence prec) {
  if (prec.commas) {
    QLJS_ASSERT(prec.binary_operators);
  }

  vector<expression_ptr, /*InSituCapacity=*/2> children(
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
    case token_type::comma: {
      if (!prec.commas) {
        break;
      }
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr rhs = children.emplace_back(
          this->parse_expression(precedence{.commas = false}));
      if (rhs->kind() == expression_kind::_invalid) {
        this->error_reporter_->report(
            error_missing_operand_for_operator{operator_span});
      }
      goto next;
    }

    QLJS_CASE_BINARY_ONLY_OPERATOR:
    case token_type::minus:
    case token_type::plus:
    case token_type::slash: {
      if (!prec.math_or_logical_or_assignment) {
        break;
      }
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr rhs = children.emplace_back(this->parse_expression(
          precedence{.binary_operators = false, .commas = false}));
      if (rhs->kind() == expression_kind::_invalid) {
        this->error_reporter_->report(
            error_missing_operand_for_operator{operator_span});
      }
      goto next;
    }

    // Function call: f(x, y, z)
    case token_type::left_paren: {
      vector<expression_ptr, 4> call_children(
          "parse_expression_remainder call children", &children.back(),
          &children.back() + 1);
      this->lexer_.skip();
      while (this->peek().type != token_type::right_paren) {
        call_children.emplace_back(
            this->parse_expression(precedence{.commas = false}));
        if (this->peek().type != token_type::comma) {
          break;
        }
        this->lexer_.skip();
      }
      QLJS_ASSERT(this->peek().type == token_type::right_paren);
      source_code_span right_paren_span = this->peek().span();
      this->lexer_.skip();
      children.back() = this->make_expression<expression::call>(
          this->expressions_.make_array(std::move(call_children)),
          right_paren_span);
      goto next;
    }

    case token_type::ampersand_equal:
    case token_type::circumflex_equal:
    case token_type::equal:
    case token_type::greater_greater_equal:
    case token_type::greater_greater_greater_equal:
    case token_type::less_less_equal:
    case token_type::minus_equal:
    case token_type::percent_equal:
    case token_type::pipe_equal:
    case token_type::plus_equal:
    case token_type::slash_equal:
    case token_type::star_equal:
    case token_type::star_star_equal: {
      if (!prec.math_or_logical_or_assignment) {
        break;
      }
      bool is_plain_assignment = this->peek().type == token_type::equal;
      this->lexer_.skip();
      expression_ptr lhs = build_expression();
      switch (lhs->kind()) {
        default:
          this->error_reporter_->report(
              error_invalid_expression_left_of_assignment{lhs->span()});
          break;
        case expression_kind::array:
        case expression_kind::dot:
        case expression_kind::index:
        case expression_kind::object:
        case expression_kind::variable:
          break;
      }
      expression_ptr rhs = this->parse_expression(precedence{.commas = false});
      children.clear();
      children.emplace_back(this->make_expression<expression::assignment>(
          is_plain_assignment ? expression_kind::assignment
                              : expression_kind::compound_assignment,
          lhs, rhs));
      goto next;
    }

    case token_type::dot: {
      this->lexer_.skip();
      switch (this->peek().type) {
        case token_type::identifier:
        QLJS_CASE_KEYWORD:
          children.back() = this->make_expression<expression::dot>(
              children.back(), this->peek().identifier_name());
          this->lexer_.skip();
          goto next;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
      }
      break;
    }

    case token_type::left_square: {
      this->lexer_.skip();
      expression_ptr subscript = this->parse_expression();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
      children.back() = this->make_expression<expression::index>(
          children.back(), subscript, this->peek().end);
      this->lexer_.skip();
      goto next;
    }

    case token_type::minus_minus:
    case token_type::plus_plus:
      if (this->peek().has_leading_newline) {
        // Newline is not allowed before suffix ++ or --. Treat as a semicolon.
        this->lexer_.insert_semicolon();
        goto semicolon;
      } else {
        source_code_span operator_span = this->peek().span();
        this->lexer_.skip();
        children.back() = this->make_expression<expression::rw_unary_suffix>(
            children.back(), operator_span);
      }
      goto next;

    case token_type::kw_in:
      if (!prec.in_operator) {
        break;
      }
      this->lexer_.skip();
      children.emplace_back(this->parse_expression(prec));
      goto next;

    case token_type::question: {
      this->lexer_.skip();

      expression_ptr condition = build_expression();
      expression_ptr true_expression = this->parse_expression();

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
      this->lexer_.skip();

      expression_ptr false_expression = this->parse_expression(prec);

      return this->make_expression<expression::conditional>(
          condition, true_expression, false_expression);
    }

    // Arrow function: (parameters, go, here) => expression-or-block
    case token_type::equal_greater: {
      this->lexer_.skip();
      if (children.size() != 1) {
        QLJS_ASSERT(false && "Not yet implemented");
      }
      expression_ptr lhs = children.back();
      children.back() = this->parse_arrow_function_body(
          function_attributes::normal, /*parameter_list_begin=*/nullptr,
          this->expressions_.make_array(
              arrow_function_parameters_from_lhs(lhs)));
      goto next;
    }

    case token_type::colon:
    case token_type::end_of_file:
    case token_type::identifier:
    case token_type::kw_const:
    case token_type::kw_from:
    case token_type::kw_let:
    case token_type::kw_of:
    case token_type::kw_return:
    case token_type::kw_var:
    case token_type::left_curly:
    case token_type::right_curly:
    case token_type::right_paren:
    case token_type::right_square:
    case token_type::semicolon:
    semicolon:
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
  }

  return build_expression();
}

template <class... Args>
expression_ptr parser::parse_arrow_function_body(
    function_attributes attributes, const char8 *parameter_list_begin,
    Args &&... args) {
  if (this->peek().type == token_type::left_curly) {
    buffering_visitor *v = this->expressions_.make_buffering_visitor();
    this->parse_and_visit_statement_block_no_scope(*v);
    const char8 *span_end = this->lexer_.end_of_previous_token();
    return this->make_expression<expression::arrow_function_with_statements>(
        attributes, std::forward<Args>(args)..., v, parameter_list_begin,
        span_end);
  } else {
    expression_ptr body = this->parse_expression(precedence{.commas = false});
    return this->make_expression<expression::arrow_function_with_expression>(
        attributes, std::forward<Args>(args)..., body, parameter_list_begin);
  }
}

expression_ptr parser::parse_function_expression(function_attributes attributes,
                                                 const char8 *span_begin) {
  QLJS_ASSERT(this->peek().type == token_type::kw_function);
  this->lexer_.skip();
  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
  std::optional<identifier> function_name = std::nullopt;
  QLJS_WARNING_POP
  if (this->peek().type == token_type::identifier) {
    function_name = this->peek().identifier_name();
    this->lexer_.skip();
  }
  buffering_visitor *v = this->expressions_.make_buffering_visitor();
  this->parse_and_visit_function_parameters_and_body_no_scope(*v);
  const char8 *span_end = this->lexer_.end_of_previous_token();
  return function_name.has_value()
             ? this->make_expression<expression::named_function>(
                   attributes, *function_name, v,
                   source_code_span(span_begin, span_end))
             : this->make_expression<expression::function>(
                   attributes, v, source_code_span(span_begin, span_end));
}

expression_ptr parser::parse_object_literal() {
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  const char8 *left_curly_begin = this->peek().begin;
  const char8 *right_curly_end;
  this->lexer_.skip();

  vector<object_property_value_pair> entries("parse_object_literal entries");
  auto parse_value_expression = [&]() {
    return this->parse_expression(precedence{.commas = false});
  };
  bool expect_comma_or_end = false;
  for (;;) {
    if (this->peek().type == token_type::right_curly) {
      right_curly_end = this->peek().end;
      this->lexer_.skip();
      break;
    }
    if (this->peek().type == token_type::comma) {
      this->lexer_.skip();
      expect_comma_or_end = false;
      continue;
    }
    if (expect_comma_or_end) {
      const char8 *comma_location = this->lexer_.end_of_previous_token();
      this->error_reporter_->report(
          error_missing_comma_between_object_literal_entries{
              source_code_span(comma_location, comma_location)});
    }
    if (this->peek().type == token_type::end_of_file) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    switch (this->peek().type) {
      case token_type::comma:
      case token_type::end_of_file:
      case token_type::right_curly:
        QLJS_ASSERT(false);
        break;

      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::number:
      case token_type::string: {
        source_code_span key_span = this->peek().span();
        expression_ptr key =
            this->make_expression<expression::literal>(key_span);
        this->lexer_.skip();
        switch (this->peek().type) {
          case token_type::comma:
          case token_type::right_curly: {
            // Name and value are the same: {keyandvalue}
            // TODO(strager): Only allow this for identifiers, not numbers or
            // strings.
            expression_ptr value = this->make_expression<expression::variable>(
                identifier(key_span));
            entries.emplace_back(key, value);
            break;
          }
          case token_type::colon:
            this->lexer_.skip();
            entries.emplace_back(key, parse_value_expression());
            break;
          case token_type::equal: {
            // TODO(strager): Only allow this for identifiers, not numbers or
            // strings.
            expression_ptr value = this->parse_expression_remainder(
                this->make_expression<expression::variable>(
                    identifier(key_span)),
                precedence{.commas = false});
            entries.emplace_back(key, value);
            break;
          }

          case token_type::left_paren: {
            buffering_visitor *v = this->expressions_.make_buffering_visitor();
            this->parse_and_visit_function_parameters_and_body_no_scope(*v);
            const char8 *span_end = this->lexer_.end_of_previous_token();
            expression_ptr func = this->make_expression<expression::function>(
                function_attributes::normal, v,
                source_code_span(key_span.begin(), span_end));
            entries.emplace_back(key, func);
            break;
          }

          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
        }
        break;
      }

      case token_type::left_square: {
        this->lexer_.skip();
        expression_ptr key = this->parse_expression();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
        this->lexer_.skip();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
        this->lexer_.skip();
        entries.emplace_back(key, parse_value_expression());
        break;
      }

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

expression_ptr parser::parse_template() {
  const char8 *template_begin = this->peek().begin;
  vector<expression_ptr> children("parse_template children");
  for (;;) {
    QLJS_ASSERT(this->peek().type == token_type::incomplete_template);
    this->lexer_.skip();
    children.emplace_back(this->parse_expression());
    switch (this->peek().type) {
      case token_type::right_curly:
        this->lexer_.skip_in_template(template_begin);
        switch (this->peek().type) {
          case token_type::complete_template: {
            const char8 *template_end = this->peek().end;
            this->lexer_.skip();
            return this->make_expression<expression::_template>(
                this->expressions_.make_array(std::move(children)),
                source_code_span(template_begin, template_end));
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

void parser::consume_semicolon() {
  switch (this->peek().type) {
    case token_type::semicolon:
      this->lexer_.skip();
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
            error_missing_semicolon_after_expression{this->peek().span()});
        this->lexer_.skip();
      }
      break;
  }
}

void parser::crash_on_unimplemented_token(const char *qljs_file_name,
                                          int qljs_line,
                                          const char *qljs_function_name) {
  this->error_reporter_->report_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/this->peek().type,
      /*token_begin=*/this->peek().begin);
  std::abort();
}

namespace {
vector<expression_ptr> arrow_function_parameters_from_lhs(expression_ptr lhs) {
  vector<expression_ptr> parameters("arrow_function_parameters_from_lhs");
  switch (lhs->kind()) {
    case expression_kind::binary_operator:
      // TODO(strager): Validate the parameter list. Disallow '(2+3) => 5',
      // for example.
      for (int i = 0; i < lhs->child_count(); ++i) {
        parameters.emplace_back(lhs->child(i));
      }
      break;
    case expression_kind::array:
    case expression_kind::object:
    case expression_kind::variable:
      parameters.emplace_back(lhs);
      break;
    default:
      QLJS_ASSERT(false && "Not yet implemented");
      break;
  }
  return parameters;
}
}
}
