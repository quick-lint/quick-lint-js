#include <cstdlib>
#include <iostream>
#include <memory>
#include <optional>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse.h>

#define QLJS_PARSER_UNIMPLEMENTED() \
  (this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__))

#define QLJS_CASE_BINARY_ONLY_OPERATOR      \
  case token_type::_instanceof:             \
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
  case token_type::slash:                   \
  case token_type::star:                    \
  case token_type::star_star

namespace quick_lint_js {
expression_ptr parser::parse_expression(precedence prec) {
  switch (this->peek().type) {
    case token_type::identifier: {
      expression_ptr ast = this->make_expression<expression_kind::variable>(
          this->peek().identifier_name());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return ast;
      }
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::_false:
    case token_type::_null:
    case token_type::_this:
    case token_type::_true:
    case token_type::complete_template:
    case token_type::number:
    case token_type::string: {
      expression_ptr ast =
          this->make_expression<expression_kind::literal>(this->peek().span());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return ast;
      }
      return this->parse_expression_remainder(ast, prec);
    }

    case token_type::incomplete_template:
      return this->parse_template();
    case token_type::_await: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child = this->parse_expression();
      return this->parse_expression_remainder(
          this->make_expression<expression_kind::await>(child, operator_span),
          prec);
    }

    case token_type::_typeof:
    case token_type::bang:
    case token_type::minus:
    case token_type::plus: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child = this->parse_expression(
          precedence{.binary_operators = false, .commas = true});
      return this->parse_expression_remainder(
          this->make_expression<expression_kind::unary_operator>(child,
                                                                 operator_span),
          prec);
    }

    case token_type::minus_minus:
    case token_type::plus_plus: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child = this->parse_expression(
          precedence{.binary_operators = false, .commas = true});
      return this->parse_expression_remainder(
          this->make_expression<expression_kind::rw_unary_prefix>(
              child, operator_span),
          prec);
    }

    case token_type::left_paren: {
      source_code_span left_paren_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child = this->parse_expression();
      switch (this->peek().type) {
        case token_type::right_paren:
          this->lexer_.skip();
          break;
        default:
          this->error_reporter_->report_error_unmatched_parenthesis(
              left_paren_span);
          break;
      }
      if (!prec.binary_operators) {
        return child;
      }
      return this->parse_expression_remainder(child, prec);
    }

    case token_type::_function: {
      const char *span_begin = this->peek().begin;
      this->lexer_.skip();
      std::optional<identifier> function_name = std::nullopt;
      if (this->peek().type == token_type::identifier) {
        function_name = this->peek().identifier_name();
        this->lexer_.skip();
      }
      std::unique_ptr<buffering_visitor> v =
          std::make_unique<buffering_visitor>();
      this->parse_and_visit_function_parameters_and_body_no_scope(*v);
      // TODO(strager): The span should stop at the end of the }, not at the
      // beginning of the following token.
      const char *span_end = this->peek().begin;
      expression_ptr function =
          function_name.has_value()
              ? this->make_expression<expression_kind::named_function>(
                    *function_name, std::move(v),
                    source_code_span(span_begin, span_end))
              : this->make_expression<expression_kind::function>(
                    std::move(v), source_code_span(span_begin, span_end));
      return this->parse_expression_remainder(function, prec);
    }

    case token_type::_new: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr target = this->parse_expression();
      std::vector<expression_ptr> children;
      if (target->kind() == expression_kind::call) {
        for (int i = 0; i < target->child_count(); ++i) {
          children.emplace_back(target->child(i));
        }
      } else {
        children.emplace_back(target);
      }
      return this->make_expression<expression_kind::_new>(
          std::move(children),
          source_code_span(operator_span.begin(), target->span().end()));
    }
    case token_type::end_of_file:
    case token_type::right_paren:
      return this->make_expression<expression_kind::_invalid>();

    QLJS_CASE_BINARY_ONLY_OPERATOR : {
      expression_ptr ast = this->make_expression<expression_kind::_invalid>();
      if (!prec.binary_operators) {
        return ast;
      }
      this->error_reporter_->report_error_missing_operand_for_operator(
          this->peek().span());
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
    assert(prec.binary_operators);
  }

  std::vector<expression_ptr> children{ast};
  auto build_expression = [&]() {
    if (children.size() == 1) {
      return children.front();
    } else {
      assert(children.size() >= 2);
      return this->make_expression<expression_kind::binary_operator>(
          std::move(children));
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
      children.emplace_back(
          this->parse_expression(precedence{.commas = false}));
      if (children.back()->kind() == expression_kind::_invalid) {
        this->error_reporter_->report_error_missing_operand_for_operator(
            operator_span);
      }
      goto next;
    }

    QLJS_CASE_BINARY_ONLY_OPERATOR:
    case token_type::minus:
    case token_type::plus: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      children.emplace_back(this->parse_expression(
          precedence{.binary_operators = false, .commas = false}));
      if (children.back()->kind() == expression_kind::_invalid) {
        this->error_reporter_->report_error_missing_operand_for_operator(
            operator_span);
      }
      goto next;
    }

    // Function call: f(x, y, z)
    case token_type::left_paren: {
      std::vector<expression_ptr> call_children{children.back()};
      this->lexer_.skip();
      while (this->peek().type != token_type::right_paren) {
        call_children.emplace_back(
            this->parse_expression(precedence{.commas = false}));
        if (this->peek().type != token_type::comma) {
          break;
        }
        this->lexer_.skip();
      }
      assert(this->peek().type == token_type::right_paren);
      source_code_span right_paren_span = this->peek().span();
      this->lexer_.skip();
      children.back() = this->make_expression<expression_kind::call>(
          std::move(call_children), right_paren_span);
      goto next;
    }

    case token_type::equal:
    case token_type::plus_equal: {
      bool is_plain_assignment = this->peek().type == token_type::equal;
      this->lexer_.skip();
      expression_ptr lhs = build_expression();
      switch (lhs->kind()) {
        default:
          this->error_reporter_
              ->report_error_invalid_expression_left_of_assignment(lhs->span());
          break;
        case expression_kind::dot:
        case expression_kind::index:
        case expression_kind::variable:
          break;
      }
      expression_ptr rhs = this->parse_expression(precedence{.commas = false});
      if (is_plain_assignment) {
        return this->make_expression<expression_kind::assignment>(lhs, rhs);
      } else {
        return this->make_expression<expression_kind::updating_assignment>(lhs,
                                                                           rhs);
      }
    }

    case token_type::dot: {
      this->lexer_.skip();
      switch (this->peek().type) {
        case token_type::identifier:
          children.back() = this->make_expression<expression_kind::dot>(
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
      children.back() = this->make_expression<expression_kind::index>(
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
        children.back() =
            this->make_expression<expression_kind::rw_unary_suffix>(
                children.back(), operator_span);
      }
      break;

    case token_type::_in:
      if (!prec.in_operator) {
        break;
      }
      this->lexer_.skip();
      children.emplace_back(this->parse_expression(prec));
      goto next;

    case token_type::_of:
    case token_type::_return:
    case token_type::end_of_file:
    case token_type::identifier:
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

expression_ptr parser::parse_template() {
  const char *template_begin = this->peek().begin;
  std::vector<expression_ptr> children;
  for (;;) {
    assert(this->peek().type == token_type::incomplete_template);
    this->lexer_.skip();
    children.emplace_back(this->parse_expression());
    switch (this->peek().type) {
      case token_type::right_curly:
        this->lexer_.skip_in_template(template_begin);
        switch (this->peek().type) {
          case token_type::complete_template: {
            const char *template_end = this->peek().end;
            this->lexer_.skip();
            return this->make_expression<expression_kind::_template>(
                std::move(children),
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
        this->error_reporter_->report_error_missing_semicolon_after_expression(
            this->peek().span());
        this->lexer_.skip();
      }
      break;
  }
}

void parser::crash_on_unimplemented_token(const char *qljs_file_name,
                                          int qljs_line,
                                          const char *qljs_function_name) {
  source_position token_position = this->locator().position(this->peek().begin);
  std::cerr << qljs_file_name << ":" << qljs_line
            << ": fatal: token not implemented in " << qljs_function_name
            << ": " << this->peek().type << " on line "
            << token_position.line_number << " column "
            << token_position.column_number << '\n';
  std::abort();
}
}  // namespace quick_lint_js
