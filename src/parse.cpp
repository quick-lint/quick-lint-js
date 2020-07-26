#include <cstdlib>
#include <iostream>
#include <quicklint-js/lex.h>
#include <quicklint-js/parse.h>

#define QLJS_PARSER_UNIMPLEMENTED() \
  (this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__))

#define QLJS_CASE_BINARY_ONLY_OPERATOR      \
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

namespace quicklint_js {
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
      this->error_reporter_->report_error_missing_oprand_for_operator(
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
    case token_type::comma:
      if (!prec.commas) {
        break;
      }
      [[fallthrough]];
    QLJS_CASE_BINARY_ONLY_OPERATOR:
    case token_type::minus:
    case token_type::plus: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      children.emplace_back(this->parse_expression(
          precedence{.binary_operators = false, .commas = false}));
      if (children.back()->kind() == expression_kind::_invalid) {
        this->error_reporter_->report_error_missing_oprand_for_operator(
            operator_span);
      }
      goto next;
    }

    // Function call: f(x, y, z)
    case token_type::left_paren: {
      std::vector<expression_ptr> call_children{children.back()};
      this->lexer_.skip();
      while (this->peek().type != token_type::right_paren) {
        call_children.emplace_back(this->parse_expression(
            precedence{.binary_operators = true, .commas = false}));
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

    case token_type::equal: {
      this->lexer_.skip();
      expression_ptr lhs = build_expression();
      switch (lhs->kind()) {
        default:
          this->error_reporter_
              ->report_error_invalid_expression_left_of_assignment(lhs->span());
          break;
        case expression_kind::dot:
        case expression_kind::variable:
          break;
      }
      expression_ptr rhs = this->parse_expression();
      return this->make_expression<expression_kind::assignment>(lhs, rhs);
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

    case token_type::end_of_file:
    case token_type::right_curly:
    case token_type::right_paren:
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

void parser::crash_on_unimplemented_token(const char *qljs_file_name,
                                          int qljs_line,
                                          const char *qljs_function_name) {
  std::cerr << qljs_file_name << ":" << qljs_line
            << ": fatal: token not implemented in " << qljs_function_name
            << ": " << this->peek().type << '\n';
  std::abort();
}
}  // namespace quicklint_js
