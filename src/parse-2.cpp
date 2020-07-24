#include <cstdlib>
#include <iostream>
#include <quicklint-js/lex.h>
#include <quicklint-js/parse-2.h>

#define QLJS_PARSER_UNIMPLEMENTED() \
  (this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__))

namespace quicklint_js {
expression_ptr parser2::parse_expression(precedence prec) {
  switch (this->peek().type) {
    case token_type::identifier: {
      expression_ptr ast = this->make_expression<expression_kind::variable>(
          this->peek().identifier_name());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return ast;
      }
      return this->parse_expression_remainder(ast);
    }
    case token_type::number: {
      expression_ptr ast =
          this->make_expression<expression_kind::literal>(this->peek().span());
      this->lexer_.skip();
      if (!prec.binary_operators) {
        return ast;
      }
      return this->parse_expression_remainder(ast);
    }
    case token_type::minus: {
      source_code_span operator_span = this->peek().span();
      this->lexer_.skip();
      expression_ptr child =
          this->parse_expression(precedence{.binary_operators = false});
      return this->parse_expression_remainder(
          this->make_expression<expression_kind::unary_operator>(
              child, operator_span));
    }
    case token_type::left_paren: {
      this->lexer_.skip();
      expression_ptr child = this->parse_expression();
      switch (this->peek().type) {
        case token_type::right_paren:
          this->lexer_.skip();
          break;
        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
      }
      return this->parse_expression_remainder(child);
    }
    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
  }
}

expression_ptr parser2::parse_expression_remainder(expression_ptr ast) {
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
    case token_type::minus:
    case token_type::plus:
      this->lexer_.skip();
      children.emplace_back(
          this->parse_expression(precedence{.binary_operators = false}));
      goto next;

    // Function call: f(x, y, z)
    case token_type::left_paren: {
      std::vector<expression_ptr> call_children{children.back()};
      this->lexer_.skip();
      while (this->peek().type != token_type::right_paren) {
        call_children.emplace_back(this->parse_expression());
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

    case token_type::comma:
    case token_type::end_of_file:
    case token_type::right_paren:
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
  }

  return build_expression();
}
void parser2::crash_on_unimplemented_token(const char *qljs_file_name,
                                           int qljs_line,
                                           const char *qljs_function_name) {
  std::cerr << qljs_file_name << ":" << qljs_line
            << ": fatal: token not implemented in " << qljs_function_name
            << ": " << this->peek().type << '\n';
  std::abort();
}
}  // namespace quicklint_js
