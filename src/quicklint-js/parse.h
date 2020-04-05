#ifndef QUICKLINT_JS_PARSE_H
#define QUICKLINT_JS_PARSE_H

#include <optional>
#include <quicklint-js/error.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/location.h>

namespace quicklint_js {
struct expression_options {
  bool parse_commas;
};

class parser {
 public:
  explicit parser(const char *input, error_reporter *error_reporter)
      : lexer_(input, error_reporter),
        locator_(input),
        error_reporter_(error_reporter) {}

  quicklint_js::locator &locator() noexcept { return this->locator_; }

  template <class Visitor>
  void parse_statement(Visitor &v) {
    this->parse_let_bindings(v);
  }

  template <class Visitor>
  void parse_expression(Visitor &v, expression_options options) {
    bool allow_binary_operator = false;
    bool allow_identifier = true;

    std::optional<source_code_span> last_operator;

    for (;;) {
      switch (this->peek().type) {
        case token_type::left_paren: {
          source_code_span left_paren_span = this->peek().span();
          this->lexer_.skip();
          this->parse_expression(v, expression_options{.parse_commas = true});
          if (this->peek().type == token_type::right_paren) {
            this->lexer_.skip();
          } else {
            this->error_reporter_->report_error_unmatched_parenthesis(
                left_paren_span);
          }
          last_operator = std::nullopt;
          allow_identifier = false;
          break;
        }

        case token_type::identifier:
          if (!allow_identifier) {
            this->error_reporter_->report_error_unexpected_identifier(
                this->peek().span());
          }
          v.visit_variable_use(this->peek().identifier_name());
          this->lexer_.skip();
          last_operator = std::nullopt;
          allow_binary_operator = true;
          allow_identifier = false;
          break;

        case token_type::number:
          this->lexer_.skip();
          last_operator = std::nullopt;
          allow_binary_operator = true;
          break;

        case token_type::plus:
          last_operator = this->peek().span();
          this->lexer_.skip();
          allow_binary_operator = false;
          allow_identifier = true;
          break;

        case token_type::dot:
          this->lexer_.skip();
          this->lexer_.skip();
          break;

        case token_type::comma:
          if (options.parse_commas) {
            goto parse_binary_operator;
          } else {
            goto done;
          }

        case token_type::ampersand:
        case token_type::circumflex:
        case token_type::star:
        parse_binary_operator:
          if (!allow_binary_operator) {
            const source_code_span &bad_token_span = last_operator.has_value()
                                                         ? *last_operator
                                                         : this->peek().span();
            this->error_reporter_->report_error_missing_oprand_for_operator(
                bad_token_span);
          }
          last_operator = this->peek().span();
          this->lexer_.skip();
          allow_binary_operator = false;
          allow_identifier = true;
          break;

        case token_type::right_paren:
        default:
        done:
          if (last_operator.has_value()) {
            this->error_reporter_->report_error_missing_oprand_for_operator(
                *last_operator);
          }
          return;
      }
    }
  }

 private:
  template <class Visitor>
  void parse_let_bindings(Visitor &v) {
    source_code_span let_span = this->peek().span();
    this->lexer_.skip();
    bool first_binding = true;
    for (;;) {
      std::optional<source_code_span> comma_span = std::nullopt;
      if (!first_binding) {
        if (this->peek().type != token_type::comma) {
          break;
        }
        comma_span = this->peek().span();
        this->lexer_.skip();
      }

      switch (this->peek().type) {
        case token_type::identifier: {
          identifier variable_name = this->peek().identifier_name();
          this->lexer_.skip();
          if (this->peek().type == token_type::equal) {
            this->lexer_.skip();
            this->parse_expression(v,
                                   expression_options{.parse_commas = false});
          }
          v.visit_variable_declaration(variable_name);
          break;
        }
        case token_type::_if:
        case token_type::number:
          this->error_reporter_->report_error_invalid_binding_in_let_statement(
              this->peek().span());
          break;
        default:
          if (first_binding) {
            this->error_reporter_->report_error_let_with_no_bindings(let_span);
          } else {
            assert(comma_span.has_value());
            this->error_reporter_->report_error_stray_comma_in_let_statement(
                *comma_span);
          }
          break;
      }
      first_binding = false;
    }

    if (this->peek().type == token_type::semicolon) {
      this->lexer_.skip();
    }
  }

  const token &peek() const noexcept { return this->lexer_.peek(); }

  lexer lexer_;
  quicklint_js::locator locator_;
  error_reporter *error_reporter_;
};
}  // namespace quicklint_js

#endif
