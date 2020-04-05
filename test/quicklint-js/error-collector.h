#ifndef QUICKLINT_JS_ERROR_COLLECTOR_H
#define QUICKLINT_JS_ERROR_COLLECTOR_H

#include <quicklint-js/error.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/location.h>
#include <vector>

namespace quicklint_js {
struct error_collector : public error_reporter {
  void report_error_invalid_binding_in_let_statement(
      source_code_span where) override {
    this->errors.emplace_back(
        error{error_invalid_binding_in_let_statement, where});
  }

  void report_error_let_with_no_bindings(source_code_span where) override {
    this->errors.emplace_back(error{error_let_with_no_bindings, where});
  }

  void report_error_missing_oprand_for_operator(
      source_code_span where) override {
    this->errors.emplace_back(error{error_missing_oprand_for_operator, where});
  }

  void report_error_stray_comma_in_let_statement(
      source_code_span where) override {
    this->errors.emplace_back(error{error_stray_comma_in_let_statement, where});
  }

  void report_error_unexpected_identifier(source_code_span where) override {
    this->errors.emplace_back(error{error_unexpected_identifier, where});
  }

  void report_error_unmatched_parenthesis(source_code_span where) override {
    this->errors.emplace_back(error{error_unmatched_parenthesis, where});
  }

  void report_error_variable_used_before_declaration(identifier name) override {
    this->errors.emplace_back(
        error{error_variable_used_before_declaration, name.span()});
  }

  enum error_kind {
    error_invalid_binding_in_let_statement,
    error_let_with_no_bindings,
    error_missing_oprand_for_operator,
    error_stray_comma_in_let_statement,
    error_unexpected_identifier,
    error_unmatched_parenthesis,
    error_variable_used_before_declaration,
  };
  struct error {
    error_kind kind;
    source_code_span where;
  };
  std::vector<error> errors;
};
}  // namespace quicklint_js

#endif
