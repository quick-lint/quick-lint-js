#ifndef QUICKLINT_JS_ERROR_H
#define QUICKLINT_JS_ERROR_H

#include <quicklint-js/lex.h>
#include <quicklint-js/location.h>

namespace quicklint_js {
class error_reporter {
 public:
  virtual void report_error_invalid_binding_in_let_statement(
      source_code_span where) = 0;
  virtual void report_error_let_with_no_bindings(source_code_span where) = 0;
  virtual void report_error_missing_oprand_for_operator(
      source_code_span where) = 0;
  virtual void report_error_stray_comma_in_let_statement(
      source_code_span where) = 0;
  virtual void report_error_unclosed_block_comment(
      source_code_span comment_open) = 0;
  virtual void report_error_unclosed_string_literal(
      source_code_span string_literal) = 0;
  virtual void report_error_unexpected_identifier(source_code_span where) = 0;
  virtual void report_error_unmatched_parenthesis(source_code_span where) = 0;
  virtual void report_error_variable_used_before_declaration(
      identifier name) = 0;
};

class null_error_reporter : public error_reporter {
 public:
  static null_error_reporter instance;

  void report_error_invalid_binding_in_let_statement(
      source_code_span) override {}
  void report_error_let_with_no_bindings(source_code_span) override {}
  void report_error_missing_oprand_for_operator(source_code_span) override {}
  void report_error_stray_comma_in_let_statement(source_code_span) override {}
  void report_error_unclosed_block_comment(source_code_span) override {}
  void report_error_unclosed_string_literal(source_code_span) override {}
  void report_error_unexpected_identifier(source_code_span) override {}
  void report_error_unmatched_parenthesis(source_code_span) override {}
  void report_error_variable_used_before_declaration(identifier) override {}
};
inline null_error_reporter null_error_reporter::instance;

}  // namespace quicklint_js

#endif
