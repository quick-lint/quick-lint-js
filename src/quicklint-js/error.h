#ifndef QUICKLINT_JS_ERROR_H
#define QUICKLINT_JS_ERROR_H

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
  virtual void report_error_unexpected_identifier(source_code_span where) = 0;
  virtual void report_error_unmatched_parenthesis(source_code_span where) = 0;
};
}  // namespace quicklint_js

#endif
