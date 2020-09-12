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

#ifndef QUICK_LINT_JS_ERROR_H
#define QUICK_LINT_JS_ERROR_H

#include <cstddef>
#include <initializer_list>
#include <iosfwd>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/unreachable.h>
#include <utility>

namespace quick_lint_js {
class error_reporter {
 public:
  error_reporter() noexcept = default;

  error_reporter(const error_reporter &) noexcept = default;
  error_reporter &operator=(const error_reporter &) noexcept = default;

  error_reporter(error_reporter &&) noexcept = default;
  error_reporter &operator=(error_reporter &&) noexcept = default;

  virtual ~error_reporter() = default;

  virtual void report_error_assignment_before_variable_declaration(
      identifier assignment, identifier declaration) = 0;
  virtual void report_error_assignment_to_const_global_variable(
      identifier assignment) = 0;
  virtual void report_error_assignment_to_const_variable(
      identifier declaration, identifier assignment,
      variable_kind var_kind) = 0;
  virtual void report_error_assignment_to_undeclared_variable(
      identifier assignment) = 0;
  virtual void report_error_big_int_literal_contains_decimal_point(
      source_code_span where) = 0;
  virtual void report_error_big_int_literal_contains_exponent(
      source_code_span where) = 0;
  virtual void report_error_big_int_literal_contains_leading_zero(
      source_code_span where) = 0;
  virtual void report_error_invalid_binding_in_let_statement(
      source_code_span where) = 0;
  virtual void report_error_invalid_expression_left_of_assignment(
      source_code_span where) = 0;
  virtual void report_error_let_with_no_bindings(source_code_span where) = 0;
  virtual void report_error_missing_comma_between_object_literal_entries(
      source_code_span where) = 0;
  virtual void report_error_missing_operand_for_operator(
      source_code_span where) = 0;
  virtual void report_error_missing_semicolon_after_expression(
      source_code_span where) = 0;
  virtual void report_error_redeclaration_of_global_variable(
      identifier redeclaration) = 0;
  virtual void report_error_redeclaration_of_variable(
      identifier redeclaration, identifier original_declaration) = 0;
  virtual void report_error_stray_comma_in_let_statement(
      source_code_span where) = 0;
  virtual void report_error_unclosed_block_comment(
      source_code_span comment_open) = 0;
  virtual void report_error_unclosed_regexp_literal(
      source_code_span regexp_literal) = 0;
  virtual void report_error_unclosed_string_literal(
      source_code_span string_literal) = 0;
  virtual void report_error_unclosed_template(
      source_code_span incomplete_template) = 0;
  virtual void report_error_unexpected_characters_in_number(
      source_code_span characters) = 0;
  virtual void report_error_unexpected_hash_character(
      source_code_span where) = 0;
  virtual void report_error_unexpected_identifier(source_code_span where) = 0;
  virtual void report_error_unmatched_parenthesis(source_code_span where) = 0;
  virtual void report_error_use_of_undeclared_variable(identifier name) = 0;
  virtual void report_error_variable_used_before_declaration(
      identifier use, identifier declaration) = 0;

  virtual void report_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character) = 0;
  virtual void report_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin) = 0;

  static void write_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character, const locator *, std::ostream &);
  static void write_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin, const locator *, std::ostream &);
};

class null_error_reporter : public error_reporter {
 public:
  static null_error_reporter instance;

  void report_error_assignment_before_variable_declaration(
      identifier, identifier) override {}
  void report_error_assignment_to_const_global_variable(identifier) override {}
  void report_error_assignment_to_const_variable(identifier, identifier,
                                                 variable_kind) override {}
  void report_error_assignment_to_undeclared_variable(identifier) override {}
  void report_error_big_int_literal_contains_decimal_point(
      source_code_span) override {}
  void report_error_big_int_literal_contains_exponent(
      source_code_span) override {}
  void report_error_big_int_literal_contains_leading_zero(
      source_code_span) override {}
  void report_error_invalid_binding_in_let_statement(
      source_code_span) override {}
  void report_error_invalid_expression_left_of_assignment(
      source_code_span) override {}
  void report_error_let_with_no_bindings(source_code_span) override {}
  void report_error_missing_comma_between_object_literal_entries(
      source_code_span) override {}
  void report_error_missing_operand_for_operator(source_code_span) override {}
  void report_error_missing_semicolon_after_expression(
      source_code_span) override {}
  void report_error_redeclaration_of_global_variable(identifier) override {}
  void report_error_redeclaration_of_variable(identifier, identifier) override {
  }
  void report_error_stray_comma_in_let_statement(source_code_span) override {}
  void report_error_unclosed_block_comment(source_code_span) override {}
  void report_error_unclosed_regexp_literal(source_code_span) override {}
  void report_error_unclosed_string_literal(source_code_span) override {}
  void report_error_unclosed_template(source_code_span) override {}
  void report_error_unexpected_characters_in_number(source_code_span) override {
  }
  void report_error_unexpected_hash_character(source_code_span) override {}
  void report_error_unexpected_identifier(source_code_span) override {}
  void report_error_unmatched_parenthesis(source_code_span) override {}
  void report_error_use_of_undeclared_variable(identifier) override {}
  void report_error_variable_used_before_declaration(identifier,
                                                     identifier) override {}

  void report_fatal_error_unimplemented_character(const char *, int,
                                                  const char *,
                                                  const char8 *) override {}
  void report_fatal_error_unimplemented_token(const char *, int, const char *,
                                              token_type,
                                              const char8 *) override {}
};
inline null_error_reporter null_error_reporter::instance;

template <class Derived>
class error_formatter {
 public:
  // Assumed member functions in Derived:
  // void write_before_message(severity, const source_code_span &origin);
  // void write_message_part(severity, string8_view);
  // void write_after_message(severity, const source_code_span &origin);

  enum class severity {
    error,
    note,
  };

  template <class... Args>
  error_formatter &error(const char8 *message, Args... parameters) {
    this->add(severity::error, message, std::forward<Args>(parameters)...);
    return *this;
  }

  template <class... Args>
  error_formatter &note(const char8 *message, Args &&... parameters) {
    this->add(severity::note, message, std::forward<Args>(parameters)...);
    return *this;
  }

  void end() {}

 private:
  template <class... Args>
  void add(severity sev, const char8 *message, Args &&... parameters) {
    static_assert(sizeof...(Args) > 0,
                  "at least origin span must be specified");
    this->add(sev, message, {this->to_span(std::forward<Args>(parameters))...});
  }

  void add(severity, const char8 *message,
           std::initializer_list<source_code_span> parameters);

  static const source_code_span &to_span(const source_code_span &span) {
    return span;
  }

  static source_code_span to_span(identifier ident) { return ident.span(); }
};

template <class Derived>
inline void error_formatter<Derived>::add(
    severity sev, const char8 *message,
    std::initializer_list<source_code_span> parameters) {
  static constexpr auto npos = string8_view::npos;
  using string8_pos = string8_view::size_type;
  QLJS_ASSERT(message);
  QLJS_ASSERT(!std::empty(parameters));

  Derived *self = static_cast<Derived *>(this);

  const source_code_span &origin_span = *parameters.begin();
  self->write_before_message(sev, origin_span);

  string8_view remaining_message(message);
  string8_pos left_curly_index;
  while ((left_curly_index = remaining_message.find(u8'{')) != npos) {
    self->write_message_part(sev,
                             remaining_message.substr(0, left_curly_index));

    string8_pos right_curly_index =
        remaining_message.find(u8'}', left_curly_index + 1);
    QLJS_ASSERT(right_curly_index != npos &&
                "invalid message format: missing }");
    string8_view curly_content = remaining_message.substr(
        left_curly_index + 1, right_curly_index - (left_curly_index + 1));
    std::size_t index;
    if (curly_content == u8"0") {
      index = 0;
    } else if (curly_content == u8"1") {
      index = 1;
    } else if (curly_content == u8"2") {
      index = 2;
    } else {
      QLJS_ASSERT(false && "invalid message format: unrecognized placeholder");
      QLJS_UNREACHABLE();
    }

    self->write_message_part(sev, (parameters.begin() + index)->string_view());
    remaining_message = remaining_message.substr(right_curly_index + 1);
  }
  self->write_message_part(sev, remaining_message);

  self->write_after_message(sev, origin_span);
}
}

#endif
