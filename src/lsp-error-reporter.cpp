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

#include <iostream>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-error-reporter.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/unreachable.h>
#include <string>

namespace quick_lint_js {
lsp_error_reporter::lsp_error_reporter(byte_buffer &output,
                                       padded_string_view input)
    : output_(output), locator_(input) {
  this->output_.append_copy(u8"[");
}

void lsp_error_reporter::finish() { this->output_.append_copy(u8"]"); }

#define QLJS_ERROR_TYPE(name, struct_body, format_call) \
  void lsp_error_reporter::report(name e) {             \
    this->begin_error();                                \
    format_error(e, this->format());                    \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

void lsp_error_reporter::report_fatal_error_unimplemented_character(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    const char8 *character) {
  error_reporter::write_fatal_error_unimplemented_character(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*character=*/character,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

void lsp_error_reporter::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
  error_reporter::write_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/type,
      /*token_begin=*/token_begin,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

void lsp_error_reporter::begin_error() {
  if (this->need_comma_) {
    this->output_.append_copy(u8",\n");
  }
  this->need_comma_ = true;
}

lsp_error_formatter lsp_error_reporter::format() {
  return lsp_error_formatter(/*output=*/this->output_,
                             /*locator=*/this->locator_);
}

lsp_error_formatter::lsp_error_formatter(byte_buffer &output,
                                         lsp_locator &locator)
    : output_(output), locator_(locator) {}

void lsp_error_formatter::write_before_message(severity sev,
                                               const source_code_span &origin) {
  char8 severity_type;
  switch (sev) {
  case severity::error:
    severity_type = u8'1';
    break;
  case severity::note:
    // Don't write notes. Only write the main message.
    return;
  case severity::warning:
    severity_type = u8'2';
    break;
  }

  lsp_range r = this->locator_.range(origin);
  this->output_.append_copy(u8"{\"range\":{\"start\":");
  this->output_.append_copy(u8"{\"line\":");
  this->output_.append_decimal_integer(r.start.line);
  this->output_.append_copy(u8",\"character\":");
  this->output_.append_decimal_integer(r.start.character);
  this->output_.append_copy(u8"},\"end\":");
  this->output_.append_copy(u8"{\"line\":");
  this->output_.append_decimal_integer(r.end.line);
  this->output_.append_copy(u8",\"character\":");
  this->output_.append_decimal_integer(r.end.character);
  this->output_.append_copy(u8"}},\"severity\":");
  this->output_.append_copy(severity_type);
  this->output_.append_copy(u8",\"message\":\"");
}

void lsp_error_formatter::write_message_part(severity sev,
                                             string8_view message) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  write_json_escaped_string(this->output_, message);
}

void lsp_error_formatter::write_after_message(severity sev,
                                              const source_code_span &) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  this->output_.append_copy(u8"\"}");
}
}
