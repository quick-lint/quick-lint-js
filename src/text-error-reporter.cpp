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

#include <ostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/text-error-reporter.h>

namespace quick_lint_js {
text_error_reporter::text_error_reporter(std::ostream &output)
    : output_(output) {}

void text_error_reporter::set_source(padded_string_view input,
                                     const char *file_path) {
  this->locator_.emplace(input);
  this->file_path_ = file_path;
}

#define QLJS_ERROR_TYPE(name, struct_body, format_call) \
  void text_error_reporter::report(name e) { format_error(e, this->format()); }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

void text_error_reporter::report_fatal_error_unimplemented_character(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    const char8 *character) {
  error_reporter::write_fatal_error_unimplemented_character(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*character=*/character,
      /*locator=*/get(this->locator_),
      /*out=*/this->output_);
}

void text_error_reporter::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
  error_reporter::write_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/type,
      /*token_begin=*/token_begin,
      /*locator=*/get(this->locator_),
      /*out=*/this->output_);
}

text_error_formatter text_error_reporter::format() {
  QLJS_ASSERT(this->file_path_);
  QLJS_ASSERT(this->locator_.has_value());
  return text_error_formatter(/*output=*/this->output_,
                              /*file_path=*/this->file_path_,
                              /*locator=*/*this->locator_);
}

text_error_formatter::text_error_formatter(std::ostream &output,
                                           const char *file_path,
                                           quick_lint_js::locator &locator)
    : output_(output), file_path_(file_path), locator_(locator) {}

void text_error_formatter::write_before_message(
    severity sev, const source_code_span &origin) {
  source_range r = this->locator_.range(origin);
  source_position p = r.begin();
  this->output_ << this->file_path_ << ":" << p.line_number << ":"
                << p.column_number << ": ";
  switch (sev) {
    case severity::error:
      this->output_ << "error: ";
      break;
    case severity::note:
      this->output_ << "note: ";
      break;
  }
}

void text_error_formatter::write_message_part(severity, string8_view message) {
  this->output_ << out_string8(message);
}

void text_error_formatter::write_after_message(severity,
                                               const source_code_span &) {
  this->output_ << '\n';
}
}
