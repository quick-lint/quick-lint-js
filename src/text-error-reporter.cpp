// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/output-stream.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/text-error-reporter.h>
#include <quick-lint-js/token.h>
#include <string_view>

#if !defined(_WIN32)
#include <unistd.h>
#endif

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
text_error_reporter::text_error_reporter(output_stream *output,
                                         bool escape_errors)
    : output_(*output), format_escape_errors_(escape_errors) {}

void text_error_reporter::set_source(padded_string_view input,
                                     const char *file_path) {
  this->locator_.emplace(input);
  this->file_path_ = file_path;
}

void text_error_reporter::report_impl(diag_type type, void *error) {
  QLJS_ASSERT(this->file_path_);
  QLJS_ASSERT(this->locator_.has_value());
  text_error_formatter formatter(
      /*output=*/&this->output_,
      /*file_path=*/this->file_path_,
      /*locator=*/*this->locator_,
      /*format_escape_errors=*/this->format_escape_errors_);
  formatter.format(get_diagnostic_info(type), error);
}

text_error_formatter::text_error_formatter(output_stream *output,
                                           const char *file_path,
                                           cli_locator &locator,
                                           bool format_escape_errors)
    : output_(*output),
      file_path_(file_path),
      locator_(locator),
      format_escape_errors_(format_escape_errors) {}

void text_error_formatter::write_before_message(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    const source_code_span &origin) {
  cli_source_range r = this->locator_.range(origin);
  cli_source_position p = r.begin();
  this->output_.append_copy(to_string8_view(this->file_path_));
  this->output_.append_copy(u8':');
  this->output_.append_decimal_integer(p.line_number);
  this->output_.append_copy(u8':');
  this->output_.append_decimal_integer(p.column_number);
  this->output_.append_literal(u8": "sv);
  switch (sev) {
  case diagnostic_severity::error:
    this->output_.append_literal(u8"error: "sv);
    break;
  case diagnostic_severity::note:
    this->output_.append_literal(u8"note: "sv);
    break;
  case diagnostic_severity::warning:
    this->output_.append_literal(u8"warning: "sv);
    break;
  }
}

void text_error_formatter::write_message_part(
    [[maybe_unused]] std::string_view code, diagnostic_severity,
    string8_view message) {
  this->output_.append_copy(message);
}

void text_error_formatter::write_after_message(std::string_view code,
                                               diagnostic_severity,
                                               const source_code_span &) {
  if (this->format_escape_errors_) {
    this->output_.append_copy(
        u8" [\x1B]8;;https://quick-lint-js.com/errors/"sv);
    this->output_.append_copy(to_string8_view(code));
    this->output_.append_literal(u8"/\x1B\\"sv);
    this->output_.append_copy(to_string8_view(code));
    this->output_.append_literal(u8"\x1B]8;;\x1B\\]\n"sv);
  } else {
    this->output_.append_literal(u8" ["sv);
    this->output_.append_copy(to_string8_view(code));
    this->output_.append_literal(u8"]\n"sv);
  }
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
