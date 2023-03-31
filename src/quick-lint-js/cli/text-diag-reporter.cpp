// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/cli/text-diag-reporter.h>
#include <quick-lint-js/container/optional.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <string_view>

#if !defined(_WIN32)
#include <unistd.h>
#endif

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
text_diag_reporter::text_diag_reporter(translator t, output_stream *output,
                                       bool escape_errors)
    : output_(*output), translator_(t), format_escape_errors_(escape_errors) {}

void text_diag_reporter::set_source(padded_string_view input,
                                    const char *file_path) {
  this->locator_.emplace(input);
  this->file_path_ = file_path;
}

void text_diag_reporter::report_impl(diag_type type, void *diag) {
  QLJS_ASSERT(this->file_path_);
  QLJS_ASSERT(this->locator_.has_value());
  text_diag_formatter formatter(
      this->translator_,
      /*output=*/&this->output_,
      /*file_path=*/this->file_path_,
      /*locator=*/*this->locator_,
      /*format_escape_errors=*/this->format_escape_errors_);
  formatter.format(get_diagnostic_info(type), diag);
}

text_diag_formatter::text_diag_formatter(translator t, output_stream *output,
                                         const char *file_path,
                                         cli_locator &locator,
                                         bool format_escape_errors)
    : diagnostic_formatter(t),
      output_(*output),
      file_path_(file_path),
      locator_(locator),
      format_escape_errors_(format_escape_errors) {}

void text_diag_formatter::write_before_message(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    const source_code_span &origin) {
  cli_source_range r = this->locator_.range(origin);
  cli_source_position p = r.begin();
  this->output_.append_copy(to_string8_view(this->file_path_));
  this->output_.append_copy(u8':');
  this->output_.append_decimal_integer(p.line_number);
  this->output_.append_copy(u8':');
  this->output_.append_decimal_integer(p.column_number);
  this->output_.append_literal(u8": "_sv);
  switch (sev) {
  case diagnostic_severity::error:
    this->output_.append_literal(u8"error: "_sv);
    break;
  case diagnostic_severity::note:
    this->output_.append_literal(u8"note: "_sv);
    break;
  case diagnostic_severity::warning:
    this->output_.append_literal(u8"warning: "_sv);
    break;
  }
}

void text_diag_formatter::write_message_part(
    [[maybe_unused]] std::string_view code, diagnostic_severity,
    string8_view message) {
  this->output_.append_copy(message);
}

void text_diag_formatter::write_after_message(std::string_view code,
                                              diagnostic_severity,
                                              const source_code_span &) {
  if (this->format_escape_errors_) {
    this->output_.append_copy(
        u8" [\x1B]8;;https://quick-lint-js.com/errors/"_sv);
    this->output_.append_copy(to_string8_view(code));
    this->output_.append_literal(u8"/\x1B\\"_sv);
    this->output_.append_copy(to_string8_view(code));
    this->output_.append_literal(u8"\x1B]8;;\x1B\\]\n"_sv);
  } else {
    this->output_.append_literal(u8" ["_sv);
    this->output_.append_copy(to_string8_view(code));
    this->output_.append_literal(u8"]\n"_sv);
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
