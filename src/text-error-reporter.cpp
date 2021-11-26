// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <iostream>
#include <ostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/text-error-reporter.h>
#include <quick-lint-js/token.h>

#if !defined(_WIN32)
#include <unistd.h>
#endif

namespace quick_lint_js {
text_error_reporter::text_error_reporter(std::ostream &output)
    : output_(output), format_escape_errors_(this->use_escape_if_auto()) {}

text_error_reporter::text_error_reporter(std::ostream &output,
                                         escape_errors escape_errors)
    : output_(output) {
  switch (escape_errors) {
  case escape_errors::auto_:
    if (this->use_escape_if_auto())
      this->format_escape_errors_ = true;
    else
      this->format_escape_errors_ = false;
    break;
  case escape_errors::always:
    this->format_escape_errors_ = true;
    break;
  case escape_errors::never:
    this->format_escape_errors_ = false;
    break;
  }
}

bool text_error_reporter::use_escape_if_auto() {
#if defined(_WIN32)
  return false;
#else
  return this->output_.rdbuf() == std::cerr.rdbuf() && isatty(STDERR_FILENO);
#endif
}

void text_error_reporter::set_source(padded_string_view input,
                                     const char *file_path) {
  this->locator_.emplace(input);
  this->file_path_ = file_path;
}

void text_error_reporter::report_impl(error_type type, void *error) {
  QLJS_ASSERT(this->file_path_);
  QLJS_ASSERT(this->locator_.has_value());
  text_error_formatter formatter(
      /*output=*/this->output_,
      /*file_path=*/this->file_path_,
      /*locator=*/*this->locator_,
      /*format_escape_errors*/ this->format_escape_errors_);
  formatter.format(get_diagnostic_info(type), error);
}

text_error_formatter::text_error_formatter(std::ostream &output,
                                           const char *file_path,
                                           cli_locator &locator,
                                           bool format_escape_errors)
    : output_(output),
      file_path_(file_path),
      locator_(locator),
      format_escape_errors_(format_escape_errors) {}

void text_error_formatter::write_before_message(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    const source_code_span &origin) {
  cli_source_range r = this->locator_.range(origin);
  cli_source_position p = r.begin();
  this->output_ << this->file_path_ << ":" << p.line_number << ":"
                << p.column_number << ": ";
  switch (sev) {
  case diagnostic_severity::error:
    this->output_ << "error: ";
    break;
  case diagnostic_severity::note:
    this->output_ << "note: ";
    break;
  case diagnostic_severity::warning:
    this->output_ << "warning: ";
    break;
  }
}

void text_error_formatter::write_message_part(
    [[maybe_unused]] std::string_view code, diagnostic_severity,
    string8_view message) {
  this->output_ << out_string8(message);
}

void text_error_formatter::write_after_message(std::string_view code,
                                               diagnostic_severity,
                                               const source_code_span &) {
  if (this->format_escape_errors_) {
    this->output_ << " [\x1B]8;;https://quick-lint-js.com/errors/#" << code
                  << "\x1B\\" << code << "\x1B]8;;\x1B\\]\n";
  } else {
    this->output_ << " [" << code << "]\n";
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
