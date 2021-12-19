// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TEXT_ERROR_REPORTER_H
#define QUICK_LINT_JS_TEXT_ERROR_REPORTER_H

#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diagnostic-formatter.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/output-stream.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>

namespace quick_lint_js {
class text_error_formatter;

class text_error_reporter final : public error_reporter {
 public:
  explicit text_error_reporter(output_stream *output, bool escape_errors);

  void set_source(padded_string_view input, const char *file_name);

  void report_impl(error_type type, void *error) override;

 private:
  output_stream &output_;
  std::optional<cli_locator> locator_;
  const char *file_path_;
  bool format_escape_errors_;
};

class text_error_formatter : public diagnostic_formatter<text_error_formatter> {
 public:
  explicit text_error_formatter(output_stream *output, const char *file_path,
                                cli_locator &locator,
                                bool format_escape_errors);

  void write_before_message(std::string_view code, diagnostic_severity,
                            const source_code_span &origin);
  void write_message_part(std::string_view code, diagnostic_severity,
                          string8_view);
  void write_after_message(std::string_view code, diagnostic_severity,
                           const source_code_span &origin);

 private:
  output_stream &output_;
  const char *file_path_;
  cli_locator &locator_;
  bool format_escape_errors_;
};
}

#endif

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
