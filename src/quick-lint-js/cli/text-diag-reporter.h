// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CLI_TEXT_DIAG_REPORTER_H
#define QUICK_LINT_JS_CLI_TEXT_DIAG_REPORTER_H

#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/diag-reporter.h>
#include <quick-lint-js/fe/diagnostic-formatter.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
class text_diag_formatter;

class text_diag_reporter final : public diag_reporter {
 public:
  explicit text_diag_reporter(translator, output_stream *output,
                              bool escape_errors);

  void set_source(padded_string_view input, const char *file_name);

  void report_impl(diag_type type, void *diag) override;

 private:
  output_stream &output_;
  translator translator_;
  std::optional<cli_locator> locator_;
  const char *file_path_;
  bool format_escape_errors_;
};

class text_diag_formatter : public diagnostic_formatter<text_diag_formatter> {
 public:
  explicit text_diag_formatter(translator, output_stream *output,
                               const char *file_path, cli_locator &locator,
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
