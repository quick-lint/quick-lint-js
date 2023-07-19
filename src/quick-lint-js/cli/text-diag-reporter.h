// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CLI_TEXT_DIAG_REPORTER_H
#define QUICK_LINT_JS_CLI_TEXT_DIAG_REPORTER_H

#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
class Text_Diag_Formatter;

class Text_Diag_Reporter final : public Diag_Reporter {
 public:
  explicit Text_Diag_Reporter(Translator, Output_Stream *output,
                              bool escape_errors);

  void set_source(Padded_String_View input, const char *file_name);

  void report_impl(Diag_Type type, void *diag) override;

 private:
  Output_Stream &output_;
  Translator translator_;
  std::optional<CLI_Locator> locator_;
  const char *file_path_;
  bool format_escape_errors_;
};

class Text_Diag_Formatter : public Diagnostic_Formatter<Text_Diag_Formatter> {
 public:
  explicit Text_Diag_Formatter(Translator, Output_Stream *output,
                               const char *file_path, CLI_Locator &locator,
                               bool format_escape_errors);

  void write_before_message(std::string_view code, Diagnostic_Severity,
                            const Source_Code_Span &origin);
  void write_message_part(std::string_view code, Diagnostic_Severity,
                          String8_View);
  void write_after_message(std::string_view code, Diagnostic_Severity,
                           const Source_Code_Span &origin);

 private:
  Output_Stream &output_;
  const char *file_path_;
  CLI_Locator &locator_;
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
