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
Text_Diag_Reporter::Text_Diag_Reporter(Translator t, Output_Stream *output,
                                       bool escape_errors)
    : output_(*output), translator_(t), format_escape_errors_(escape_errors) {}

void Text_Diag_Reporter::set_source(Padded_String_View input,
                                    const char *file_path) {
  this->locator_.emplace(input);
  this->file_path_ = file_path;
}

void Text_Diag_Reporter::report(const Diag_List &diags) {
  QLJS_ASSERT(this->file_path_);
  QLJS_ASSERT(this->locator_.has_value());
  diags.for_each([&](Diag_Type type, void *diag) -> void {
    Text_Diag_Formatter formatter(
        this->translator_,
        /*output=*/&this->output_,
        /*file_path=*/this->file_path_,
        /*locator=*/*this->locator_,
        /*format_escape_errors=*/this->format_escape_errors_);
    formatter.format(get_diagnostic_info(type), diag);
  });
}

Text_Diag_Formatter::Text_Diag_Formatter(Translator t, Output_Stream *output,
                                         const char *file_path,
                                         CLI_Locator &locator,
                                         bool format_escape_errors)
    : Diagnostic_Formatter(t),
      output_(*output),
      file_path_(file_path),
      locator_(locator),
      format_escape_errors_(format_escape_errors) {}

void Text_Diag_Formatter::write_before_message(
    [[maybe_unused]] std::string_view code, Diagnostic_Severity sev,
    const Source_Code_Span &origin) {
  CLI_Source_Range r = this->locator_.range(origin);
  CLI_Source_Position p = r.begin();
  this->output_.append_copy(to_string8_view(this->file_path_));
  this->output_.append_copy(u8':');
  this->output_.append_decimal_integer(p.line_number);
  this->output_.append_copy(u8':');
  this->output_.append_decimal_integer(p.column_number);
  this->output_.append_literal(u8": "_sv);
  switch (sev) {
  case Diagnostic_Severity::error:
    this->output_.append_literal(u8"error: "_sv);
    break;
  case Diagnostic_Severity::note:
    this->output_.append_literal(u8"note: "_sv);
    break;
  case Diagnostic_Severity::warning:
    this->output_.append_literal(u8"warning: "_sv);
    break;
  }
}

void Text_Diag_Formatter::write_message_part(
    [[maybe_unused]] std::string_view code, Diagnostic_Severity,
    String8_View message) {
  this->output_.append_copy(message);
}

void Text_Diag_Formatter::write_after_message(std::string_view code,
                                              Diagnostic_Severity,
                                              const Source_Code_Span &) {
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
