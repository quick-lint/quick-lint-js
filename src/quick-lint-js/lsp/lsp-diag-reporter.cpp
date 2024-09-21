// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/optional.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp/lsp-diag-reporter.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <string>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
LSP_Diag_Reporter::LSP_Diag_Reporter(Translator t, Byte_Buffer &output,
                                     Padded_String_View input)
    : output_(output), locator_(input), translator_(t) {
  this->output_.append_copy(u8"["_sv);
}

void LSP_Diag_Reporter::finish() { this->output_.append_copy(u8"]"_sv); }

void LSP_Diag_Reporter::report(const Diag_List &diags) {
  diags.for_each([&](Diag_Type type, void *diag) -> void {
    if (this->need_comma_) {
      this->output_.append_copy(u8",\n"_sv);
    }
    this->need_comma_ = true;
    LSP_Diag_Formatter formatter(/*output=*/this->output_,
                                 /*locator=*/this->locator_, this->translator_);
    formatter.format(get_diagnostic_info(type), diag);
  });
}

LSP_Diag_Formatter::LSP_Diag_Formatter(Byte_Buffer &output,
                                       LSP_Locator &locator, Translator t)
    : Diagnostic_Formatter(t), output_(output), locator_(locator) {}

void LSP_Diag_Formatter::write_before_message(std::string_view code,
                                              Diagnostic_Severity sev,
                                              const Source_Code_Span &origin) {
  Char8 severity_type{};
  switch (sev) {
  case Diagnostic_Severity::error:
    severity_type = u8'1';
    break;
  case Diagnostic_Severity::note:
    // Don't write notes. Only write the main message.
    return;
  case Diagnostic_Severity::warning:
    severity_type = u8'2';
    break;
  }

  LSP_Range r = this->locator_.range(origin);
  this->output_.append_copy(
      u8"{\"range\":{\"start\":"_sv
      u8"{\"line\":"_sv);
  this->output_.append_decimal_integer(r.start.line);
  this->output_.append_copy(u8",\"character\":"_sv);
  this->output_.append_decimal_integer(r.start.character);
  this->output_.append_copy(
      u8"},\"end\":"_sv
      u8"{\"line\":"_sv);
  this->output_.append_decimal_integer(r.end.line);
  this->output_.append_copy(u8",\"character\":"_sv);
  this->output_.append_decimal_integer(r.end.character);
  this->output_.append_copy(u8"}},\"severity\":"_sv);
  this->output_.append_copy(severity_type);
  this->output_.append_copy(u8",\"code\":\""_sv);
  this->output_.append_copy(to_string8_view(code));
  this->output_.append_copy(
      u8"\",\"codeDescription\":"_sv
      u8"{\"href\":\"https://quick-lint-js.com/errors/"_sv);
  this->output_.append_copy(to_string8_view(code));
  this->output_.append_copy(
      u8"/\"},\"source\":\"quick-lint-js\""_sv
      u8",\"message\":\""_sv);
}

void LSP_Diag_Formatter::write_message_part(
    [[maybe_unused]] std::string_view code, Diagnostic_Severity sev,
    String8_View message) {
  if (sev == Diagnostic_Severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  write_json_escaped_string(this->output_, message);
}

void LSP_Diag_Formatter::write_after_message(
    [[maybe_unused]] std::string_view code, Diagnostic_Severity sev,
    const Source_Code_Span &) {
  if (sev == Diagnostic_Severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  this->output_.append_copy(u8"\"}"_sv);
}
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
