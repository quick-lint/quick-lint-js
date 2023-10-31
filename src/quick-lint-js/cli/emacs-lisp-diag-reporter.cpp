// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/cli/emacs-lisp-diag-reporter.h>
#include <quick-lint-js/cli/emacs-location.h>
#include <quick-lint-js/container/optional.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/enum-cast.h>

namespace quick_lint_js {
Emacs_Lisp_Diag_Reporter::Emacs_Lisp_Diag_Reporter(Translator t,
                                                   Output_Stream *output)
    : output_(*output), translator_(t) {
  this->output_.append_copy(u8'(');
}

void Emacs_Lisp_Diag_Reporter::finish() { this->output_.append_copy(u8')'); }

void Emacs_Lisp_Diag_Reporter::set_source(Padded_String_View input) {
  this->locator_.emplace(input);
}

void Emacs_Lisp_Diag_Reporter::report_impl(Diag_Type type, void *diag) {
  QLJS_ASSERT(this->locator_.has_value());
  Emacs_Lisp_Diag_Formatter formatter(this->translator_,
                                      /*output=*/&this->output_,
                                      /*locator=*/*this->locator_);
  formatter.format(get_diagnostic_info(type), diag);
}

Emacs_Lisp_Diag_Formatter::Emacs_Lisp_Diag_Formatter(Translator t,
                                                     Output_Stream *output,
                                                     Emacs_Locator &locator)
    : Diagnostic_Formatter(t), output_(*output), locator_(locator) {}

void Emacs_Lisp_Diag_Formatter::write_before_message(
    std::string_view code, Diagnostic_Severity sev,
    const Source_Code_Span &origin) {
  if (sev == Diagnostic_Severity::note) {
    return;
  }
  Emacs_Source_Range r = this->locator_.range(origin);
  Emacs_Source_Position::Offset_Type beg = r.begin().offset;
  Emacs_Source_Position::Offset_Type end = r.end().offset;
  this->output_.append_literal(u8"(("_sv);
  this->output_.append_decimal_integer(beg);
  this->output_.append_literal(u8" . "_sv);
  this->output_.append_decimal_integer(end);
  this->output_.append_literal(u8") "_sv);
  this->output_.append_decimal_integer(enum_to_int_cast(sev));
  this->output_.append_literal(u8" \""_sv);
  this->output_.append_copy(to_string8_view(code));
  this->output_.append_literal(u8"\" \""_sv);
}

namespace {
void write_elisp_stringp_escaped_message(Output_Stream &output,
                                         String8_View message) {
  for (const auto &v : message) {
    switch (v) {
    case '\\':
    case '"':
      output.append_copy(u8'\\');
    }
    output.append_copy(v);
  }
}
}

void Emacs_Lisp_Diag_Formatter::write_message_part(
    [[maybe_unused]] std::string_view code, Diagnostic_Severity sev,
    String8_View message) {
  if (sev == Diagnostic_Severity::note) {
    return;
  }
  write_elisp_stringp_escaped_message(this->output_, message);
}

void Emacs_Lisp_Diag_Formatter::write_after_message(
    [[maybe_unused]] std::string_view code, Diagnostic_Severity sev,
    const Source_Code_Span &) {
  if (sev == Diagnostic_Severity::note) {
    return;
  }
  this->output_.append_literal(u8"\")"_sv);
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
