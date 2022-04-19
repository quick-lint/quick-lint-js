// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/emacs-lisp-diag-reporter.h>
#include <quick-lint-js/emacs-location.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/output-stream.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>

namespace quick_lint_js {
emacs_lisp_diag_reporter::emacs_lisp_diag_reporter(output_stream *output)
    : output_(*output) {
  this->output_.append_copy(u8'(');
}

void emacs_lisp_diag_reporter::finish() { this->output_.append_copy(u8')'); }

void emacs_lisp_diag_reporter::set_source(padded_string_view input) {
  this->locator_.emplace(input);
}

void emacs_lisp_diag_reporter::report_impl(diag_type type, void *error) {
  QLJS_ASSERT(this->locator_.has_value());
  emacs_lisp_error_formatter formatter(/*output=*/&this->output_,
                                       /*locator=*/*this->locator_);
  formatter.format(get_diagnostic_info(type), error);
}

emacs_lisp_error_formatter::emacs_lisp_error_formatter(output_stream *output,
                                                       emacs_locator &locator)
    : output_(*output), locator_(locator) {}

void emacs_lisp_error_formatter::write_before_message(
    std::string_view code, diagnostic_severity sev,
    const source_code_span &origin) {
  if (sev == diagnostic_severity::note) {
    return;
  }
  emacs_source_range r = this->locator_.range(origin);
  emacs_source_position::offset_type beg = r.begin().offset;
  emacs_source_position::offset_type end = r.end().offset;
  this->output_.append_literal(u8"(("sv);
  this->output_.append_decimal_integer(beg);
  this->output_.append_literal(u8" . "sv);
  this->output_.append_decimal_integer(end);
  this->output_.append_literal(u8") "sv);
  this->output_.append_decimal_integer(static_cast<int>(sev));
  this->output_.append_literal(u8" \""sv);
  this->output_.append_copy(to_string8_view(code));
  this->output_.append_literal(u8"\" \""sv);
}

namespace {
void write_elisp_stringp_escaped_message(output_stream &output,
                                         string8_view message) {
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

void emacs_lisp_error_formatter::write_message_part(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    string8_view message) {
  if (sev == diagnostic_severity::note) {
    return;
  }
  write_elisp_stringp_escaped_message(this->output_, message);
}

void emacs_lisp_error_formatter::write_after_message(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    const source_code_span &) {
  if (sev == diagnostic_severity::note) {
    return;
  }
  this->output_.append_literal(u8"\")"sv);
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
