// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/emacs-lisp-error-reporter.h>
#include <quick-lint-js/emacs-location.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>

namespace quick_lint_js {
emacs_lisp_error_reporter::emacs_lisp_error_reporter(std::ostream &output)
    : output_(output) {
  this->output_ << "(";
}

void emacs_lisp_error_reporter::finish() { this->output_ << ")"; }

void emacs_lisp_error_reporter::set_source(padded_string_view input) {
  this->locator_.emplace(input);
}

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  void emacs_lisp_error_reporter::report(name e) {            \
    format_error(e, this->format(code));                      \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

emacs_lisp_error_formatter emacs_lisp_error_reporter::format(const char *code) {
  QLJS_ASSERT(this->locator_.has_value());
  return emacs_lisp_error_formatter(/*output=*/this->output_,
                                    /*locator=*/*this->locator_,
                                    /*code=*/code);
}

emacs_lisp_error_formatter::emacs_lisp_error_formatter(std::ostream &output,
                                                       emacs_locator &locator,
                                                       const char *code)
    : output_(output), locator_(locator), code_(code) {}

void emacs_lisp_error_formatter::write_before_message(
    severity sev, const source_code_span &origin) {
  if (sev == severity::note) {
    return;
  }
  emacs_source_range r = this->locator_.range(origin);
  emacs_source_position::offset_type beg = r.begin().offset;
  emacs_source_position::offset_type end = r.end().offset;
  this->output_ << "((" << beg << " . " << end << ") " << static_cast<int>(sev)
                << " "
                << "\"" << this->code_ << "\" \"";
}

namespace {
void write_elisp_stringp_escaped_message(std::ostream &output,
                                         string8_view message) {
  for (const auto &v : message) {
    switch (v) {
    case '\\':
    case '"':
      output << '\\';
    }
    output << static_cast<char>(v);
  }
}
}

void emacs_lisp_error_formatter::write_message_part(severity sev,
                                                    string8_view message) {
  if (sev == severity::note) {
    return;
  }
  write_elisp_stringp_escaped_message(this->output_, message);
}

void emacs_lisp_error_formatter::write_after_message(severity sev,
                                                     const source_code_span &) {
  if (sev == severity::note) {
    return;
  }
  this->output_ << "\")";
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
