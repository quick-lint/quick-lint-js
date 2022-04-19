// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_C_API_ERROR_REPORTER_H
#define QUICK_LINT_JS_C_API_ERROR_REPORTER_H

#include <cstdint>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diagnostic-formatter.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/warning.h>
#include <vector>

struct qljs_web_demo_diagnostic;

namespace quick_lint_js {
class lsp_locator;
class web_demo_locator;

template <class Diagnostic, class Locator>
class c_api_error_formatter;

template <class Diagnostic, class Locator>
class c_api_error_reporter final : public error_reporter {
 public:
  explicit c_api_error_reporter();

  void set_input(padded_string_view input, const Locator *);
  void reset();

  const Diagnostic *get_diagnostics();

  void report_impl(diag_type type, void *error) override;

 private:
  char8 *allocate_c_string(string8_view);

  std::vector<Diagnostic> diagnostics_;
  const Locator *locator_;
  const char8 *input_;
  monotonic_allocator string_allocator_;

  friend c_api_error_formatter<Diagnostic, Locator>;
};

template <class Diagnostic, class Locator>
class c_api_error_formatter
    : public diagnostic_formatter<c_api_error_formatter<Diagnostic, Locator>> {
 public:
  explicit c_api_error_formatter(
      c_api_error_reporter<Diagnostic, Locator> *reporter);

  void write_before_message(std::string_view code, diagnostic_severity,
                            const source_code_span &origin);
  void write_message_part(std::string_view code, diagnostic_severity,
                          string8_view);
  void write_after_message(std::string_view code, diagnostic_severity,
                           const source_code_span &origin);

 private:
  c_api_error_reporter<Diagnostic, Locator> *reporter_;
  string8 current_message_;
};

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wweak-template-vtables")

extern template class c_api_error_formatter<qljs_web_demo_diagnostic,
                                            web_demo_locator>;
extern template class c_api_error_reporter<qljs_web_demo_diagnostic,
                                           web_demo_locator>;

QLJS_WARNING_POP
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
