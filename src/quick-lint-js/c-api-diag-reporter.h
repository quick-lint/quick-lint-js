// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdint>
#include <optional>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/web-demo-location.h>
#include <vector>

struct QLJS_Web_Demo_Diagnostic;

namespace quick_lint_js {
template <class Diagnostic, class Locator>
class C_API_Diag_Formatter;

template <class Diagnostic, class Locator>
class C_API_Diag_Reporter final : public Diag_Reporter {
 public:
  explicit C_API_Diag_Reporter();

  void set_input(Padded_String_View input);
  // Does not reset translator.
  void reset();

  void set_translator(Translator);

  const Diagnostic *get_diagnostics();

  void report_impl(Diag_Type type, void *diag) override;

 private:
  Char8 *allocate_c_string(String8_View);

  Translator translator_;
  std::vector<Diagnostic> diagnostics_;
  const Char8 *input_;
  std::optional<Locator> locator_;
  Monotonic_Allocator string_allocator_{
      "c_api_diag_reporter::string_allocator_"};

  friend C_API_Diag_Formatter<Diagnostic, Locator>;
};

template <class Diagnostic, class Locator>
class C_API_Diag_Formatter
    : public Diagnostic_Formatter<C_API_Diag_Formatter<Diagnostic, Locator>> {
 public:
  explicit C_API_Diag_Formatter(
      C_API_Diag_Reporter<Diagnostic, Locator> *reporter);

  void write_before_message(std::string_view code, Diagnostic_Severity,
                            const Source_Code_Span &origin);
  void write_message_part(std::string_view code, Diagnostic_Severity,
                          String8_View);
  void write_after_message(std::string_view code, Diagnostic_Severity,
                           const Source_Code_Span &origin);

 private:
  C_API_Diag_Reporter<Diagnostic, Locator> *reporter_;
  String8 current_message_;
};

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wweak-template-vtables")

extern template class C_API_Diag_Formatter<QLJS_Web_Demo_Diagnostic,
                                           Web_Demo_Locator>;
extern template class C_API_Diag_Reporter<QLJS_Web_Demo_Diagnostic,
                                          Web_Demo_Locator>;

QLJS_WARNING_POP
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
