// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <memory>
#include <quick-lint-js/c-api-diag-reporter.h>
#include <quick-lint-js/c-api.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/optional.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/web-demo-location.h>
#include <string>

namespace quick_lint_js {
template <class Diagnostic, class Locator>
C_API_Diag_Reporter<Diagnostic, Locator>::C_API_Diag_Reporter() = default;

template <class Diagnostic, class Locator>
void C_API_Diag_Reporter<Diagnostic, Locator>::set_input(
    Padded_String_View input) {
  this->input_ = input.data();
  this->locator_.emplace(input);
}

template <class Diagnostic, class Locator>
void C_API_Diag_Reporter<Diagnostic, Locator>::reset() {
  this->diagnostics_.clear();
  // TODO(strager): Release allocated string memory.
}

template <class Diagnostic, class Locator>
void C_API_Diag_Reporter<Diagnostic, Locator>::set_translator(Translator t) {
  this->translator_ = t;
}

template <class Diagnostic, class Locator>
void C_API_Diag_Reporter<Diagnostic, Locator>::report_impl(Diag_Type type,
                                                           void *diag) {
  C_API_Diag_Formatter formatter(this);
  formatter.format(get_diagnostic_info(type), diag);
}

template <class Diagnostic, class Locator>
const Diagnostic *C_API_Diag_Reporter<Diagnostic, Locator>::get_diagnostics() {
  // Null-terminate the returned diagnostics.
  this->diagnostics_.emplace_back();

  return this->diagnostics_.data();
}

template <class Diagnostic, class Locator>
Char8 *C_API_Diag_Reporter<Diagnostic, Locator>::allocate_c_string(
    String8_View string) {
  Char8 *result =
      this->string_allocator_.template allocate_uninitialized_array<Char8>(
          string.size() + 1);
  std::uninitialized_copy(string.begin(), string.end(), result);
  result[string.size()] = u8'\0';
  return result;
}

template <class Diagnostic, class Locator>
C_API_Diag_Formatter<Diagnostic, Locator>::C_API_Diag_Formatter(
    C_API_Diag_Reporter<Diagnostic, Locator> *reporter)
    : Diagnostic_Formatter<C_API_Diag_Formatter<Diagnostic, Locator>>(
          reporter->translator_),
      reporter_(reporter) {}

template <class Diagnostic, class Locator>
void C_API_Diag_Formatter<Diagnostic, Locator>::write_before_message(
    std::string_view, Diagnostic_Severity sev, const Source_Code_Span &) {
  if (sev == Diagnostic_Severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }
  QLJS_ASSERT(this->current_message_.empty());
}

template <class Diagnostic, class Locator>
void C_API_Diag_Formatter<Diagnostic, Locator>::write_message_part(
    std::string_view, Diagnostic_Severity sev, String8_View message) {
  if (sev == Diagnostic_Severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }
  this->current_message_.append(message);
}

template <class Diagnostic, class Locator>
void C_API_Diag_Formatter<Diagnostic, Locator>::write_after_message(
    std::string_view code, Diagnostic_Severity sev,
    const Source_Code_Span &origin) {
  QLJS_Severity diag_severity = qljs_severity_error;
  switch (sev) {
  case Diagnostic_Severity::note:
    // Don't write notes. Only write the main message.
    return;
  case Diagnostic_Severity::error:
    diag_severity = qljs_severity_error;
    break;
  case Diagnostic_Severity::warning:
    diag_severity = qljs_severity_warning;
    break;
  }
  Diagnostic &diag = this->reporter_->diagnostics_.emplace_back();
  if constexpr (std::is_same_v<Locator, LSP_Locator>) {
    LSP_Range r = this->reporter_->locator_->range(origin);
    diag.start_line = r.start.line;
    diag.start_character = r.start.character;
    diag.end_line = r.end.line;
    diag.end_character = r.end.character;
  } else {
    Web_Demo_Source_Range r = this->reporter_->locator_->range(origin);
    diag.begin_offset = narrow_cast<int>(r.begin);
    diag.end_offset = narrow_cast<int>(r.end);
  }

  char *code_end = std::copy(code.begin(), code.end(), &diag.code[0]);
  *code_end = '\0';

  diag.message = reinterpret_cast<const char *>(
      this->reporter_->allocate_c_string(this->current_message_));
  diag.severity = diag_severity;
}

template class C_API_Diag_Formatter<QLJS_Web_Demo_Diagnostic, Web_Demo_Locator>;
template class C_API_Diag_Reporter<QLJS_Web_Demo_Diagnostic, Web_Demo_Locator>;
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
