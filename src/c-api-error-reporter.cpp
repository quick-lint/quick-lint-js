// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <memory>
#include <quick-lint-js/c-api-error-reporter.h>
#include <quick-lint-js/c-api.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/sublime-text-3-location.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/web-demo-location.h>
#include <string>

namespace quick_lint_js {
template <class Diagnostic, class Locator>
c_api_error_reporter<Diagnostic, Locator>::c_api_error_reporter() = default;

template <class Diagnostic, class Locator>
void c_api_error_reporter<Diagnostic, Locator>::set_input(
    padded_string_view input, const Locator *locator) {
  this->locator_ = locator;
  this->input_ = input.data();
}

template <class Diagnostic, class Locator>
void c_api_error_reporter<Diagnostic, Locator>::reset() {
  this->diagnostics_.clear();
  // TODO(strager): Release allocated string memory.
}

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call)      \
  template <class Diagnostic, class Locator>                       \
  void c_api_error_reporter<Diagnostic, Locator>::report(name e) { \
    format_error(e, this->format(code));                           \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

template <class Diagnostic, class Locator>
const Diagnostic *c_api_error_reporter<Diagnostic, Locator>::get_diagnostics() {
  // Null-terminate the returned errors.
  this->diagnostics_.emplace_back();

  return this->diagnostics_.data();
}

template <class Diagnostic, class Locator>
c_api_error_formatter<Diagnostic, Locator>
c_api_error_reporter<Diagnostic, Locator>::format(const char *code) {
  return c_api_error_formatter(this, /*code=*/code);
}

template <class Diagnostic, class Locator>
char8 *c_api_error_reporter<Diagnostic, Locator>::allocate_c_string(
    string8_view string) {
  char8 *result =
      this->string_allocator_.template allocate_uninitialized_array<char8>(
          string.size() + 1);
  std::uninitialized_copy(string.begin(), string.end(), result);
  result[string.size()] = u8'\0';
  return result;
}

template <class Diagnostic, class Locator>
c_api_error_formatter<Diagnostic, Locator>::c_api_error_formatter(
    c_api_error_reporter<Diagnostic, Locator> *reporter, const char *code)
    : reporter_(reporter), code_(code) {}

template <class Diagnostic, class Locator>
void c_api_error_formatter<Diagnostic, Locator>::write_before_message(
    severity sev, const source_code_span &) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }
  QLJS_ASSERT(this->current_message_.empty());
}

template <class Diagnostic, class Locator>
void c_api_error_formatter<Diagnostic, Locator>::write_message_part(
    severity sev, string8_view message) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }
  this->current_message_.append(message);
}

template <class Diagnostic, class Locator>
void c_api_error_formatter<Diagnostic, Locator>::write_after_message(
    severity sev, const source_code_span &origin) {
  qljs_severity diag_severity = qljs_severity_error;
  switch (sev) {
  case severity::note:
    // Don't write notes. Only write the main message.
    return;
  case severity::error:
    diag_severity = qljs_severity_error;
    break;
  case severity::warning:
    diag_severity = qljs_severity_warning;
    break;
  }
  Diagnostic &diag = this->reporter_->diagnostics_.emplace_back();
  if constexpr (std::is_same_v<Locator, lsp_locator>) {
    lsp_range r = this->reporter_->locator_->range(origin);
    diag.start_line = r.start.line;
    diag.start_character = r.start.character;
    diag.end_line = r.end.line;
    diag.end_character = r.end.character;
  } else if constexpr (std::is_same_v<Locator, sublime_text_3_locator>) {
    sublime_text_3_source_range r = this->reporter_->locator_->range(origin);
    diag.begin_offset = narrow_cast<int>(r.begin);
    diag.end_offset = narrow_cast<int>(r.end);
  } else {
    web_demo_source_range r = this->reporter_->locator_->range(origin);
    diag.begin_offset = narrow_cast<int>(r.begin);
    diag.end_offset = narrow_cast<int>(r.end);
  }
  diag.code = this->code_;
  diag.message = reinterpret_cast<const char *>(
      this->reporter_->allocate_c_string(this->current_message_));
  diag.severity = diag_severity;
}

template class c_api_error_formatter<qljs_vscode_diagnostic, lsp_locator>;
template class c_api_error_reporter<qljs_vscode_diagnostic, lsp_locator>;

template class c_api_error_formatter<qljs_web_demo_diagnostic,
                                     web_demo_locator>;
template class c_api_error_reporter<qljs_web_demo_diagnostic, web_demo_locator>;

template class c_api_error_formatter<qljs_sublime_text_3_diagnostic,
                                     sublime_text_3_locator>;
template class c_api_error_reporter<qljs_sublime_text_3_diagnostic,
                                    sublime_text_3_locator>;

template class c_api_error_formatter<qljs_sublime_text_4_diagnostic,
                                     lsp_locator>;
template class c_api_error_reporter<qljs_sublime_text_4_diagnostic,
                                    lsp_locator>;
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
