// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <iostream>
#include <memory>
#include <quick-lint-js/c-api.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/web-demo-error-reporter-2.h>
#include <string>

namespace quick_lint_js {
web_demo_error_reporter_2::web_demo_error_reporter_2() = default;

void web_demo_error_reporter_2::set_input(padded_string_view input,
                                          const web_demo_locator *locator) {
  this->locator_ = locator;
  this->input_ = input.data();
}

void web_demo_error_reporter_2::reset() {
  this->diagnostics_.clear();
  // TODO(strager): Release allocated string memory.
}

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  void web_demo_error_reporter_2::report(name e) {            \
    format_error(e, this->format(code));                      \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

const qljs_web_demo_diagnostic *web_demo_error_reporter_2::get_diagnostics() {
  // Null-terminate the returned errors.
  this->diagnostics_.emplace_back();

  return this->diagnostics_.data();
}

web_demo_error_formatter_2 web_demo_error_reporter_2::format(const char *code) {
  return web_demo_error_formatter_2(this, /*code=*/code);
}

char8 *web_demo_error_reporter_2::allocate_c_string(string8_view string) {
  char8 *result = this->string_allocator_.allocate_uninitialized_array<char8>(
      string.size() + 1);
  std::uninitialized_copy(string.begin(), string.end(), result);
  result[string.size()] = u8'\0';
  return result;
}

web_demo_error_formatter_2::web_demo_error_formatter_2(
    web_demo_error_reporter_2 *reporter, const char *code)
    : reporter_(reporter), code_(code) {}

void web_demo_error_formatter_2::write_before_message(
    severity sev, const source_code_span &) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }
  QLJS_ASSERT(this->current_message_.empty());
}

void web_demo_error_formatter_2::write_message_part(severity sev,
                                                    string8_view message) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }
  this->current_message_.append(message);
}

void web_demo_error_formatter_2::write_after_message(
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
  qljs_web_demo_diagnostic &diag = this->reporter_->diagnostics_.emplace_back();
  web_demo_source_range r = this->reporter_->locator_->range(origin);
  diag.begin_offset = narrow_cast<int>(r.begin);
  diag.end_offset = narrow_cast<int>(r.end);
  diag.code = this->code_;
  diag.message = reinterpret_cast<const char *>(
      this->reporter_->allocate_c_string(this->current_message_));
  diag.severity = diag_severity;
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
