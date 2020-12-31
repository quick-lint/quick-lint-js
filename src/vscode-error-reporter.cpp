// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <boost/container/pmr/monotonic_buffer_resource.hpp>
#include <boost/container/pmr/polymorphic_allocator.hpp>
#include <iostream>
#include <memory>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vscode-error-reporter.h>
#include <quick-lint-js/vscode.h>
#include <string>

namespace quick_lint_js {
vscode_error_reporter::vscode_error_reporter() = default;

void vscode_error_reporter::set_input(padded_string_view input,
                                      const lsp_locator *locator) {
  this->locator_ = locator;
  this->input_ = input.data();
}

void vscode_error_reporter::reset() {
  this->diagnostics_.clear();
  // TODO(strager): Release allocated string memory.
}

#define QLJS_ERROR_TYPE(name, struct_body, format_call) \
  void vscode_error_reporter::report(name e) {          \
    format_error(e, this->format());                    \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

const qljs_vscode_diagnostic *vscode_error_reporter::get_diagnostics() {
  // Null-terminate the returned errors.
  this->diagnostics_.emplace_back();

  return this->diagnostics_.data();
}

void vscode_error_reporter::report_fatal_error_unimplemented_character(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    const char8 *character) {
  error_reporter::write_fatal_error_unimplemented_character(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*character=*/character,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

void vscode_error_reporter::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
  error_reporter::write_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/type,
      /*token_begin=*/token_begin,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

vscode_error_formatter vscode_error_reporter::format() {
  return vscode_error_formatter(this);
}

char8 *vscode_error_reporter::allocate_c_string(string8_view string) {
  boost::container::pmr::polymorphic_allocator<char8> allocator(
      &this->string_memory_);
  char8 *result = allocator.allocate(string.size() + 1);
  std::uninitialized_copy(string.begin(), string.end(), result);
  result[string.size()] = u8'\0';
  return result;
}

vscode_error_formatter::vscode_error_formatter(vscode_error_reporter *reporter)
    : reporter_(reporter) {}

void vscode_error_formatter::write_before_message(severity sev,
                                                  const source_code_span &) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }
  QLJS_ASSERT(this->current_message_.empty());
}

void vscode_error_formatter::write_message_part(severity sev,
                                                string8_view message) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }
  this->current_message_.append(message);
}

void vscode_error_formatter::write_after_message(
    severity sev, const source_code_span &origin) {
  qljs_vscode_severity diag_severity = qljs_vscode_severity_error;
  switch (sev) {
  case severity::note:
    // Don't write notes. Only write the main message.
    return;
  case severity::error:
    diag_severity = qljs_vscode_severity_error;
    break;
  case severity::warning:
    diag_severity = qljs_vscode_severity_warning;
    break;
  }
  qljs_vscode_diagnostic &diag = this->reporter_->diagnostics_.emplace_back();
  lsp_range r = this->reporter_->locator_->range(origin);
  diag.start_line = r.start.line;
  diag.start_character = r.start.character;
  diag.end_line = r.end.line;
  diag.end_character = r.end.character;
  diag.message = reinterpret_cast<const char *>(
      this->reporter_->allocate_c_string(this->current_message_));
  diag.severity = diag_severity;
}
}
