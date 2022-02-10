// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-error-reporter.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <string>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
lsp_error_reporter::lsp_error_reporter(byte_buffer &output,
                                       padded_string_view input)
    : output_(output), locator_(input) {
  this->output_.append_copy(u8"["sv);
}

void lsp_error_reporter::finish() { this->output_.append_copy(u8"]"sv); }

void lsp_error_reporter::report_impl(error_type type, void *error) {
  if (this->need_comma_) {
    this->output_.append_copy(u8",\n"sv);
  }
  this->need_comma_ = true;
  lsp_error_formatter formatter(/*output=*/this->output_,
                                /*locator=*/this->locator_);
  formatter.format(get_diagnostic_info(type), error);
}

lsp_error_formatter::lsp_error_formatter(byte_buffer &output,
                                         lsp_locator &locator)
    : output_(output), locator_(locator) {}

void lsp_error_formatter::write_before_message(std::string_view code,
                                               diagnostic_severity sev,
                                               const source_code_span &origin) {
  char8 severity_type{};
  switch (sev) {
  case diagnostic_severity::error:
    severity_type = u8'1';
    break;
  case diagnostic_severity::note:
    // Don't write notes. Only write the main message.
    return;
  case diagnostic_severity::warning:
    severity_type = u8'2';
    break;
  }

  lsp_range r = this->locator_.range(origin);
  this->output_.append_copy(
      u8"{\"range\":{\"start\":"sv
      u8"{\"line\":"sv);
  this->output_.append_decimal_integer(r.start.line);
  this->output_.append_copy(u8",\"character\":"sv);
  this->output_.append_decimal_integer(r.start.character);
  this->output_.append_copy(
      u8"},\"end\":"sv
      u8"{\"line\":"sv);
  this->output_.append_decimal_integer(r.end.line);
  this->output_.append_copy(u8",\"character\":"sv);
  this->output_.append_decimal_integer(r.end.character);
  this->output_.append_copy(u8"}},\"severity\":"sv);
  this->output_.append_copy(severity_type);
  this->output_.append_copy(u8",\"code\":\""sv);
  this->output_.append_copy(to_string8_view(code));
  this->output_.append_copy(
      u8"\",\"codeDescription\":"sv
      u8"{\"href\":\"https://quick-lint-js.com/errors/"sv);
  this->output_.append_copy(to_string8_view(code));
  this->output_.append_copy(
      u8"/\"},\"source\":\"quick-lint-js\""sv
      u8",\"message\":\""sv);
}

void lsp_error_formatter::write_message_part(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    string8_view message) {
  if (sev == diagnostic_severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  write_json_escaped_string(this->output_, message);
}

void lsp_error_formatter::write_after_message(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    const source_code_span &) {
  if (sev == diagnostic_severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  this->output_.append_copy(u8"\"}"sv);
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
