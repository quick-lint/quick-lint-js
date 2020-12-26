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

#ifndef QUICK_LINT_JS_LSP_ERROR_REPORTER_H
#define QUICK_LINT_JS_LSP_ERROR_REPORTER_H

#include <optional>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/error-formatter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>
#include <string>

namespace quick_lint_js {
class lsp_error_formatter;

class lsp_error_reporter final : public error_reporter {
 public:
  explicit lsp_error_reporter(byte_buffer &output, padded_string_view input);

  void finish();

#define QLJS_ERROR_TYPE(name, struct_body, format) void report(name) override;
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  void report_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character) override;
  void report_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin) override;

 private:
  lsp_error_formatter begin_error();
  lsp_error_formatter format();

  byte_buffer &output_;
  lsp_locator locator_;
  bool need_comma_ = false;
};

class lsp_error_formatter : public error_formatter<lsp_error_formatter> {
 public:
  explicit lsp_error_formatter(byte_buffer &output, lsp_locator &);
  void write_before_message(severity, const source_code_span &origin);
  void write_message_part(severity, string8_view);
  void write_after_message(severity, const source_code_span &origin);

 private:
  byte_buffer &output_;
  lsp_locator &locator_;
};
}

#endif
