// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_ERROR_REPORTER_H
#define QUICK_LINT_JS_LSP_ERROR_REPORTER_H

#include <optional>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/error-formatter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>
#include <string>

namespace quick_lint_js {
class lsp_error_formatter;

class lsp_error_reporter final : public error_reporter {
 public:
  explicit lsp_error_reporter(byte_buffer &output, padded_string_view input);

  void finish();

#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  void report(name) override;
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

 private:
  lsp_error_formatter begin_error(const char *code);
  lsp_error_formatter format(const char *code);

  byte_buffer &output_;
  lsp_locator locator_;
  bool need_comma_ = false;
};

class lsp_error_formatter : public error_formatter<lsp_error_formatter> {
 public:
  explicit lsp_error_formatter(byte_buffer &output, lsp_locator &,
                               const char *code);
  void write_before_message(severity, const source_code_span &origin);
  void write_message_part(severity, string8_view);
  void write_after_message(severity, const source_code_span &origin);

 private:
  byte_buffer &output_;
  lsp_locator &locator_;
  const char *code_;
};
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
