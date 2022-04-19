// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_ERROR_REPORTER_H
#define QUICK_LINT_JS_LSP_ERROR_REPORTER_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <optional>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/diag-reporter.h>
#include <quick-lint-js/diagnostic-formatter.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>
#include <string>

namespace quick_lint_js {
class lsp_diag_formatter;

class lsp_diag_reporter final : public diag_reporter {
 public:
  explicit lsp_diag_reporter(byte_buffer &output, padded_string_view input);

  void finish();

  void report_impl(diag_type type, void *error) override;

 private:
  byte_buffer &output_;
  lsp_locator locator_;
  bool need_comma_ = false;
};

class lsp_diag_formatter : public diagnostic_formatter<lsp_diag_formatter> {
 public:
  explicit lsp_diag_formatter(byte_buffer &output, lsp_locator &);
  void write_before_message(std::string_view code, diagnostic_severity,
                            const source_code_span &origin);
  void write_message_part(std::string_view code, diagnostic_severity,
                          string8_view);
  void write_after_message(std::string_view code, diagnostic_severity,
                           const source_code_span &origin);

 private:
  byte_buffer &output_;
  lsp_locator &locator_;
};
}

#endif

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
