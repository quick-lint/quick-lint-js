// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_LSP_DIAG_REPORTER_H
#define QUICK_LINT_JS_LSP_LSP_DIAG_REPORTER_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <optional>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <string>

namespace quick_lint_js {
class LSP_Diag_Formatter;

class LSP_Diag_Reporter final : public Diag_Reporter {
 public:
  explicit LSP_Diag_Reporter(Translator, Byte_Buffer &output,
                             Padded_String_View input);

  void finish();

  void report_impl(Diag_Type type, void *diag) override;

 private:
  Byte_Buffer &output_;
  LSP_Locator locator_;
  Translator translator_;
  bool need_comma_ = false;
};

class LSP_Diag_Formatter : public Diagnostic_Formatter<LSP_Diag_Formatter> {
 public:
  explicit LSP_Diag_Formatter(Byte_Buffer &output, LSP_Locator &, Translator);
  void write_before_message(std::string_view code, Diagnostic_Severity,
                            const Source_Code_Span &origin);
  void write_message_part(std::string_view code, Diagnostic_Severity,
                          String8_View);
  void write_after_message(std::string_view code, Diagnostic_Severity,
                           const Source_Code_Span &origin);

 private:
  Byte_Buffer &output_;
  LSP_Locator &locator_;
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
