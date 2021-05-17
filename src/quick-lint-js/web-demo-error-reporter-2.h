// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_WEB_DEMO_ERROR_REPORTER_2_H
#define QUICK_LINT_JS_WEB_DEMO_ERROR_REPORTER_2_H

#include <cstdint>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-formatter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/web-demo-location.h>
#include <vector>

struct qljs_vscode_diagnostic;

namespace quick_lint_js {
class web_demo_error_formatter_2;

// TODO(strager): Rename to web_demo_error_reporter when the old class is
// removed.
class web_demo_error_reporter_2 final : public error_reporter {
 public:
  explicit web_demo_error_reporter_2();

  void set_input(padded_string_view input, const web_demo_locator *);
  void reset();

  const qljs_web_demo_diagnostic *get_diagnostics();

#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  void report(name) override;
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

 private:
  web_demo_error_formatter_2 format(const char *code);

  char8 *allocate_c_string(string8_view);

  std::vector<qljs_web_demo_diagnostic> diagnostics_;
  const web_demo_locator *locator_;
  const char8 *input_;
  monotonic_allocator string_allocator_;

  friend web_demo_error_formatter_2;
};

class web_demo_error_formatter_2
    : public error_formatter<web_demo_error_formatter_2> {
 public:
  explicit web_demo_error_formatter_2(web_demo_error_reporter_2 *reporter,
                                      const char *code);

  void write_before_message(severity, const source_code_span &origin);
  void write_message_part(severity, string8_view);
  void write_after_message(severity, const source_code_span &origin);

 private:
  web_demo_error_reporter_2 *reporter_;
  const char *code_;
  string8 current_message_;
};
}

#endif

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
