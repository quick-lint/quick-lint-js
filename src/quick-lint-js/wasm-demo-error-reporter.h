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

#ifndef QUICK_LINT_JS_WASM_DEMO_ERROR_REPORTER_H
#define QUICK_LINT_JS_WASM_DEMO_ERROR_REPORTER_H

#include <cstdint>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-formatter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/wasm-demo-location.h>
#include <vector>

namespace quick_lint_js {
class wasm_demo_error_formatter;

class wasm_demo_error_reporter final : public error_reporter {
 public:
  struct error {
    const char8 *message = nullptr;
    std::uint32_t begin_offset;
    std::uint32_t end_offset;
  };

  explicit wasm_demo_error_reporter(padded_string_view input);

  const error *get_errors() noexcept;

#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  void report(name) override;
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  void report_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character) override;
  void report_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin) override;

 private:
  wasm_demo_error_formatter format();

  char8 *allocate_c_string(string8_view);

  std::vector<error> errors_;
  wasm_demo_locator locator_;
  const char8 *input_;
  monotonic_allocator string_allocator_;

  friend wasm_demo_error_formatter;
};

class wasm_demo_error_formatter
    : public error_formatter<wasm_demo_error_formatter> {
 public:
  explicit wasm_demo_error_formatter(wasm_demo_error_reporter *reporter);

  void write_before_message(severity, const source_code_span &origin);
  void write_message_part(severity, string8_view);
  void write_after_message(severity, const source_code_span &origin);

 private:
  wasm_demo_error_reporter *reporter_;
  string8 current_message_;
};
}

#endif
