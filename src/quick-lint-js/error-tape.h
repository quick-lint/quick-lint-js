// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ERROR_TAPE_H
#define QUICK_LINT_JS_ERROR_TAPE_H

#include <quick-lint-js/error.h>
#include <quick-lint-js/text-error-reporter.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>

namespace quick_lint_js {
template <typename T>
class error_tape final : public error_reporter {
 public:
  error_tape(T reporter) : reporter_(reporter), error_(false) {}

  T *get_reporter() { return &(this->reporter_); }

  bool get_error(void) { return this->error_; }

  void set_error(void) { this->error_ = true; }

#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  void report(name e) override final {                   \
    set_error();                                         \
    reporter_.report(e);                                 \
  }
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  void report_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character) override final {
    reporter_.report_fatal_error_unimplemented_character(
        qljs_file_name, qljs_line, qljs_function_name, character);
  }

  void report_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type type, const char8 *token_begin) override final {
    reporter_.report_fatal_error_unimplemented_token(
        qljs_file_name, qljs_line, qljs_function_name, type, token_begin);
  }

 private:
  T reporter_;

  bool error_;
};
}

#endif

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
