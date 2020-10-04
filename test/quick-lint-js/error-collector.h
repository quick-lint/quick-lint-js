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

#ifndef QUICK_LINT_JS_ERROR_COLLECTOR_H
#define QUICK_LINT_JS_ERROR_COLLECTOR_H

#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
#include <utility>
#include <variant>
#include <vector>

namespace quick_lint_js {
struct error_collector : public error_reporter {
#define QLJS_ERROR_TYPE(name, struct_body, format_call)                \
  void report(name e) override {                                       \
    this->errors.emplace_back(std::in_place_type<name>, std::move(e)); \
  }
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  void report_fatal_error_unimplemented_character(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      const char8 *character) override;
  void report_fatal_error_unimplemented_token(
      const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
      token_type, const char8 *token_begin) override;

  // HACK(strager): Derive from std::variant<> instead of aliasing it to improve
  // googletest's error messages when it prints out the class' name.
  // HACK(strager): std::monostate allows us to use QLJS_X_ERROR_TYPES. Without
  // std::monostate, we would have a dangling leading or trailing comma.
  using error_variant = std::variant<std::monostate
#define QLJS_ERROR_TYPE(name, struct_body, format_call) , name
                                         QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
                                     >;
  struct error : public error_variant {
    using error_variant::error_variant;
  };

  std::vector<error> errors;
};

void PrintTo(const error_collector::error &, std::ostream *);

#define QLJS_ERROR_TYPE(name, struct_body, format_call) \
  void PrintTo(const name &, std::ostream *);
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
}

#endif
