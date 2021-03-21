// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <iostream>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>

namespace quick_lint_js {
#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  void error_collector::report(name e) {                      \
    this->errors.emplace_back(std::move(e));                  \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  error_collector::error::error(name &&data)                  \
      : kind_(kind::kind_##name), variant_##name##_(std::move(data)) {}
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

void error_collector::report_fatal_error_unimplemented_character(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    const char8 *character) {
  error_reporter::write_fatal_error_unimplemented_character(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*character=*/character,
      /*locator=*/nullptr);
}

void error_collector::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
  error_reporter::write_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/type,
      /*token_begin=*/token_begin,
      /*locator=*/nullptr);
}

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call)              \
  template <>                                                              \
  bool holds_alternative<name>(const error_collector::error &e) noexcept { \
    return e.kind_ == error_collector::error::kind::kind_##name;           \
  }                                                                        \
                                                                           \
  template <>                                                              \
  const name &get<name>(const error_collector::error &e) noexcept {        \
    QLJS_ASSERT(holds_alternative<name>(e));                               \
    return e.variant_##name##_;                                            \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

void PrintTo(const error_collector::error &e, std::ostream *out) {
  switch (e.kind_) {
#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  case error_collector::error::kind::kind_##name:             \
    *out << #name;                                            \
    return;
    QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
  }
  QLJS_UNREACHABLE();
}

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  void PrintTo(const name &, std::ostream *out) { *out << #name; }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
}

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
