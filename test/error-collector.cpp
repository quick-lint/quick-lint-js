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

#include <iostream>
#include <quick-lint-js/error-collector.h>

namespace quick_lint_js {
void error_collector::report_fatal_error_unimplemented_character(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    const char8 *character) {
  error_reporter::write_fatal_error_unimplemented_character(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*character=*/character,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
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
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

void PrintTo(const error_collector::error &e, std::ostream *out) {
  using namespace quick_lint_js;

  struct type_name_getter {
    const char *operator()(const std::monostate &) const noexcept {
      return "(monostate)";
    }

#define QLJS_ERROR_TYPE(name, struct_body, format_call) \
  const char *operator()(const name &) const noexcept { return #name; }
    QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
  };

  *out << std::visit(type_name_getter(),
                     static_cast<const error_collector::error_variant &>(e));
}

#define QLJS_ERROR_TYPE(name, struct_body, format_call) \
  void PrintTo(const name &, std::ostream *out) { *out << #name; }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
}
