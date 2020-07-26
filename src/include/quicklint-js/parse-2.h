// quicklint-js finds bugs in JavaScript programs.
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

#ifndef QUICKLINT_JS_PARSE_2_H
#define QUICKLINT_JS_PARSE_2_H

#include <cstdlib>
#include <quicklint-js/error.h>
#include <quicklint-js/expression.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/location.h>
#include <vector>

namespace quicklint_js {
class parser2 {
 public:
  explicit parser2(const char *input, error_reporter *error_reporter)
      : lexer_(input, error_reporter),
        locator_(input),
        error_reporter_(error_reporter) {}

  const quicklint_js::locator &locator() const noexcept {
    return this->locator_;
  }

  expression_ptr parse_expression() {
    return this->parse_expression(
        precedence{.binary_operators = true, .commas = true});
  }

 private:
  struct precedence {
    bool binary_operators;
    bool commas;
  };

  expression_ptr parse_expression(precedence);

  expression_ptr parse_expression_remainder(expression_ptr, precedence);

  expression_ptr parse_template();

  const token &peek() const noexcept { return this->lexer_.peek(); }

  [[noreturn]] void crash_on_unimplemented_token(
      const char *qljs_file_name, int qljs_line,
      const char *qljs_function_name);

  template <expression_kind Kind, class... Args>
  expression_ptr make_expression(Args &&... args) {
    return this->expressions_.make_expression<Kind>(
        std::forward<Args>(args)...);
  }

  lexer lexer_;
  quicklint_js::locator locator_;
  error_reporter *error_reporter_;
  expression_arena expressions_;
};
}  // namespace quicklint_js

#endif
