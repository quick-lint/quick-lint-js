// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ARG_PARSER_H
#define QUICK_LINT_JS_ARG_PARSER_H

#include <optional>
#include <string_view>

namespace quick_lint_js {
class arg_parser {
 public:
  explicit arg_parser(int argc, char** argv) noexcept;

  arg_parser(const arg_parser&) = delete;
  arg_parser& operator=(const arg_parser&) = delete;

  const char* match_option_with_value(std::string_view option_name) noexcept;

  bool match_flag_shorthand(char option_shorthand) noexcept;

  bool match_flag_option(std::string_view full_option_name,
                         std::string_view partial_option_name) noexcept;

  const char* match_argument() noexcept;

  const char* match_anything() noexcept;

  bool done() const noexcept;

 private:
  void parse_current_arg() noexcept;

  void advance(int count) noexcept;

  const char* current_arg() noexcept;

  struct option {
    std::string_view arg_key;
    const char* arg_value;
    bool arg_has_equal;
  };

  std::optional<option> option_;
  bool is_ignoring_options_ = false;
  int current_arg_index_ = 1;

  int argc_;
  char** argv_;
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
