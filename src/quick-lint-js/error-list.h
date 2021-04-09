// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ERROR_LIST_H
#define QUICK_LINT_JS_ERROR_LIST_H

#include <array>
#include <quick-lint-js/error.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
struct parsed_error_list {
  bool error_missing_predicate() const noexcept;

  std::vector<std::string_view> included_codes;
  std::vector<std::string_view> excluded_codes;
  std::vector<std::string_view> included_categories;
  std::vector<std::string_view> excluded_categories;
  std::vector<std::string_view> unexpected;
  bool override_defaults = false;
};

parsed_error_list parse_error_list(const char* raw_error_list);

class compiled_error_list {
 public:
  void add(const parsed_error_list&);

  std::vector<std::string> parse_errors(std::string_view cli_option_name) const;
  std::vector<std::string> parse_warnings() const;

  template <class Error>
  bool is_present() const noexcept;

  bool is_user_provided() const noexcept;

 private:
  bool is_present(const char* error_code) const noexcept;

  std::vector<parsed_error_list> parsed_error_lists_;
};

#define QLJS_ERROR_TYPE(error_name, error_code, struct_body, format) \
  template <>                                                        \
  bool compiled_error_list::is_present<error_name>() const noexcept;
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
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
