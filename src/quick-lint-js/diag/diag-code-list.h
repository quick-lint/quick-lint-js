// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_DIAG_CODE_LIST_H
#define QUICK_LINT_JS_DIAG_DIAG_CODE_LIST_H

#include <array>
#include <bitset>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
struct Parsed_Diag_Code_List {
  bool error_missing_predicate() const noexcept;

  std::vector<std::string_view> included_codes;
  std::vector<std::string_view> excluded_codes;
  std::vector<std::string_view> included_categories;
  std::vector<std::string_view> excluded_categories;
  std::vector<std::string_view> unexpected;
  bool override_defaults = false;
};

Parsed_Diag_Code_List parse_diag_code_list(const char* raw_diag_code_list);

class Compiled_Diag_Code_List {
 public:
  void add(const Parsed_Diag_Code_List&);

  std::vector<std::string> parse_errors(std::string_view cli_option_name) const;
  std::vector<std::string> parse_warnings() const;

  bool is_present(Diag_Type) const noexcept;

  bool is_user_provided() const noexcept;

 private:
  struct Codes {
    std::bitset<Diag_Type_Count> included_codes;
    std::bitset<Diag_Type_Count> excluded_codes;
    std::vector<std::string_view> included_categories;
    std::vector<std::string_view> excluded_categories;
    bool override_defaults;
  };

  std::vector<Codes> parsed_diag_code_lists_;

  // Collected errors and warnings:
  std::vector<std::string_view> unknown_codes_;
  bool has_missing_predicate_error_ = false;
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
