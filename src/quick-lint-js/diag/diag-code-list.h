// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <array>
#include <bitset>
#include <memory>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/port/span.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
struct Parsed_Diag_Code_List {
  bool error_missing_predicate() const;

  Span<const std::string_view> included_codes = Span<const std::string_view>();
  Span<const std::string_view> excluded_codes = Span<const std::string_view>();
  Span<const std::string_view> included_categories =
      Span<const std::string_view>();
  Span<const std::string_view> excluded_categories =
      Span<const std::string_view>();
  Span<const std::string_view> unexpected = Span<const std::string_view>();
  bool override_defaults = false;
};

// Returns Span-s allocated by allocator.
//
// Return std::string_view-s within raw_diag_code_list.
Parsed_Diag_Code_List parse_diag_code_list(const char* raw_diag_code_list,
                                           Monotonic_Allocator* allocator);

class Compiled_Diag_Code_List {
 public:
  explicit Compiled_Diag_Code_List();

  // Retains references to the std::string_view-s.
  void add(const Parsed_Diag_Code_List&);

  Span<std::string_view> parse_errors(std::string_view cli_option_name,
                                      Monotonic_Allocator* allocator) const;
  Span<std::string_view> parse_warnings(Monotonic_Allocator* allocator) const;

  bool is_present(Diag_Type) const;

  bool is_user_provided() const;

 private:
  struct Codes {
    std::bitset<Diag_Type_Count> included_codes;
    std::bitset<Diag_Type_Count> excluded_codes;
    Span<std::string_view> included_categories;
    Span<std::string_view> excluded_categories;
    bool override_defaults;
  };

  // TODO(strager): Make caller provide the allocator.
  // HACK(strager): This is a unique_ptr to make Compiled_Diag_Code_List
  // movable.
  std::unique_ptr<Monotonic_Allocator> allocator_;

  Bump_Vector<Codes> parsed_diag_code_lists_{"parsed_diag_code_lists_",
                                             this->allocator_.get()};

  // Collected errors and warnings:
  Bump_Vector<std::string_view> unknown_codes_{"unknown_codes_",
                                               this->allocator_.get()};
  bool has_missing_predicate_error_ = false;
};
}

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
