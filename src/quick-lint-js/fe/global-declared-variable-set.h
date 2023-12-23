// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <optional>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
struct Global_Declared_Variable {
  String8_View name;
  bool is_writable;
  // If false, the variable was already lexically declared in the module thus
  // cannot be declared by the user with 'let'.
  bool is_shadowable;
  // If true, the variable is only visible in type expressions, not in normal
  // expressions.
  bool is_type_only;

  Variable_Kind kind() const;
  Variable_Declaration_Flags flags() const {
    return Variable_Declaration_Flags::none;
  }
};

class Global_Declared_Variable_Set {
 public:
  using Found_Variable_Type = std::optional<Global_Declared_Variable>;

  explicit Global_Declared_Variable_Set();

  void add_predefined_global_variable(const Char8 *name, bool is_writable);

  void add_global_variable(Global_Declared_Variable);

  void add_literally_everything();

  void reserve_more_global_variables(std::size_t extra_count,
                                     bool is_shadowable, bool is_writable);

  std::optional<Global_Declared_Variable> find_runtime_or_type(
      Identifier name) const;
  std::optional<Global_Declared_Variable> find_runtime_or_type(
      String8_View name) const;

  // See Variable_Analyzer::Declared_Variable_Set::find_runtime.
  std::optional<Global_Declared_Variable> find_runtime(Identifier name) const;

  // See Variable_Analyzer::Declared_Variable_Set::find_type.
  std::optional<Global_Declared_Variable> find_type(Identifier name) const;

  // Return this Global_Declared_Variable_Set to its default-constructed state.
  void clear();

  // For testing only:
  std::vector<String8_View> get_all_variable_names() const;

 private:
  struct Variable_Options {
    // See Global_Declared_Variable::is_writable.
    bool is_writable;
    // See Global_Declared_Variable::is_shadowable.
    bool is_shadowable;
    // See Global_Declared_Variable::is_type_only.
    bool is_type_only;
  };

  Hash_Map<String8_View, Variable_Options> variables_;
  bool all_variables_declared_;
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
