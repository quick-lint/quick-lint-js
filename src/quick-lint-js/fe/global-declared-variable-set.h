// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_GLOBAL_DECLARED_VARIABLE_SET_H
#define QUICK_LINT_JS_FE_GLOBAL_DECLARED_VARIABLE_SET_H

#include <optional>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
struct global_declared_variable {
  string8_view name;
  bool is_writable;
  // If false, the variable was already lexically declared in the module thus
  // cannot be declared by the user with 'let'.
  bool is_shadowable;

  variable_kind kind() const noexcept;
};

class global_declared_variable_set {
 public:
  using found_variable_type = std::optional<global_declared_variable>;

  void add_predefined_global_variable(const char8 *name, bool is_writable);

  void add_global_variable(global_declared_variable);

  void add_literally_everything();

  void reserve_more_global_variables(std::size_t extra_count,
                                     bool is_shadowable, bool is_writable);

  std::optional<global_declared_variable> find(identifier name) const noexcept;
  std::optional<global_declared_variable> find(string8_view name) const
      noexcept;

  // See variable_analyzer::declared_variable_set::find_runtime.
  std::optional<global_declared_variable> find_runtime(identifier name) const
      noexcept;

  // See variable_analyzer::declared_variable_set::find_type.
  std::optional<global_declared_variable> find_type(identifier name) const
      noexcept;

  // For testing only:
  std::vector<string8_view> get_all_variable_names() const;

 private:
  struct variable_options {
    // See global_declared_variable::is_writable.
    bool is_writable;
    // See global_declared_variable::is_shadowable.
    bool is_shadowable;
  };

  hash_map<string8_view, variable_options> variables_;
  bool all_variables_declared_ = false;
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
