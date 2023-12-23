// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <optional>
#include <quick-lint-js/container/hash-set.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/identifier.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
Variable_Kind Global_Declared_Variable::kind() const {
  if (this->is_type_only) {
    // TODO(strager): What should we do here? Will this ever be called?
    return Variable_Kind::_let;
  }

  if (this->is_writable) {
    return Variable_Kind::_let;
  } else {
    return Variable_Kind::_const;
  }
}

Global_Declared_Variable_Set::Global_Declared_Variable_Set() { this->clear(); }

void Global_Declared_Variable_Set::add_predefined_global_variable(
    const Char8 *name, bool is_writable) {
  this->add_global_variable(Global_Declared_Variable{
      .name = name,
      .is_writable = is_writable,
      .is_shadowable = true,
      .is_type_only = false,
  });
}

void Global_Declared_Variable_Set::add_global_variable(
    Global_Declared_Variable global_variable) {
  this->variables_[global_variable.name] = Variable_Options{
      .is_writable = global_variable.is_writable,
      .is_shadowable = global_variable.is_shadowable,
      .is_type_only = global_variable.is_type_only,
  };
}

void Global_Declared_Variable_Set::add_literally_everything() {
  this->all_variables_declared_ = true;
}

void Global_Declared_Variable_Set::reserve_more_global_variables(
    std::size_t extra_count, [[maybe_unused]] bool is_shadowable,
    [[maybe_unused]] bool is_writable) {
  this->variables_.reserve(this->variables_.size() + extra_count);
}

std::optional<Global_Declared_Variable>
Global_Declared_Variable_Set::find_runtime_or_type(Identifier name) const {
  return this->find_runtime_or_type(name.normalized_name());
}

std::optional<Global_Declared_Variable>
Global_Declared_Variable_Set::find_runtime_or_type(String8_View name) const {
  auto it = this->variables_.find(name);
  if (it != this->variables_.end()) {
    return Global_Declared_Variable{
        .name = name,
        .is_writable = it->second.is_writable,
        .is_shadowable = it->second.is_shadowable,
        .is_type_only = it->second.is_type_only,
    };
  }
  if (this->all_variables_declared_) {
    return Global_Declared_Variable{
        .name = name,
        .is_writable = true,
        .is_shadowable = true,
        .is_type_only = false,
    };
  }
  return std::nullopt;
}

std::optional<Global_Declared_Variable> Global_Declared_Variable_Set::find(
    Identifier name, Is_Runtime_Or_Type options) const {
  std::optional<Global_Declared_Variable> var =
      this->find_runtime_or_type(name);
  if (!var.has_value()) {
    return std::nullopt;
  }
  // TODO(#690): Do not treat all globals as type-visible.
  bool var_is_type = true;
  bool var_is_runtime = !var->is_type_only;
  if (var_is_type == options.is_type || var_is_runtime == options.is_runtime) {
    return var;
  } else {
    return std::nullopt;
  }
}

void Global_Declared_Variable_Set::clear() {
  this->variables_.clear();
  this->all_variables_declared_ = false;
}

std::vector<String8_View> Global_Declared_Variable_Set::get_all_variable_names()
    const {
  std::vector<String8_View> result;
  result.reserve(this->variables_.size());
  for (auto &[name, _options] : this->variables_) {
    result.push_back(name);
  }
  return result;
}
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
