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
variable_kind global_declared_variable::kind() const noexcept {
  if (this->is_writable) {
    return variable_kind::_let;
  } else {
    return variable_kind::_const;
  }
}

void global_declared_variable_set::add_predefined_global_variable(
    const char8 *name, bool is_writable) {
  this->add_global_variable(global_declared_variable{
      .name = name, .is_writable = is_writable, .is_shadowable = true});
}

void global_declared_variable_set::add_global_variable(
    global_declared_variable global_variable) {
  this->variables_[global_variable.name] = variable_options{
      .is_writable = global_variable.is_writable,
      .is_shadowable = global_variable.is_shadowable,
  };
}

void global_declared_variable_set::add_literally_everything() {
  this->all_variables_declared_ = true;
}

void global_declared_variable_set::reserve_more_global_variables(
    std::size_t extra_count, [[maybe_unused]] bool is_shadowable,
    [[maybe_unused]] bool is_writable) {
  this->variables_.reserve(this->variables_.size() + extra_count);
}

std::optional<global_declared_variable> global_declared_variable_set::find(
    identifier name) const noexcept {
  return this->find(name.normalized_name());
}

std::optional<global_declared_variable> global_declared_variable_set::find(
    string8_view name) const noexcept {
  auto it = this->variables_.find(name);
  if (it != this->variables_.end()) {
    return global_declared_variable{
        .name = name,
        .is_writable = it->second.is_writable,
        .is_shadowable = it->second.is_shadowable,
    };
  }
  if (this->all_variables_declared_) {
    return global_declared_variable{
        .name = name,
        .is_writable = true,
        .is_shadowable = true,
    };
  }
  return std::nullopt;
}

std::optional<global_declared_variable>
global_declared_variable_set::find_runtime(identifier name) const noexcept {
  // global_declared_variable_set doesn't support type-only variables. All
  // variables are accessible at run-time.
  return this->find(name);
}

std::optional<global_declared_variable> global_declared_variable_set::find_type(
    identifier name) const noexcept {
  // TODO(#690): Do not treat all globals as type-visible.
  return this->find(name);
}

std::vector<string8_view> global_declared_variable_set::get_all_variable_names()
    const {
  std::vector<string8_view> result;
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
