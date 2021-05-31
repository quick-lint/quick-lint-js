// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONFIGURATION_H
#define QUICK_LINT_JS_CONFIGURATION_H

#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/padded-string.h>
#include <simdjson.h>
#include <vector>

namespace quick_lint_js {
class configuration {
 public:
  const global_declared_variable_set& globals() noexcept;

  const std::optional<canonical_path>& config_file_path() const;

  void reset_global_groups();
  bool add_global_group(string8_view group_name);

  global_declared_variable* add_global_variable(string8_view name);
  void remove_global_variable(string8_view name);

  void load_from_json(padded_string_view);
  void set_config_file_path(const canonical_path&);
  void set_config_file_path(canonical_path&&);

  void reset();

 private:
  void load_global_groups_from_json(simdjson::ondemand::value&);
  void load_globals_from_json(simdjson::ondemand::object&);

  bool should_remove_global_variable(string8_view name);

  global_declared_variable_set globals_;
  std::vector<string8> globals_to_remove_;
  std::optional<canonical_path> config_file_path_;
  bool add_global_group_node_js_ = true;
  bool add_global_group_ecmascript_ = true;
  monotonic_allocator string_allocator_;
  string8_view save_string(std::string_view s);
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
