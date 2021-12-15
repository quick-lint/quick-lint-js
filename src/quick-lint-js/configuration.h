// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONFIGURATION_H
#define QUICK_LINT_JS_CONFIGURATION_H

#include <array>
#include <cstddef>
#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/global-variables.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/padded-string.h>
#include <simdjson.h>
#include <vector>

namespace quick_lint_js {
class error_reporter;

class configuration {
 public:
  explicit configuration();

  const global_declared_variable_set& globals() noexcept;

  void reset_global_groups();
  void allow_literally_any_global_variable();
  bool add_global_group(string8_view group_name);

  void add_global_variable(global_declared_variable);
  void remove_global_variable(string8_view name);

  void load_from_json(padded_string_view, error_reporter*);

  void reset();

  // TODO(strager): Move this out of the configuration class. It's only used by
  // the CLI.
  bool errors_were_reported = false;

 private:
  bool load_global_groups_from_json(simdjson::ondemand::value&,
                                    error_reporter*);
  bool load_globals_from_json(simdjson::ondemand::object&, error_reporter*);

  bool should_remove_global_variable(string8_view name);

  [[gnu::noinline]] void build_globals_from_groups();

  // Returns false on parse error, and true otherwise.
  template <class Error>
  bool get_bool_or_default(
      ::simdjson::simdjson_result<::simdjson::ondemand::value>&& value,
      bool* out, bool default_value, error_reporter*);

  void report_json_error(padded_string_view json, error_reporter*);

  global_declared_variable_set globals_;
  std::vector<string8> globals_to_remove_;
  bool did_add_globals_from_groups_ = false;
  std::array<bool, global_group_count> enabled_global_groups_;
  bool literally_anything_global_group_enabled_ = false;
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
