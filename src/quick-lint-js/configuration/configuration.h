// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <array>
#include <cstddef>
#include <optional>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/global-variables.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/simdjson-fwd.h>
#include <vector>

namespace quick_lint_js {
class Diag_List;

class Configuration {
 public:
  explicit Configuration();

  const Global_Declared_Variable_Set& globals();

  void reset_global_groups();
  bool add_global_group(String8_View group_name);

  void add_global_variable(Global_Declared_Variable);

  // For testing and internal use only.
  //
  // name must live as long as this Configuration object.
  void remove_global_variable(String8_View name);

  void load_from_json(Padded_String_View, Diag_List* out_diags);

  void reset();

 private:
  bool load_global_groups_from_json(simdjson::ondemand::value&,
                                    Diag_List* out_diags);
  bool load_globals_from_json(simdjson::ondemand::object&,
                              Diag_List* out_diags);

  bool should_remove_global_variable(String8_View name);

  [[gnu::noinline]] void build_globals_from_groups();

  void report_json_error(Padded_String_View json, Diag_List* out_diags);

  Monotonic_Allocator allocator_{"Configuration::allocator_"};
  Global_Declared_Variable_Set globals_;
  Vector<String8_View> globals_to_remove_{"Configuration::globals_to_remove_",
                                          &this->allocator_};
  bool did_add_globals_from_groups_ = false;
  std::array<bool, global_group_count> enabled_global_groups_;
  bool literally_anything_global_group_enabled_ = false;

  String8_View save_string(std::string_view s);
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
