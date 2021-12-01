// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_BENCHMARK_CONFIG_H
#define QUICK_LINT_JS_BENCHMARK_CONFIG_H

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace quick_lint_js {
struct benchmark_config_server {
  std::string name;
  std::vector<std::string> command;
  std::optional<std::string> cwd;
  bool allow_incremental_changes = true;
  std::int64_t diagnostics_messages_to_ignore = 0;
  std::string initialization_options_json = "{}";
  bool need_files_on_disk = false;
  bool wait_for_empty_diagnostics_on_open = true;
  std::string workspace_configuration_json = "{}";
};

struct benchmark_config {
  std::vector<benchmark_config_server> servers;

  static benchmark_config load_from_file(const char* config_path);
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
