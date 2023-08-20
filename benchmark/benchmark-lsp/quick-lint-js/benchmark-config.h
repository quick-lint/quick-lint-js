// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace quick_lint_js {
struct Benchmark_Config_Server {
  std::string name;
  std::string program_name;
  std::vector<std::string> command;
  std::optional<std::string> cwd;
  bool allow_incremental_changes = true;
  std::int64_t diagnostics_messages_to_ignore = 0;
  std::int64_t diagnostics_messages_to_ignore_after_incremental_change = 0;
  std::string initialization_options_json = "{}";
  bool need_files_on_disk = false;
  bool supports_jsx = false;
  bool parallelize_open = true;
  bool wait_for_empty_diagnostics_on_open = true;
  std::string workspace_configuration_json = "{}";
};

struct Benchmark_Config_Program {
  std::string name;
  std::map<std::string, std::string> (*get_metadata)();

  bool dumped_metadata = false;
};

struct Benchmark_Config {
  std::vector<Benchmark_Config_Server> servers;
  std::vector<Benchmark_Config_Program> programs;

  static Benchmark_Config load();
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
