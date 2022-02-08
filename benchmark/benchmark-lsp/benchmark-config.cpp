// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/json/parse.hpp>
#include <boost/json/serialize.hpp>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/benchmark-config.h>
#include <quick-lint-js/boost-json.h>
#include <quick-lint-js/file.h>
#include <system_error>

namespace quick_lint_js {
benchmark_config benchmark_config::load_from_file(const char* config_path) {
  auto config_file = read_file(config_path);
  if (!config_file.ok()) {
    std::fprintf(stderr, "error: %s\n", config_file.error_to_string().c_str());
    std::exit(1);
  }

  std::error_code error;
  ::boost::json::value root =
      ::boost::json::parse(to_string_view(config_file->string_view()), error);
  if (error != std::error_code()) {
    std::fprintf(stderr, "error: %s: parsing JSON failed\n", config_path);
    std::exit(1);
  }

  benchmark_config config;
  for (::boost::json::value& server_value :
       root.as_object()["servers"].as_array()) {
    ::boost::json::object& server_object = server_value.as_object();
    benchmark_config_server& server = config.servers.emplace_back();

    server.name = server_object["name"].as_string();
    for (::boost::json::value& command_value :
         server_object["command"].as_array()) {
      server.command.push_back(std::string(command_value.as_string()));
    }
    if (::boost::json::string* v = if_string(server_object, "cwd")) {
      server.cwd = *v;
    }
    if (bool* v = if_bool(server_object, "allowIncrementalChanges")) {
      server.allow_incremental_changes = *v;
    }
    if (std::int64_t* v =
            if_int64(server_object, "diagnosticsMessagesToIgnore")) {
      server.diagnostics_messages_to_ignore = *v;
    }
    if (std::int64_t* v =
            if_int64(server_object,
                     "diagnosticsMessagesToIgnoreAfterIncrementalChange")) {
      server.diagnostics_messages_to_ignore_after_incremental_change = *v;
    }
    if (::boost::json::value* v =
            server_object.if_contains("initializationOptions")) {
      server.initialization_options_json = ::boost::json::serialize(*v);
    }
    if (bool* v = if_bool(server_object, "needFilesOnDisk")) {
      server.need_files_on_disk = *v;
    }
    if (bool* v = if_bool(server_object, "supportsJSX")) {
      server.supports_jsx = *v;
    }
    if (bool* v = if_bool(server_object, "waitForEmptyDiagnosticsOnOpen")) {
      server.wait_for_empty_diagnostics_on_open = *v;
    }
    if (::boost::json::value* v =
            server_object.if_contains("workspaceConfiguration")) {
      server.workspace_configuration_json = ::boost::json::serialize(*v);
    }
  }
  return config;
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
