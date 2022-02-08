// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/benchmark-config.h>
#include <vector>

namespace quick_lint_js {
benchmark_config benchmark_config::load() {
  std::vector<benchmark_config_server> servers = {
      benchmark_config_server{
          .name = "vscode-eslint-airbnb",
          .command = {"node",
                      "../node_modules/vscode-eslint/server/out/"
                      "eslintServer.js",
                      "--stdio"},
          .cwd = "eslint/airbnb/",
          .need_files_on_disk = true,
          .workspace_configuration_json = R"({
            "nodePath": null,
            "rulesCustomizations": [],
            "run": "onType",
            "validate": "on"
          })",
      },

      benchmark_config_server{
          .name = "vscode-eslint-react",
          .command = {"node",
                      "../node_modules/vscode-eslint/server/out/"
                      "eslintServer.js",
                      "--stdio"},
          .cwd = "eslint/react/",
          .need_files_on_disk = true,
          .supports_jsx = true,
          .workspace_configuration_json = R"({
            "nodePath": null,
            "rulesCustomizations": [],
            "run": "onType",
            "validate": "on"
          })",
      },

      benchmark_config_server{
          .name = "vscode-eslint-typescript",
          .command = {"node",
                      "../node_modules/vscode-eslint/server/out/"
                      "eslintServer.js",
                      "--stdio"},
          .cwd = "eslint/typescript/",
          .need_files_on_disk = true,
          .supports_jsx = true,
          .workspace_configuration_json = R"({
            "nodePath": null,
            "rulesCustomizations": [],
            "run": "onType",
            "validate": "on"
          })",
      },

      benchmark_config_server{
          .name = "vscode-eslint-vanilla",
          .command = {"node",
                      "../node_modules/vscode-eslint/server/out/"
                      "eslintServer.js",
                      "--stdio"},
          .cwd = "eslint/vanilla/",
          .need_files_on_disk = true,
          .workspace_configuration_json = R"({
            "nodePath": null,
            "rulesCustomizations": [],
            "run": "onType",
            "validate": "on"
          })",
      },

      benchmark_config_server{
          .name = "vscode-eslint-vue",
          .command = {"node",
                      "../node_modules/vscode-eslint/server/out/"
                      "eslintServer.js",
                      "--stdio"},
          .cwd = "eslint/vue/",
          .need_files_on_disk = true,
          .workspace_configuration_json = R"({
            "nodePath": null,
            "rulesCustomizations": [],
            "run": "onType",
            "validate": "on"
          })",
      },

      benchmark_config_server{
          .name = "Deno",
          .command = {"deno", "lsp"},
          .diagnostics_messages_to_ignore = 2,
          .initialization_options_json = R"({
            "enable": true,
            "lint": true,
            "unstable": true
          })",
          .supports_jsx = true,
          .workspace_configuration_json = R"({
            "enable": true,
            "lint": true,
            "unstable": true
          })",
      },

      benchmark_config_server{
          .name = "Deno-nolint",
          .command = {"deno", "lsp"},
          .diagnostics_messages_to_ignore = 1,
          .initialization_options_json = R"({
            "enable": true,
            "lint": false,
            "unstable": true
          })",
          .supports_jsx = true,
          .workspace_configuration_json = R"({
            "enable": true,
            "lint": false,
            "unstable": true
          })",
      },

      benchmark_config_server{
          .name = "Flow",
          .command = {"./run.sh"},
          .cwd = "flow/",
          .diagnostics_messages_to_ignore_after_incremental_change = 1,
          .supports_jsx = true,
          .wait_for_empty_diagnostics_on_open = false,
      },

      benchmark_config_server{
          .name = "quick-lint-js",
          .command = {"quick-lint-js", "--lsp-server"},
          .supports_jsx = true,
      },

      benchmark_config_server{
          .name = "RSLint",
          .command = {"rslint-lsp"},
          .allow_incremental_changes = false,
      },

      benchmark_config_server{
          .name = "TypeScript",
          .command = {"node", "./node_modules/.bin/typescript-language-server",
                      "--stdio"},
          .cwd = "typescript/",
          .need_files_on_disk = true,
          .supports_jsx = false,
      },

      benchmark_config_server{
          .name = "TypeScript-JSX",
          .command = {"node", "./node_modules/.bin/typescript-language-server",
                      "--stdio"},
          .cwd = "typescript-jsx/",
          .need_files_on_disk = true,
          .supports_jsx = true,
      },
  };
  return benchmark_config{
      .servers = std::move(servers),
  };
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
