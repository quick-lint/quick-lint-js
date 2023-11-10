// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <quick-lint-js/benchmark-config.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/process.h>
#include <simdjson.h>
#include <spawn.h>
#include <string>
#include <unistd.h>
#include <vector>

#if QLJS_HAVE_CRT_EXTERNS_H
#include <crt_externs.h>
#endif

QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")

using namespace std::literals::string_literals;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
std::vector<std::string_view> split(std::string_view s, char separator) {
  std::vector<std::string_view> result;
  while (!s.empty()) {
    std::size_t separator_index = s.find(separator);
    if (separator_index == s.npos) {
      result.push_back(s);
      break;
    }
    result.push_back(s.substr(0, separator_index));
    s = s.substr(separator_index + 1);
  }
  return result;
}

// TODO(strager): Reuse run_program from <quick-lint-js/port/child-process.h>.
std::string run_program(std::vector<std::string> command,
                        std::optional<std::string> cwd) {
  Pipe_FDs program_output = make_pipe();
  ::posix_spawn_file_actions_t file_actions;
  posix_spawn_file_actions_init(&file_actions);
  posix_spawn_file_actions_adddup2(&file_actions, program_output.writer.get(),
                                   STDOUT_FILENO);
  posix_spawn_file_actions_adddup2(&file_actions, program_output.writer.get(),
                                   STDERR_FILENO);

  std::vector<char*> argv;
  for (const std::string& arg : command) {
    argv.push_back(const_cast<char*>(arg.c_str()));
  }
  argv.push_back(nullptr);
  const char* exe_file = command.at(0).c_str();

  std::filesystem::path old_cwd = std::filesystem::current_path();
  std::filesystem::path new_cwd = old_cwd;
  if (cwd.has_value()) {
    new_cwd.append(*cwd);
  }
  std::filesystem::current_path(new_cwd);
  ::pid_t pid;
#if QLJS_HAVE_NS_GET_ENVIRON
  char**& environ = *::_NSGetEnviron();
#endif
  int rc = ::posix_spawnp(/*pid=*/&pid, /*file=*/exe_file,
                          /*file_actions=*/&file_actions,
                          /*attrp=*/nullptr,
                          /*argv=*/argv.data(),
                          /*envp=*/environ);
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to spawn %s: %s\n", exe_file,
                 std::strerror(errno));
    std::exit(1);
  }
  std::filesystem::current_path(old_cwd);
  program_output.writer.close();

  posix_spawn_file_actions_destroy(&file_actions);

  auto output = read_file(program_output.reader.ref());
  if (!output.ok()) {
    std::fprintf(stderr, "error: %s\n", output.error_to_string().c_str());
    std::exit(1);
  }

  wait_for_process_exit(pid);

  return to_string(output->string_view());
}

std::map<std::string, std::string> get_yarn_packages_versions(
    std::string project_directory) {
  std::string yarn_list_output = run_program(
      {"yarn", "list", "--depth=0", "--json"}, /*cwd=*/project_directory);
  std::vector<std::string_view> lines = split(yarn_list_output, '\n');
  QLJS_ALWAYS_ASSERT(lines.size() >= 1);
  std::string_view json = lines[lines.size() - 1];
  QLJS_ALWAYS_ASSERT(!json.empty());

  ::simdjson::dom::parser parser;
  ::simdjson::dom::element root;
  if (parser.parse(json.data(), json.size()).get(root) != ::simdjson::SUCCESS) {
    std::fprintf(stderr, "error: parsing 'yarn list' JSON failed\n");
    std::exit(1);
  }

  std::map<std::string, std::string> package_versions;
  ::simdjson::dom::array packages;
  if (root["data"]["trees"].get(packages) != ::simdjson::SUCCESS) {
    std::fprintf(stderr, "error: 'yarn list' JSON missing .data.trees array\n");
    std::exit(1);
  }
  for (::simdjson::dom::element package : packages) {
    std::string_view full_package_name;
    if (package["name"].get(full_package_name) != ::simdjson::SUCCESS) {
      std::fprintf(
          stderr,
          "error: 'yarn list' JSON missing .name in .data.trees array\n");
      std::exit(1);
    }
    std::size_t version_separator_index = full_package_name.rfind('@');
    QLJS_ALWAYS_ASSERT(version_separator_index != full_package_name.npos);
    std::string_view package_name =
        std::string_view(full_package_name).substr(0, version_separator_index);
    std::string_view package_version =
        std::string_view(full_package_name).substr(version_separator_index + 1);
    auto [_it, inserted] = package_versions.try_emplace(
        std::string(package_name), package_version);
    // If the following assertion fails, 'yarn list' gave multiple entries for
    // the same package.
    QLJS_ALWAYS_ASSERT(inserted);
  }
  return package_versions;
}

std::string get_nodejs_version() {
  std::string node_version_output =
      run_program({"node", "--version"}, /*cwd=*/std::nullopt);
  std::vector<std::string_view> lines = split(node_version_output, '\n');
  QLJS_ALWAYS_ASSERT(lines.size() >= 1);
  return std::string(lines[0]);
}
}

Benchmark_Config Benchmark_Config::load() {
  std::vector<Benchmark_Config_Server> servers = {
      Benchmark_Config_Server{
          .name = "vscode-eslint-airbnb",
          .program_name = "ESLint",
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

      Benchmark_Config_Server{
          .name = "vscode-eslint-react",
          .program_name = "ESLint",
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

      Benchmark_Config_Server{
          .name = "vscode-eslint-typescript",
          .program_name = "ESLint",
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

      Benchmark_Config_Server{
          .name = "vscode-eslint-vanilla",
          .program_name = "ESLint",
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

      Benchmark_Config_Server{
          .name = "vscode-eslint-vue",
          .program_name = "ESLint",
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

      Benchmark_Config_Server{
          .name = "Deno",
          .program_name = "Deno",
          .command = {"deno", "lsp"},
          .diagnostics_messages_to_ignore = 2,
          .diagnostics_messages_to_ignore_after_incremental_change = 2,
          .initialization_options_json = R"({
            "enable": true,
            "lint": true,
            "unstable": true
          })",
          .supports_jsx = true,
          .parallelize_open = false,
          .workspace_configuration_json = R"({
            "enable": true,
            "lint": true,
            "unstable": true
          })",
      },

      Benchmark_Config_Server{
          .name = "Deno-nolint",
          .program_name = "Deno",
          .command = {"deno", "lsp"},
          .diagnostics_messages_to_ignore = 1,
          .diagnostics_messages_to_ignore_after_incremental_change = 1,
          .initialization_options_json = R"({
            "enable": true,
            "lint": false,
            "unstable": true
          })",
          .supports_jsx = true,
          .parallelize_open = false,
          .workspace_configuration_json = R"({
            "enable": true,
            "lint": false,
            "unstable": true
          })",
      },

      Benchmark_Config_Server{
          .name = "Flow",
          .program_name = "Flow",
          .command = {"./run.sh"},
          .cwd = "flow/",
          .diagnostics_messages_to_ignore_after_incremental_change = 1,
          .supports_jsx = true,
          .wait_for_empty_diagnostics_on_open = false,
      },

      Benchmark_Config_Server{
          .name = "quick-lint-js",
          .program_name = "quick-lint-js",
          .command = {"quick-lint-js", "--lsp-server"},
          .supports_jsx = true,
      },

      Benchmark_Config_Server{
          .name = "RSLint",
          .program_name = "RSLint",
          .command = {"rslint-lsp"},
          .allow_incremental_changes = false,
      },

      Benchmark_Config_Server{
          .name = "Biome",
          .program_name = "Biome",
          .command = {"./run.sh"},
          .cwd = "biome/",
          .supports_jsx = true,
      },

      Benchmark_Config_Server{
          .name = "TypeScript",
          .program_name = "TypeScript",
          .command = {"node", "./node_modules/.bin/typescript-language-server",
                      "--stdio"},
          .cwd = "typescript/",
          .need_files_on_disk = true,
          .supports_jsx = false,
      },

      Benchmark_Config_Server{
          .name = "TypeScript-JSX",
          .program_name = "TypeScript",
          .command = {"node", "./node_modules/.bin/typescript-language-server",
                      "--stdio"},
          .cwd = "typescript-jsx/",
          .need_files_on_disk = true,
          .supports_jsx = true,
      },
  };

  std::vector<Benchmark_Config_Program> programs = {
      Benchmark_Config_Program{
          .name = "Deno",
          .get_metadata =
              []() {
                std::string deno_version_output =
                    run_program({"deno", "--version"}, /*cwd=*/std::nullopt);
                std::map<std::string, std::string> metadata;
                for (std::string_view line : split(deno_version_output, '\n')) {
                  std::vector<std::string_view> parts = split(line, ' ');
                  if (parts.size() < 2) {
                    continue;
                  }
                  std::string_view key = parts[0];
                  std::string_view value = parts[1];
                  if (!key.empty() && !value.empty()) {
                    auto [_it, inserted] =
                        metadata.try_emplace(std::string(key), value);
                    // If the following assertion fails, 'deno --version' gave
                    // multiple lines with the same key.
                    QLJS_ALWAYS_ASSERT(inserted);
                  }
                }
                return metadata;
              },
      },

      Benchmark_Config_Program{
          .name = "ESLint",
          .get_metadata =
              []() {
                auto metadata = get_yarn_packages_versions("eslint");
                auto [_it, inserted] =
                    metadata.try_emplace("node"s, get_nodejs_version());
                QLJS_ALWAYS_ASSERT(inserted);

                const char* package_json_path = "eslint/package.json";
                auto package_json_content = read_file(package_json_path);
                if (!package_json_content.ok()) {
                  std::fprintf(stderr, "error: %s\n",
                               package_json_content.error_to_string().c_str());
                  std::exit(1);
                }
                ::simdjson::dom::parser parser;
                ::simdjson::dom::element root;
                if (parser
                        .parse(reinterpret_cast<const char*>(
                                   package_json_content->data()),
                               package_json_content->size())
                        .get(root) != ::simdjson::SUCCESS) {
                  std::fprintf(stderr, "error: %s: parsing JSON failed\n",
                               package_json_path);
                  std::exit(1);
                }
                std::string_view vscode_eslint_dependency;
                if (root["dependencies"]["vscode-eslint"].get(
                        vscode_eslint_dependency) != ::simdjson::SUCCESS) {
                  std::fprintf(stderr,
                               "error: %s: failed to extract "
                               ".dependencies['vscode-eslint']\n",
                               package_json_path);
                  std::exit(1);
                }
                metadata["vscode-eslint"] = vscode_eslint_dependency;

                return metadata;
              },
      },

      Benchmark_Config_Program{
          .name = "Flow",
          .get_metadata = []() { return get_yarn_packages_versions("flow"); },
      },

      Benchmark_Config_Program{
          .name = "quick-lint-js",
          .get_metadata =
              []() {
                std::string qljs_version_output = run_program(
                    {"quick-lint-js", "--version"}, /*cwd=*/std::nullopt);
                std::vector<std::string_view> lines =
                    split(qljs_version_output, '\n');
                QLJS_ALWAYS_ASSERT(lines.size() >= 1);
                std::vector<std::string_view> parts = split(lines[0], ' ');
                QLJS_ALWAYS_ASSERT(parts.size() == 3);
                return std::map<std::string, std::string>{
                    {"version"s, std::string(parts[2])},
                };
              },
      },

      Benchmark_Config_Program{
          .name = "Biome",
          .get_metadata = []() { return get_yarn_packages_versions("biome"); },
      },

      Benchmark_Config_Program{
          .name = "TypeScript",
          .get_metadata =
              []() {
                auto metadata = get_yarn_packages_versions("typescript");
                auto [_it, inserted] =
                    metadata.try_emplace("node"s, get_nodejs_version());
                QLJS_ALWAYS_ASSERT(inserted);
                return metadata;
              },
      },
  };

  return Benchmark_Config{
      .servers = std::move(servers),
      .programs = std::move(programs),
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
