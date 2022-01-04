// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONFIGURATION_LOADER_H
#define QUICK_LINT_JS_CONFIGURATION_LOADER_H

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <optional>
#include <quick-lint-js/buffering-error-reporter.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/result.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace quick_lint_js {
struct file_to_lint;

class configuration_filesystem {
 public:
  virtual ~configuration_filesystem() = default;

  virtual result<canonical_path_result, canonicalize_path_io_error>
  canonicalize_path(const std::string&) = 0;
  virtual result<padded_string, read_file_io_error> read_file(
      const canonical_path&) = 0;
};

struct loaded_config_file {
  explicit loaded_config_file();

  configuration config;

  // The content of the quick-lint-js.config file.
  padded_string file_content;

  // Errors discovered while parsing file_content.
  buffering_error_reporter errors;

  // The path to the quick-lint-js.config file. Never nullptr.
  const canonical_path* config_path;
};

// Returned by configuration_loader::refresh.
struct configuration_change {
  // The path given to configuration_loader::watch_and_load_for_file or
  // configuration_loader::watch_and_load_config_file. Never nullptr.
  const std::string* watched_path;

  // If config_file is nullptr, then no configuration file exists.
  loaded_config_file* config_file;

  // If error is not nullptr, then error points to an I/O error which prevented
  // a configuration file from being determined at all.
  //
  // Invariant: (error == nullptr) || (config_file == nullptr)
  // Invariant: (error == nullptr) || !error->ok()
  result<void, canonicalize_path_io_error, read_file_io_error>*
      error;  // Sometimes nullptr.

  // token is the pointer given to
  // configuration_loader::watch_and_load_for_file or
  // configuration_loader::watch_and_load_config_file.
  void* token;
};

// A configuration_loader has a few responsibilities:
//
// * Load the configuration file for a .js file (watch_and_load_for_file(),
//   watch_and_load_config_file(), load_for_file()).
// * Minimize reloading and reparsing of configuration files if many .js files
//   share a configuration file.
// * Query when a configuration file has changed (refresh()).
class configuration_loader {
 public:
  explicit configuration_loader(configuration_filesystem*);

  configuration_filesystem* fs() noexcept { return this->fs_; }

  // Returns nullptr if there is no config file.
  result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
  watch_and_load_for_file(const std::string& file_path, const void* token);

  // Fails if the config file does not exist.
  result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
  watch_and_load_config_file(const std::string& file_path, const void* token);

  // Returns nullptr if there is no config file.
  result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
  load_for_file(const std::string& file_path);

  // Returns nullptr if there is no config file.
  result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
  load_for_file(const file_to_lint&);

  // Undo a call to watch_and_load_for_file or watch_and_load_config_file.
  void unwatch_file(const std::string& file_path);

  // Scans the filesystem for changes to configuration files.
  //
  // refresh checks files registered with watch_and_load_for_file and
  // watch_and_load_config_file.
  //
  // There are many scenarios where the configuration might change. For example:
  //
  // * quick-lint-js.config was previously found for a .js file, and that
  //   quick-lint-js.config's content changed.
  // * A quick-lint-js.config file didn't exist, and now one does.
  // * A quick-lint-js.config file was moved into an ancestor directory.
  std::vector<configuration_change> refresh();

  // Returns true if the path might possibly be a configuration file detected by
  // load_for_file or watch_and_load_for_file.
  //
  // is_config_file_path does not inspect the filesystem.
  bool is_config_file_path(const std::string& file_path) const;

 private:
  struct found_config_file {
    std::optional<canonical_path> path;
    loaded_config_file* already_loaded = nullptr;
    padded_string file_content{};
  };

  struct watched_config_path {
    std::string input_config_path;
    std::optional<canonical_path> actual_config_path;
    result<void, canonicalize_path_io_error, read_file_io_error> error;
    void* token;
  };

  struct watched_input_path {
    std::string input_path;
    std::optional<canonical_path> config_path;
    result<void, canonicalize_path_io_error, read_file_io_error> error;
    void* token;
  };

  result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
  load_config_file(const char* config_path);
  result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
  find_and_load_config_file_for_input(const char* input_path);

  result<loaded_config_file*, read_file_io_error>
  find_and_load_config_file_in_directory_and_ancestors(canonical_path&&,
                                                       const char* input_path);
  result<found_config_file, read_file_io_error>
  find_config_file_in_directory_and_ancestors(canonical_path&&);

  result<canonical_path_result, canonicalize_path_io_error>
  get_parent_directory(const char* input_path);

  loaded_config_file* get_loaded_config(const canonical_path& path) noexcept;

  configuration_filesystem* fs_;

  // Key: config file path
  // Value: cached parsed configuration
  std::unordered_map<canonical_path, loaded_config_file> loaded_config_files_;

  std::vector<watched_config_path> watched_config_paths_;
  std::vector<watched_input_path> watched_input_paths_;
};
}

#endif

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
