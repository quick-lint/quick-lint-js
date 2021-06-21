// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONFIGURATION_LOADER_H
#define QUICK_LINT_JS_CONFIGURATION_LOADER_H

#include <quick-lint-js/configuration.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file.h>
#include <string>
#include <string_view>
#include <unordered_map>

namespace quick_lint_js {
struct file_to_lint;

class configuration_filesystem {
 public:
  virtual ~configuration_filesystem() = default;

  virtual canonical_path_result canonicalize_path(const std::string&) = 0;
  virtual read_file_result read_file(const canonical_path&) = 0;
};

struct configuration_or_error {
  explicit configuration_or_error(configuration* config);
  explicit configuration_or_error(std::string&& error);

  bool ok() const noexcept;

  configuration& operator*();
  configuration* operator->();

  configuration* config = nullptr;
  std::string error;
};

class configuration_loader {
 public:
  explicit configuration_loader(configuration_filesystem*);

  configuration_or_error load_for_file(const std::string& file_path);
  configuration_or_error load_for_file(const file_to_lint&);

  void refresh();

 private:
  struct loaded_config_file {
    configuration config;
  };

  struct found_config_file {
    std::optional<canonical_path> path;
    loaded_config_file* already_loaded = nullptr;
    padded_string file_content{};
    std::string error;
  };

  configuration_or_error load_config_file(const char* config_path);
  configuration_or_error find_and_load_config_file_for_input(
      const char* input_path);
  configuration_or_error find_and_load_config_file_for_current_directory();

  configuration_or_error find_and_load_config_file_in_directory_and_ancestors(
      canonical_path&&, const char* input_path);
  found_config_file find_config_file_in_directory_and_ancestors(
      canonical_path&&);

  loaded_config_file* get_loaded_config(const canonical_path& path) noexcept;

  configuration_filesystem* fs_;
  configuration default_config_;

  // Key: an input file path
  // Value: key to loaded_config_files_ (config file path)
  std::unordered_map<std::string, canonical_path> input_path_config_files_;

  // Key: config file path
  // Value: cached parsed configuration
  std::unordered_map<canonical_path, loaded_config_file> loaded_config_files_;
};

class basic_configuration_filesystem : public configuration_filesystem {
 public:
  static basic_configuration_filesystem* instance() noexcept;

  canonical_path_result canonicalize_path(const std::string&) override;
  read_file_result read_file(const canonical_path&) override;
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
