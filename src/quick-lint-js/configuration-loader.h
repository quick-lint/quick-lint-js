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

class configuration_loader {
 public:
  explicit configuration_loader(configuration_filesystem*);

  configuration* load_for_file(const std::string& file_path);
  configuration* load_for_file(const file_to_lint&);

  std::string error() const;

  void refresh();

 private:
  configuration* load_config_file(const char* config_path);
  configuration* find_and_load_config_file(const char* input_path);

  configuration* get_loaded_config(const canonical_path& path) noexcept;

  configuration_filesystem* fs_;
  configuration default_config_;
  std::unordered_map<canonical_path, configuration> loaded_config_files_;
  std::string last_error_;
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
