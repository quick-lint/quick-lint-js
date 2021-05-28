// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/options.h>
#include <string_view>
#include <unordered_map>

namespace quick_lint_js {
configuration* configuration_loader::load_for_file(const file_to_lint& file) {
  if (!file.config_file) {
    return &this->default_config_;
  }
  auto existing_config_it = this->loaded_config_files_.find(file.config_file);
  if (existing_config_it != this->loaded_config_files_.end()) {
    return &existing_config_it->second;
  }

  read_file_result config_json = read_file(file.config_file);
  config_json.exit_if_not_ok();
  configuration* config = &this->loaded_config_files_[file.config_file];
  config->load_from_json(&config_json.content);
  return config;
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
