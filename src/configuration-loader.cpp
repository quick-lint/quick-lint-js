// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file-path.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/warning.h>
#include <string_view>
#include <unordered_map>
#include <utility>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
configuration_or_error::configuration_or_error(configuration* config)
    : config(config) {
  QLJS_ASSERT(this->config);
}

configuration_or_error::configuration_or_error(std::string&& error)
    : error(std::move(error)) {}

bool configuration_or_error::ok() const noexcept {
  return this->config != nullptr;
}

configuration& configuration_or_error::operator*() {
  QLJS_ASSERT(this->ok());
  return *this->config;
}

configuration* configuration_or_error::operator->() {
  QLJS_ASSERT(this->ok());
  return this->config;
}

configuration_loader::configuration_loader(configuration_filesystem* fs)
    : fs_(fs) {}

configuration_or_error configuration_loader::load_for_file(
    const std::string& file_path) {
  return this->find_and_load_config_file(file_path.c_str());
}

configuration_or_error configuration_loader::load_for_file(
    const file_to_lint& file) {
  if (file.config_file) {
    return this->load_config_file(file.config_file);
  } else {
    return this->find_and_load_config_file(file.path);
  }
}

configuration_or_error configuration_loader::load_config_file(
    const char* config_path) {
  canonical_path_result canonical_config_path =
      this->fs_->canonicalize_path(config_path);
  if (!canonical_config_path.ok()) {
    return configuration_or_error(std::move(canonical_config_path).error());
  }

  if (loaded_config_file* config_file =
          this->get_loaded_config(canonical_config_path.canonical())) {
    return configuration_or_error(&config_file->config);
  }
  read_file_result config_json =
      this->fs_->read_file(canonical_config_path.canonical());
  if (!config_json.ok()) {
    return configuration_or_error(std::move(config_json.error));
  }
  auto [config_it, inserted] = this->loaded_config_files_.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(canonical_config_path.canonical()),
      std::forward_as_tuple());
  QLJS_ASSERT(inserted);
  loaded_config_file* config_file = &config_it->second;
  config_file->config.set_config_file_path(
      std::move(canonical_config_path).canonical());
  config_file->config.load_from_json(&config_json.content);
  return configuration_or_error(&config_file->config);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

configuration_or_error configuration_loader::find_and_load_config_file(
    const char* input_path) {
  canonical_path_result canonical_input_path =
      this->fs_->canonicalize_path(input_path ? input_path : ".");
  if (!canonical_input_path.ok()) {
    return configuration_or_error(std::move(canonical_input_path).error());
  }

  bool should_drop_file_name = input_path != nullptr;
  if (canonical_input_path.have_missing_components()) {
    canonical_input_path.drop_missing_components();
    should_drop_file_name = false;
  }
  canonical_path parent_directory = std::move(canonical_input_path).canonical();
  if (should_drop_file_name) {
    parent_directory.parent();
  }

  // TODO(strager): Cache directory->config to reduce lookups in cases like the
  // following:
  //
  // input paths: ./a/b/c/d/1.js, ./a/b/c/d/2.js, ./a/b/c/d/3.js
  // config path: ./quick-lint-js.config

  for (;;) {
    for (const std::string_view& file_name : {
             "quick-lint-js.config"sv,
             ".quick-lint-js.config"sv,
         }) {
      canonical_path config_path = parent_directory;
      config_path.append_component(file_name);

      if (loaded_config_file* config_file =
              this->get_loaded_config(config_path)) {
        return configuration_or_error(&config_file->config);
      }

      read_file_result config_json = this->fs_->read_file(config_path);
      if (config_json.ok()) {
        auto [config_it, inserted] = this->loaded_config_files_.emplace(
            std::piecewise_construct, std::forward_as_tuple(config_path),
            std::forward_as_tuple());
        QLJS_ASSERT(inserted);
        loaded_config_file* config_file = &config_it->second;
        config_file->config.set_config_file_path(std::move(config_path));
        config_file->config.load_from_json(&config_json.content);
        return configuration_or_error(&config_file->config);
      }
      if (!config_json.is_not_found_error) {
        return configuration_or_error(std::move(config_json.error));
      }

      // Loop, looking for a different file.
    }

    // Loop, looking in parent directories.
    if (!parent_directory.parent()) {
      // We searched the root directory which has no parent.
      break;
    }
  }

  return configuration_or_error(&this->default_config_);
}

QLJS_WARNING_POP

configuration_loader::loaded_config_file*
configuration_loader::get_loaded_config(const canonical_path& path) noexcept {
  auto existing_config_it = this->loaded_config_files_.find(path);
  return existing_config_it == this->loaded_config_files_.end()
             ? nullptr
             : &existing_config_it->second;
}

void configuration_loader::refresh() { this->loaded_config_files_.clear(); }

basic_configuration_filesystem*
basic_configuration_filesystem::instance() noexcept {
  static basic_configuration_filesystem fs;
  return &fs;
}

canonical_path_result basic_configuration_filesystem::canonicalize_path(
    const std::string& path) {
  return quick_lint_js::canonicalize_path(path);
}

read_file_result basic_configuration_filesystem::read_file(
    const canonical_path& path) {
  return quick_lint_js::read_file(path.c_str());
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
