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

configuration_or_error configuration_loader::watch_and_load_for_file(
    const std::string& file_path) {
  this->watched_paths_.emplace_back(file_path);
  return this->find_and_load_config_file_for_input(file_path.c_str());
}

configuration_or_error configuration_loader::load_for_file(
    const std::string& file_path) {
  return this->find_and_load_config_file_for_input(file_path.c_str());
}

configuration_or_error configuration_loader::load_for_file(
    const file_to_lint& file) {
  if (file.config_file) {
    return this->load_config_file(file.config_file);
  } else {
    if (file.path) {
      return this->find_and_load_config_file_for_input(file.path);
    } else {
      return this->find_and_load_config_file_for_current_directory();
    }
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
  config_file->file_content = std::move(config_json.content);
  config_file->config.set_config_file_path(
      std::move(canonical_config_path).canonical());
  config_file->config.load_from_json(&config_file->file_content);
  return configuration_or_error(&config_file->config);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

configuration_or_error
configuration_loader::find_and_load_config_file_for_input(
    const char* input_path) {
  auto input_path_config_file_it =
      this->input_path_config_files_.find(input_path);
  if (input_path_config_file_it != this->input_path_config_files_.end()) {
    const canonical_path& config_path = input_path_config_file_it->second;
    auto config_file_it = this->loaded_config_files_.find(config_path);
    QLJS_ASSERT(config_file_it != this->loaded_config_files_.end());
    return configuration_or_error(&config_file_it->second.config);
  }

  canonical_path_result parent_directory =
      this->get_parent_directory(input_path);
  if (!parent_directory.ok()) {
    return configuration_or_error(std::move(parent_directory).error());
  }
  return this->find_and_load_config_file_in_directory_and_ancestors(
      std::move(parent_directory).canonical(), /*input_path=*/input_path);
}

configuration_or_error
configuration_loader::find_and_load_config_file_for_current_directory() {
  canonical_path_result canonical_cwd = this->fs_->canonicalize_path(".");
  if (!canonical_cwd.ok()) {
    return configuration_or_error(std::move(canonical_cwd).error());
  }

  if (canonical_cwd.have_missing_components()) {
    canonical_cwd.drop_missing_components();
  }
  return this->find_and_load_config_file_in_directory_and_ancestors(
      std::move(canonical_cwd).canonical(), /*input_path=*/nullptr);
}

configuration_or_error
configuration_loader::find_and_load_config_file_in_directory_and_ancestors(
    canonical_path&& parent_directory, const char* input_path) {
  found_config_file found = this->find_config_file_in_directory_and_ancestors(
      std::move(parent_directory));
  if (!found.error.empty()) {
    return configuration_or_error(std::move(found.error));
  }
  if (!found.path.has_value()) {
    return configuration_or_error(&this->default_config_);
  }
  canonical_path& config_path = *found.path;
  if (input_path) {
    auto [_it, inserted] =
        this->input_path_config_files_.try_emplace(input_path, config_path);
    QLJS_ASSERT(inserted);
  }

  if (found.already_loaded) {
    return configuration_or_error(&found.already_loaded->config);
  }

  auto [config_it, inserted] = this->loaded_config_files_.emplace(
      std::piecewise_construct, std::forward_as_tuple(config_path),
      std::forward_as_tuple());
  QLJS_ASSERT(inserted);
  loaded_config_file* config_file = &config_it->second;
  config_file->file_content = std::move(found.file_content);
  config_file->config.set_config_file_path(std::move(config_path));
  config_file->config.load_from_json(&config_file->file_content);
  return configuration_or_error(&config_file->config);
}

configuration_loader::found_config_file
configuration_loader::find_config_file_in_directory_and_ancestors(
    canonical_path&& parent_directory) {
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
        return found_config_file{
            .path = std::move(config_path),
            .already_loaded = config_file,
            .file_content = padded_string(),
            .error = std::string(),
        };
      }

      read_file_result config_json = this->fs_->read_file(config_path);
      if (config_json.ok()) {
        return found_config_file{
            .path = std::move(config_path),
            .already_loaded = nullptr,
            .file_content = std::move(config_json.content),
            .error = std::string(),
        };
      }
      if (!config_json.is_not_found_error) {
        return found_config_file{
            .path = std::move(config_path),
            .already_loaded = nullptr,
            .file_content = padded_string(),
            .error = std::move(config_json.error),
        };
      }

      // Loop, looking for a different file.
    }

    // Loop, looking in parent directories.
    if (!parent_directory.parent()) {
      // We searched the root directory which has no parent.
      break;
    }
  }

  return found_config_file{
      .path = std::nullopt,
      .already_loaded = nullptr,
      .file_content = padded_string(),
      .error = std::string(),
  };
}

QLJS_WARNING_POP

canonical_path_result configuration_loader::get_parent_directory(
    const char* input_path) {
  canonical_path_result canonical_input_path =
      this->fs_->canonicalize_path(input_path);
  if (!canonical_input_path.ok()) {
    return canonical_input_path;
  }

  bool should_drop_file_name = true;
  if (canonical_input_path.have_missing_components()) {
    canonical_input_path.drop_missing_components();
    should_drop_file_name = false;
  }
  canonical_path parent_directory = std::move(canonical_input_path).canonical();
  if (should_drop_file_name) {
    parent_directory.parent();
  }
  std::string parent_directory_string = std::move(parent_directory).path();
  return canonical_path_result(std::move(parent_directory_string),
                               parent_directory_string.size());
}

configuration_loader::loaded_config_file*
configuration_loader::get_loaded_config(const canonical_path& path) noexcept {
  auto existing_config_it = this->loaded_config_files_.find(path);
  return existing_config_it == this->loaded_config_files_.end()
             ? nullptr
             : &existing_config_it->second;
}

std::vector<configuration_change> configuration_loader::refresh() {
  std::vector<configuration_change> changes;

  std::unordered_map<canonical_path, loaded_config_file> loaded_config_files =
      std::move(this->loaded_config_files_);

  for (const std::string& input_path : this->watched_paths_) {
    canonical_path_result parent_directory =
        this->get_parent_directory(input_path.c_str());
    if (!parent_directory.ok()) {
      // TODO(strager): Should we report a change?
      continue;
    }
    found_config_file latest =
        this->find_config_file_in_directory_and_ancestors(
            std::move(parent_directory).canonical());

    auto old_config_path_it = this->input_path_config_files_.find(input_path);
    std::optional<canonical_path> old_config_path =
        old_config_path_it == this->input_path_config_files_.end()
            ? std::nullopt
            : std::optional<canonical_path>(old_config_path_it->second);

    if (latest.path != old_config_path) {
      configuration* config;
      if (latest.path.has_value()) {
        auto loaded_config_it = loaded_config_files.find(*latest.path);
        if (loaded_config_it == loaded_config_files.end()) {
          loaded_config_file& loaded_config = loaded_config_files[*latest.path];
          loaded_config.file_content = std::move(latest.file_content);
          loaded_config.config.reset();
          loaded_config.config.set_config_file_path(*latest.path);
          loaded_config.config.load_from_json(&loaded_config.file_content);
          config = &loaded_config.config;
        } else {
          config = &loaded_config_it->second.config;
        }
      } else {
        config = &this->default_config_;
      }
      changes.emplace_back(configuration_change{
          .watched_path = &input_path,
          .config = config,
      });
      if (latest.path.has_value()) {
        this->input_path_config_files_.insert_or_assign(input_path,
                                                        *latest.path);
      } else {
        this->input_path_config_files_.erase(input_path);
      }
    }
  }

  this->loaded_config_files_ = std::move(loaded_config_files);

  for (auto& [config_path, loaded_config] : this->loaded_config_files_) {
    // TODO(strager): Avoid reading config files again.
    // (find_config_file_in_directory_and_ancestors in the loop above already
    // read the config file.)
    read_file_result config_json = this->fs_->read_file(config_path);
    if (!config_json.ok()) {
      continue;
    }

    bool did_change = loaded_config.file_content != config_json.content;
    if (did_change) {
      loaded_config.file_content = std::move(config_json.content);
      loaded_config.config.reset();
      loaded_config.config.set_config_file_path(config_path);
      loaded_config.config.load_from_json(&loaded_config.file_content);

      for (const auto& entry : this->input_path_config_files_) {
        const std::string& input_path = entry.first;
        const canonical_path& input_config_path = entry.second;
        if (input_config_path == config_path) {
          auto existing_change_it =
              std::find_if(changes.begin(), changes.end(),
                           [&](const configuration_change& change) {
                             return *change.watched_path == input_path;
                           });
          bool already_changed = existing_change_it != changes.end();
          if (!already_changed) {
            changes.emplace_back(configuration_change{
                .watched_path = &input_path,
                .config = &loaded_config.config,
            });
          }
        }
      }
    }
  }

  return changes;
}

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
