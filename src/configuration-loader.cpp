// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file-path.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/warning.h>
#include <string_view>
#include <unordered_map>
#include <utility>

QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")

using namespace std::literals::string_literals;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
loaded_config_file::loaded_config_file()
    : errors(boost::container::pmr::new_delete_resource()) {}

configuration_loader::configuration_loader(configuration_filesystem* fs)
    : fs_(fs) {}

result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
configuration_loader::watch_and_load_for_file(const std::string& file_path,
                                              const void* token) {
  watched_input_path& watch =
      this->watched_input_paths_.emplace_back(watched_input_path{
          .input_path = std::move(file_path),
          .config_path =
              std::nullopt,  // Updated by find_and_load_config_file_for_input.
          .error = {},
          .token = const_cast<void*>(token),
      });
  result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
      r = this->find_and_load_config_file_for_input(file_path.c_str());
  if (!r.ok()) {
    watch.error =
        r.copy_errors<canonicalize_path_io_error, read_file_io_error>();
    return r.propagate();
  }
  return *r;
}

result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
configuration_loader::watch_and_load_config_file(const std::string& file_path,
                                                 const void* token) {
  watched_config_path& watch =
      this->watched_config_paths_.emplace_back(watched_config_path{
          .input_config_path = std::move(file_path),
          .actual_config_path = std::nullopt,
          .error = {},
          .token = const_cast<void*>(token),
      });
  result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
      r = this->load_config_file(file_path.c_str());
  if (!r.ok()) {
    watch.error =
        r.copy_errors<canonicalize_path_io_error, read_file_io_error>();
    return r.propagate();
  }
  watch.actual_config_path = *(*r)->config_path;
  return *r;
}

result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
configuration_loader::load_for_file(const std::string& file_path) {
  return this->find_and_load_config_file_for_input(file_path.c_str());
}

result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
configuration_loader::load_for_file(const file_to_lint& file) {
  if (file.config_file) {
    return this->load_config_file(file.config_file);
  }
  if (file.path_for_config_search) {
    return this->find_and_load_config_file_for_input(
        file.path_for_config_search);
  }
  if (file.is_stdin) {
    return nullptr;
  }
  QLJS_ASSERT(file.path);
  return this->find_and_load_config_file_for_input(file.path);
}

result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
configuration_loader::load_config_file(const char* config_path) {
  result<canonical_path_result, canonicalize_path_io_error>
      canonical_config_path = this->fs_->canonicalize_path(config_path);
  if (!canonical_config_path.ok()) return canonical_config_path.propagate();

  if (loaded_config_file* config_file =
          this->get_loaded_config(canonical_config_path->canonical())) {
    return config_file;
  }
  result<padded_string, read_file_io_error> config_json =
      this->fs_->read_file(canonical_config_path->canonical());
  if (!config_json.ok()) return config_json.propagate();
  auto [config_it, inserted] = this->loaded_config_files_.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(canonical_config_path->canonical()),
      std::forward_as_tuple());
  QLJS_ASSERT(inserted);
  loaded_config_file* config_file = &config_it->second;
  config_file->config_path = &config_it->first;
  config_file->file_content = std::move(*config_json);
  config_file->config.load_from_json(&config_file->file_content,
                                     &config_file->errors);
  return config_file;
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

result<loaded_config_file*, canonicalize_path_io_error, read_file_io_error>
configuration_loader::find_and_load_config_file_for_input(
    const char* input_path) {
  result<canonical_path_result, canonicalize_path_io_error> parent_directory =
      this->get_parent_directory(input_path);
  if (!parent_directory.ok()) return parent_directory.propagate();
  result<loaded_config_file*, read_file_io_error> r =
      this->find_and_load_config_file_in_directory_and_ancestors(
          std::move(*parent_directory).canonical(),
          /*input_path=*/input_path);
  if (!r.ok()) return r.propagate();
  return *r;
}

result<loaded_config_file*, read_file_io_error>
configuration_loader::find_and_load_config_file_in_directory_and_ancestors(
    canonical_path&& parent_directory, const char* input_path) {
  result<found_config_file, read_file_io_error> found =
      this->find_config_file_in_directory_and_ancestors(
          std::move(parent_directory));
  if (!found.ok()) return found.propagate();
  if (!found->path.has_value()) {
    return nullptr;
  }
  canonical_path& config_path = *found->path;
  if (input_path) {
    for (watched_input_path& watch : this->watched_input_paths_) {
      if (watch.input_path == input_path) {
        watch.config_path = config_path;
      }
    }
  }

  if (found->already_loaded) {
    return found->already_loaded;
  }

  auto [config_it, inserted] = this->loaded_config_files_.emplace(
      std::piecewise_construct, std::forward_as_tuple(config_path),
      std::forward_as_tuple());
  QLJS_ASSERT(inserted);
  loaded_config_file* config_file = &config_it->second;
  config_file->config_path = &config_it->first;
  config_file->file_content = std::move(found->file_content);
  config_file->config.load_from_json(&config_file->file_content,
                                     &config_file->errors);
  return config_file;
}

// This algorithm is documented in docs/config.adoc:
// https://quick-lint-js.com/config/#_files
result<configuration_loader::found_config_file, read_file_io_error>
configuration_loader::find_config_file_in_directory_and_ancestors(
    canonical_path&& parent_directory) {
  // TODO(strager): Cache directory->config to reduce lookups in cases like the
  // following:
  //
  // input paths: ./a/b/c/d/1.js, ./a/b/c/d/2.js, ./a/b/c/d/3.js
  // config path: ./quick-lint-js.config

  for (;;) {
    canonical_path config_path = parent_directory;
    config_path.append_component("quick-lint-js.config"sv);
    QLJS_ASSERT(this->is_config_file_path(config_path.c_str()));

    if (loaded_config_file* config_file =
            this->get_loaded_config(config_path)) {
      return found_config_file{
          .path = std::move(config_path),
          .already_loaded = config_file,
          .file_content = padded_string(),
      };
    }

    result<padded_string, read_file_io_error> config_json =
        this->fs_->read_file(config_path);
    if (!config_json.ok()) {
      if (config_json.error().io_error.is_file_not_found_error()) {
        goto not_found;
      }
      return config_json.propagate();
    }
    return found_config_file{
        .path = std::move(config_path),
        .already_loaded = nullptr,
        .file_content = std::move(*config_json),
    };

  not_found:
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
  };
}

QLJS_WARNING_POP

result<canonical_path_result, canonicalize_path_io_error>
configuration_loader::get_parent_directory(const char* input_path) {
  result<canonical_path_result, canonicalize_path_io_error>
      canonical_input_path = this->fs_->canonicalize_path(input_path);
  if (!canonical_input_path.ok()) return canonical_input_path.propagate();

  bool should_drop_file_name = true;
  if (canonical_input_path->have_missing_components()) {
    canonical_input_path->drop_missing_components();
    should_drop_file_name = false;
  }
  canonical_path parent_directory =
      std::move(*canonical_input_path).canonical();
  if (should_drop_file_name) {
    parent_directory.parent();
  }
  std::string parent_directory_string = std::move(parent_directory).path();
  return canonical_path_result(std::move(parent_directory_string),
                               parent_directory_string.size());
}

loaded_config_file* configuration_loader::get_loaded_config(
    const canonical_path& path) noexcept {
  auto existing_config_it = this->loaded_config_files_.find(path);
  return existing_config_it == this->loaded_config_files_.end()
             ? nullptr
             : &existing_config_it->second;
}

void configuration_loader::unwatch_file(const std::string& file_path) {
  this->watched_config_paths_.erase(
      std::remove_if(this->watched_config_paths_.begin(),
                     this->watched_config_paths_.end(),
                     [&](const watched_config_path& watch) {
                       return watch.input_config_path == file_path;
                     }),
      this->watched_config_paths_.end());
  this->watched_input_paths_.erase(
      std::remove_if(this->watched_input_paths_.begin(),
                     this->watched_input_paths_.end(),
                     [&](const watched_input_path& watch) {
                       return watch.input_path == file_path;
                     }),
      this->watched_input_paths_.end());
}

void configuration_loader::unwatch_all_files() {
  this->watched_config_paths_.clear();
  this->watched_input_paths_.clear();
}

std::vector<configuration_change> configuration_loader::refresh() {
  std::vector<configuration_change> changes;

  std::unordered_map<canonical_path, loaded_config_file> loaded_config_files =
      std::move(this->loaded_config_files_);

  for (watched_config_path& watch : this->watched_config_paths_) {
    result<canonical_path_result, canonicalize_path_io_error>
        canonical_config_path =
            this->fs_->canonicalize_path(watch.input_config_path);
    if (!canonical_config_path.ok()) {
      auto new_error =
          canonical_config_path
              .copy_errors<canonicalize_path_io_error, read_file_io_error>();
      if (watch.error != new_error) {
        watch.error = std::move(new_error);
        changes.emplace_back(configuration_change{
            .watched_path = &watch.input_config_path,
            .config_file = nullptr,
            .error = &watch.error,
            .token = watch.token,
        });
      }
      continue;
    }

    result<padded_string, read_file_io_error> latest_json =
        this->fs_->read_file(canonical_config_path->canonical());
    if (!latest_json.ok()) {
      auto new_error =
          latest_json
              .copy_errors<canonicalize_path_io_error, read_file_io_error>();
      if (watch.error != new_error) {
        watch.error = std::move(new_error);
        changes.emplace_back(configuration_change{
            .watched_path = &watch.input_config_path,
            .config_file = nullptr,
            .error = &watch.error,
            .token = watch.token,
        });
      }
      continue;
    }

    if (canonical_config_path->canonical() != watch.actual_config_path ||
        !watch.error.ok()) {
      loaded_config_file* config_file;
      auto loaded_config_it =
          loaded_config_files.find(canonical_config_path->canonical());
      if (loaded_config_it == loaded_config_files.end()) {
        auto [config_it, inserted] = loaded_config_files.emplace(
            std::piecewise_construct,
            std::forward_as_tuple(canonical_config_path->canonical()),
            std::forward_as_tuple());
        QLJS_ASSERT(inserted);
        loaded_config_file& loaded_config = config_it->second;
        loaded_config.config_path = &config_it->first;
        loaded_config.file_content = std::move(*latest_json);
        loaded_config.config.reset();
        loaded_config.errors.clear();
        loaded_config.config.load_from_json(&loaded_config.file_content,
                                            &loaded_config.errors);
        config_file = &loaded_config;
      } else {
        config_file = &loaded_config_it->second;
      }
      changes.emplace_back(configuration_change{
          .watched_path = &watch.input_config_path,
          .config_file = config_file,
          .error = nullptr,
          .token = watch.token,
      });
      watch.actual_config_path = canonical_config_path->canonical();
      watch.error =
          result<void, canonicalize_path_io_error, read_file_io_error>();
    }
  }

  for (watched_input_path& watch : this->watched_input_paths_) {
    const std::string& input_path = watch.input_path;
    result<canonical_path_result, canonicalize_path_io_error> parent_directory =
        this->get_parent_directory(input_path.c_str());
    if (!parent_directory.ok()) {
      auto new_error =
          parent_directory
              .copy_errors<canonicalize_path_io_error, read_file_io_error>();
      if (watch.error != new_error) {
        watch.error = std::move(new_error);
        changes.emplace_back(configuration_change{
            .watched_path = &input_path,
            .config_file = nullptr,
            .error = &watch.error,
            .token = watch.token,
        });
      }
      continue;
    }

    result<found_config_file, read_file_io_error> latest =
        this->find_config_file_in_directory_and_ancestors(
            std::move(*parent_directory).canonical());
    if (!latest.ok()) {
      auto new_error =
          latest.copy_errors<canonicalize_path_io_error, read_file_io_error>();
      if (watch.error != new_error) {
        watch.error = std::move(new_error);
        changes.emplace_back(configuration_change{
            .watched_path = &input_path,
            .config_file = nullptr,
            .error = &watch.error,
            .token = watch.token,
        });
      }
      continue;
    }

    if (latest->path != watch.config_path || !watch.error.ok()) {
      loaded_config_file* config_file;
      if (latest->path.has_value()) {
        auto loaded_config_it = loaded_config_files.find(*latest->path);
        if (loaded_config_it == loaded_config_files.end()) {
          auto [config_it, inserted] = loaded_config_files.emplace(
              std::piecewise_construct, std::forward_as_tuple(*latest->path),
              std::forward_as_tuple());
          QLJS_ASSERT(inserted);
          loaded_config_file& loaded_config = config_it->second;
          loaded_config.config_path = &config_it->first;
          loaded_config.file_content = std::move(latest->file_content);
          loaded_config.config.reset();
          loaded_config.errors.clear();
          loaded_config.config.load_from_json(&loaded_config.file_content,
                                              &loaded_config.errors);
          config_file = &loaded_config;
        } else {
          config_file = &loaded_config_it->second;
        }
      } else {
        config_file = nullptr;
      }
      changes.emplace_back(configuration_change{
          .watched_path = &input_path,
          .config_file = config_file,
          .error = nullptr,
          .token = watch.token,
      });
      watch.config_path = latest->path;
      watch.error =
          result<void, canonicalize_path_io_error, read_file_io_error>();
    }
  }

  this->loaded_config_files_ = std::move(loaded_config_files);

  for (auto& [config_path, loaded_config] : this->loaded_config_files_) {
    // TODO(strager): Avoid reading config files again.
    // (find_config_file_in_directory_and_ancestors in the loop above already
    // read the config file.)
    result<padded_string, read_file_io_error> config_json =
        this->fs_->read_file(config_path);
    if (!config_json.ok()) {
      continue;
    }

    bool did_change = loaded_config.file_content != *config_json;
    if (did_change) {
      QLJS_ASSERT(*loaded_config.config_path == config_path);
      loaded_config.file_content = std::move(*config_json);
      loaded_config.config.reset();
      loaded_config.errors.clear();
      loaded_config.config.load_from_json(&loaded_config.file_content,
                                          &loaded_config.errors);

      for (const watched_config_path& watch : this->watched_config_paths_) {
        if (watch.actual_config_path == config_path) {
          auto existing_change_it = std::find_if(
              changes.begin(), changes.end(),
              [&](const configuration_change& change) {
                return *change.watched_path == watch.input_config_path &&
                       change.token == watch.token;
              });
          bool already_changed = existing_change_it != changes.end();
          if (!already_changed) {
            changes.emplace_back(configuration_change{
                .watched_path = &watch.input_config_path,
                .config_file = &loaded_config,
                .error = nullptr,
                .token = watch.token,
            });
          }
        }
      }

      for (const watched_input_path& watch : this->watched_input_paths_) {
        if (watch.config_path == config_path) {
          auto existing_change_it =
              std::find_if(changes.begin(), changes.end(),
                           [&](const configuration_change& change) {
                             return *change.watched_path == watch.input_path;
                           });
          bool already_changed = existing_change_it != changes.end();
          if (!already_changed) {
            changes.emplace_back(configuration_change{
                .watched_path = &watch.input_path,
                .config_file = &loaded_config,
                .error = nullptr,
                .token = watch.token,
            });
          }
        }
      }
    }
  }

  return changes;
}

bool configuration_loader::is_config_file_path(
    const std::string& file_path) const {
#if defined(_WIN32)
#define QLJS_PREFERRED_PATH_SEPARATOR "\\"
#else
#define QLJS_PREFERRED_PATH_SEPARATOR "/"
#endif
  return ends_with(file_path,
                   QLJS_PREFERRED_PATH_SEPARATOR "quick-lint-js.config");
}
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
