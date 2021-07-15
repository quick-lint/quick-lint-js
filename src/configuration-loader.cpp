// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/leaf/handle_errors.hpp>
#include <boost/leaf/pred.hpp>
#include <boost/leaf/result.hpp>
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

QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
template <class T>
std::string error_to_string(
    const result<T, canonicalize_path_io_error, read_file_io_error,
                 platform_file_io_error>& error) {
  if (error.template has_error<canonicalize_path_io_error>()) {
    return error.template error<canonicalize_path_io_error>().to_string();
  } else if (error.template has_error<read_file_io_error>()) {
    return error.template error<read_file_io_error>().to_string();
  } else {
    QLJS_ASSERT(error.template has_error<platform_file_io_error>());
    return error.template error<platform_file_io_error>().to_string();
  }
}
}

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
    const std::string& file_path, const void* token) {
  watched_path& watch = this->watched_paths_.emplace_back(watched_path{
      .input_path = std::move(file_path),
      .config_path =
          std::nullopt,  // Updated by find_and_load_config_file_for_input.
      .error = std::string(),
      .token = const_cast<void*>(token),
  });
  result<configuration*, canonicalize_path_io_error, read_file_io_error,
         platform_file_io_error>
      r = this->find_and_load_config_file_for_input(file_path.c_str());
  if (!r.ok()) {
    std::string message = error_to_string(r);
    watch.error = message;
    return configuration_or_error(std::move(message));
  }
  return configuration_or_error(*r);
}

boost::leaf::result<configuration*> configuration_loader::load_for_file(
    const std::string& file_path) {
  result<configuration*, canonicalize_path_io_error, read_file_io_error,
         platform_file_io_error>
      r = this->load_for_file_2(file_path);
  if (!r.ok()) {
    if (r.has_error<canonicalize_path_io_error>()) {
      return r.error<canonicalize_path_io_error>().make_leaf_error();
    } else if (r.has_error<read_file_io_error>()) {
      return r.error<read_file_io_error>().make_leaf_error();
    } else {
      QLJS_ASSERT(r.has_error<platform_file_io_error>());
      return r.error<platform_file_io_error>().make_leaf_error();
    }
  }
  return *r;
}

boost::leaf::result<configuration*> configuration_loader::load_for_file(
    const file_to_lint& file) {
  result<configuration*, canonicalize_path_io_error, read_file_io_error,
         platform_file_io_error>
      r = this->load_for_file_2(file);
  if (!r.ok()) {
    if (r.has_error<canonicalize_path_io_error>()) {
      return r.error<canonicalize_path_io_error>().make_leaf_error();
    } else if (r.has_error<read_file_io_error>()) {
      return r.error<read_file_io_error>().make_leaf_error();
    } else {
      QLJS_ASSERT(r.has_error<platform_file_io_error>());
      return r.error<platform_file_io_error>().make_leaf_error();
    }
  }
  return *r;
}

result<configuration*, canonicalize_path_io_error, read_file_io_error,
       platform_file_io_error>
configuration_loader::load_for_file_2(const std::string& file_path) {
  return this->find_and_load_config_file_for_input(file_path.c_str());
}

result<configuration*, canonicalize_path_io_error, read_file_io_error,
       platform_file_io_error>
configuration_loader::load_for_file_2(const file_to_lint& file) {
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

sloppy_result<configuration*> configuration_loader::load_for_file_sloppy(
    const std::string& file_path) {
  result<configuration*, canonicalize_path_io_error, read_file_io_error,
         platform_file_io_error>
      r = this->load_for_file_2(file_path);
  if (!r.ok()) {
    std::string message = error_to_string(r);
    return sloppy_result<configuration*>::failure(std::move(message));
  }
  return *r;
}

sloppy_result<configuration*> configuration_loader::load_for_file_sloppy(
    const file_to_lint& file) {
  result<configuration*, canonicalize_path_io_error, read_file_io_error,
         platform_file_io_error>
      r = this->load_for_file_2(file);
  if (!r.ok()) {
    return sloppy_result<configuration*>::failure(error_to_string(r));
  }
  return *r;
}

result<configuration*, canonicalize_path_io_error, read_file_io_error,
       platform_file_io_error>
configuration_loader::load_config_file(const char* config_path) {
  result<canonical_path_result, canonicalize_path_io_error>
      canonical_config_path = this->fs_->canonicalize_path(config_path);
  if (!canonical_config_path.ok()) return canonical_config_path.propagate();

  if (loaded_config_file* config_file =
          this->get_loaded_config(canonical_config_path->canonical())) {
    return &config_file->config;
  }
  result<padded_string, read_file_io_error, platform_file_io_error>
      config_json = this->fs_->read_file(canonical_config_path->canonical());
  if (!config_json.ok()) return config_json.propagate();
  auto [config_it, inserted] = this->loaded_config_files_.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(canonical_config_path->canonical()),
      std::forward_as_tuple());
  QLJS_ASSERT(inserted);
  loaded_config_file* config_file = &config_it->second;
  config_file->file_content = std::move(*config_json);
  config_file->config.set_config_file_path(
      std::move(*canonical_config_path).canonical());
  config_file->config.load_from_json(&config_file->file_content);
  return &config_file->config;
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

result<configuration*, canonicalize_path_io_error, read_file_io_error,
       platform_file_io_error>
configuration_loader::find_and_load_config_file_for_input(
    const char* input_path) {
  result<canonical_path_result, canonicalize_path_io_error> parent_directory =
      this->get_parent_directory(input_path);
  if (!parent_directory.ok()) return parent_directory.propagate();
  result<configuration*, read_file_io_error, platform_file_io_error> r =
      this->find_and_load_config_file_in_directory_and_ancestors(
          std::move(*parent_directory).canonical(),
          /*input_path=*/input_path);
  if (!r.ok()) return r.propagate();
  return *r;
}

result<configuration*, canonicalize_path_io_error, read_file_io_error,
       platform_file_io_error>
configuration_loader::find_and_load_config_file_for_current_directory() {
  result<canonical_path_result, canonicalize_path_io_error> canonical_cwd =
      this->fs_->canonicalize_path(".");
  if (!canonical_cwd.ok()) return canonical_cwd.propagate();

  if (canonical_cwd->have_missing_components()) {
    canonical_cwd->drop_missing_components();
  }
  result<configuration*, read_file_io_error, platform_file_io_error> r =
      this->find_and_load_config_file_in_directory_and_ancestors(
          std::move(*canonical_cwd).canonical(), /*input_path=*/nullptr);
  if (!r.ok()) return r.propagate();
  return *r;
}

result<configuration*, read_file_io_error, platform_file_io_error>
configuration_loader::find_and_load_config_file_in_directory_and_ancestors(
    canonical_path&& parent_directory, const char* input_path) {
  result<found_config_file, read_file_io_error, platform_file_io_error> found =
      this->find_config_file_in_directory_and_ancestors(
          std::move(parent_directory));
  if (!found.ok()) return found.propagate();
  if (!found->path.has_value()) {
    return &this->default_config_;
  }
  canonical_path& config_path = *found->path;
  if (input_path) {
    for (watched_path& watch : this->watched_paths_) {
      if (watch.input_path == input_path) {
        watch.config_path = config_path;
      }
    }
  }

  if (found->already_loaded) {
    return &found->already_loaded->config;
  }

  auto [config_it, inserted] = this->loaded_config_files_.emplace(
      std::piecewise_construct, std::forward_as_tuple(config_path),
      std::forward_as_tuple());
  QLJS_ASSERT(inserted);
  loaded_config_file* config_file = &config_it->second;
  config_file->file_content = std::move(found->file_content);
  config_file->config.set_config_file_path(std::move(config_path));
  config_file->config.load_from_json(&config_file->file_content);
  return &config_file->config;
}

result<configuration_loader::found_config_file, read_file_io_error,
       platform_file_io_error>
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
        };
      }

      result<padded_string, read_file_io_error, platform_file_io_error>
          config_json = this->fs_->read_file(config_path);
      if (!config_json.ok()) {
        if (config_json.has_error<read_file_io_error>()) {
          if (config_json.error<read_file_io_error>()
                  .io_error.is_file_not_found_error()) {
            // Loop, looking for a different file.
            continue;
          }
        } else {
          QLJS_ASSERT(config_json.has_error<platform_file_io_error>());
          if (config_json.error<platform_file_io_error>()
                  .is_file_not_found_error()) {
            // Loop, looking for a different file.
            continue;
          }
        }
        return config_json.propagate();
      }
      return found_config_file{
          .path = std::move(config_path),
          .already_loaded = nullptr,
          .file_content = std::move(*config_json),
      };
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

  for (watched_path& watch : this->watched_paths_) {
    const std::string& input_path = watch.input_path;
    result<canonical_path_result, canonicalize_path_io_error> parent_directory =
        this->get_parent_directory(input_path.c_str());
    if (!parent_directory.ok()) {
      std::string message = parent_directory.error().to_string();
      if (watch.error != message) {
        watch.error = std::move(message);
        changes.emplace_back(configuration_change{
            .watched_path = &input_path,
            .config = &this->default_config_,
            .token = watch.token,
        });
      }
      continue;
    }

    std::optional<found_config_file> latest = boost::leaf::try_handle_all(
        [&]() -> boost::leaf::result<std::optional<found_config_file>> {
          result<found_config_file, read_file_io_error, platform_file_io_error>
              found = this->find_config_file_in_directory_and_ancestors(
                  std::move(*parent_directory).canonical());
          if (!found.ok()) {
            if (found.has_error<read_file_io_error>()) {
              return found.error<read_file_io_error>().make_leaf_error();
            } else {
              QLJS_ASSERT(found.has_error<platform_file_io_error>());
              return found.error<platform_file_io_error>().make_leaf_error();
            }
          }
          return std::optional<found_config_file>(std::move(*found));
        },
        make_read_file_error_handlers(
            [&](std::string&& message) -> std::optional<found_config_file> {
              if (watch.error != message) {
                watch.error = std::move(message);
                changes.emplace_back(configuration_change{
                    .watched_path = &input_path,
                    .config = &this->default_config_,
                    .token = watch.token,
                });
              }
              return std::nullopt;
            }),
        [&](e_errno error) -> std::optional<found_config_file> {
          const char* message = std::strerror(error.error);
          if (watch.error != message) {
            watch.error = message;
            changes.emplace_back(configuration_change{
                .watched_path = &input_path,
                .config = &this->default_config_,
                .token = watch.token,
            });
          }
          return std::nullopt;
        },
        []() -> std::optional<found_config_file> {
          QLJS_ASSERT(false);
          return std::nullopt;
        });
    if (!latest.has_value()) {
      continue;
    }

    if (latest->path != watch.config_path) {
      configuration* config;
      if (latest->path.has_value()) {
        auto loaded_config_it = loaded_config_files.find(*latest->path);
        if (loaded_config_it == loaded_config_files.end()) {
          loaded_config_file& loaded_config =
              loaded_config_files[*latest->path];
          loaded_config.file_content = std::move(latest->file_content);
          loaded_config.config.reset();
          loaded_config.config.set_config_file_path(*latest->path);
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
          .token = watch.token,
      });
      watch.config_path = latest->path;
    }
  }

  this->loaded_config_files_ = std::move(loaded_config_files);

  for (auto& [config_path, loaded_config] : this->loaded_config_files_) {
    // TODO(strager): Avoid reading config files again.
    // (find_config_file_in_directory_and_ancestors in the loop above already
    // read the config file.)
    result<padded_string, read_file_io_error, platform_file_io_error>
        config_json = this->fs_->read_file(config_path);
    if (!config_json.ok()) {
      continue;
    }

    bool did_change = loaded_config.file_content != *config_json;
    if (did_change) {
      loaded_config.file_content = std::move(*config_json);
      loaded_config.config.reset();
      loaded_config.config.set_config_file_path(config_path);
      loaded_config.config.load_from_json(&loaded_config.file_content);

      for (const watched_path& watch : this->watched_paths_) {
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
                .config = &loaded_config.config,
                .token = watch.token,
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

result<canonical_path_result, canonicalize_path_io_error>
basic_configuration_filesystem::canonicalize_path(const std::string& path) {
  return quick_lint_js::canonicalize_path(path);
}

result<padded_string, read_file_io_error, platform_file_io_error>
basic_configuration_filesystem::read_file(const canonical_path& path) {
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
