// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <optional>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/span.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
struct File_To_Lint;

class Configuration_Filesystem {
 public:
  virtual ~Configuration_Filesystem() = default;

  virtual Result<Canonical_Path_Result, Canonicalize_Path_IO_Error>
  canonicalize_path(const std::string&) = 0;
  virtual Result<Padded_String, Read_File_IO_Error> read_file(
      const Canonical_Path&) = 0;
};

struct Loaded_Config_File {
  explicit Loaded_Config_File();

  Monotonic_Allocator memory{"Loaded_Config_File"};

  Configuration config;

  // The content of the quick-lint-js.config file.
  Padded_String file_content;

  // Errors discovered while parsing file_content.
  Diag_List errors;

  // The path to the quick-lint-js.config file. Never nullptr.
  const Canonical_Path* config_path;
};

struct Configuration_Load_IO_Error {
  /*implicit*/ Configuration_Load_IO_Error(Canonicalize_Path_IO_Error&&);
  /*implicit*/ Configuration_Load_IO_Error(const Canonicalize_Path_IO_Error&);

  /*implicit*/ Configuration_Load_IO_Error(Read_File_IO_Error&&);
  /*implicit*/ Configuration_Load_IO_Error(const Read_File_IO_Error&);

  std::string message;
  Platform_File_IO_Error io_error;

  // Read_File_IO_Error::path or Canonicalize_Path_IO_Error::input_path
  std::string path;

  // Empty if Read_File_IO_Error.
  std::string canonicalizing_path;

  const std::string& to_string() const;

  friend bool operator==(const Configuration_Load_IO_Error&,
                         const Configuration_Load_IO_Error&);
  friend bool operator!=(const Configuration_Load_IO_Error&,
                         const Configuration_Load_IO_Error&);
};

// Returned by Configuration_Loader::refresh.
struct Configuration_Change {
  // The path given to Configuration_Loader::watch_and_load_for_file or
  // Configuration_Loader::watch_and_load_config_file. Never nullptr.
  const std::string* watched_path;

  // If config_file is nullptr, then no configuration file exists.
  Loaded_Config_File* config_file;

  // If error is not nullptr, then error points to an I/O error which prevented
  // a configuration file from being determined at all.
  //
  // Invariant: (error == nullptr) || (config_file == nullptr)
  Configuration_Load_IO_Error* error;  // Sometimes nullptr.

  // token is the pointer given to
  // Configuration_Loader::watch_and_load_for_file or
  // Configuration_Loader::watch_and_load_config_file.
  void* token;
};

// A Configuration_Loader has a few responsibilities:
//
// * Load the configuration file for a .js file (watch_and_load_for_file(),
//   watch_and_load_config_file(), load_for_file()).
// * Minimize reloading and reparsing of configuration files if many .js files
//   share a configuration file.
// * Query when a configuration file has changed (refresh()).
class Configuration_Loader {
 public:
  explicit Configuration_Loader(Configuration_Filesystem*);
  ~Configuration_Loader();

  Configuration_Filesystem* fs() { return this->fs_; }

  // Returns nullptr if there is no config file.
  Result<Loaded_Config_File*, Configuration_Load_IO_Error>
  watch_and_load_for_file(const std::string& file_path, const void* token);

  // Fails if the config file does not exist.
  Result<Loaded_Config_File*, Configuration_Load_IO_Error>
  watch_and_load_config_file(const std::string& file_path, const void* token);

  // Returns nullptr if there is no config file.
  Result<Loaded_Config_File*, Configuration_Load_IO_Error> load_for_file(
      const std::string& file_path);

  // Returns nullptr if there is no config file.
  Result<Loaded_Config_File*, Configuration_Load_IO_Error> load_for_file(
      const File_To_Lint&);

  // Undo a call to watch_and_load_for_file or watch_and_load_config_file.
  void unwatch_file(const std::string& file_path);

  // Undo all calls to watch_and_load_for_file or watch_and_load_config_file.
  void unwatch_all_files();

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
  Span<Configuration_Change> refresh(Monotonic_Allocator*);

  // Returns true if the path might possibly be a configuration file detected by
  // load_for_file or watch_and_load_for_file.
  //
  // is_config_file_path does not inspect the filesystem.
  bool is_config_file_path(const std::string& file_path) const;

 private:
  struct Found_Config_File {
    std::optional<Canonical_Path> path;
    Loaded_Config_File* already_loaded = nullptr;
    Padded_String file_content{};
  };

  struct Watched_Config_Path {
    std::string input_config_path;
    std::optional<Canonical_Path> actual_config_path;
    std::optional<Configuration_Load_IO_Error> error;
    void* token;
  };

  struct Watched_Input_Path {
    std::string input_path;
    std::optional<Canonical_Path> config_path;
    std::optional<Configuration_Load_IO_Error> error;
    void* token;
  };

  Result<Loaded_Config_File*, Configuration_Load_IO_Error> load_config_file(
      const char* config_path);
  Result<Loaded_Config_File*, Configuration_Load_IO_Error>
  find_and_load_config_file_for_input(const char* input_path);

  Result<Loaded_Config_File*, Read_File_IO_Error>
  find_and_load_config_file_in_directory_and_ancestors(Canonical_Path&&,
                                                       const char* input_path);
  Result<Found_Config_File, Read_File_IO_Error>
  find_config_file_in_directory_and_ancestors(Canonical_Path&&);

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error>
  get_parent_directory(const char* input_path);

  Loaded_Config_File* get_loaded_config(const Canonical_Path& path);

  Configuration_Filesystem* fs_;

  // Key: config file path
  // Value: cached parsed configuration
  Hash_Map<Canonical_Path, Loaded_Config_File> loaded_config_files_;

  Vector<Watched_Config_Path> watched_config_paths_{
      "Configuration_Loader::watched_config_paths_", new_delete_resource()};
  Vector<Watched_Input_Path> watched_input_paths_{
      "Configuration_Loader::watched_input_paths_", new_delete_resource()};
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
