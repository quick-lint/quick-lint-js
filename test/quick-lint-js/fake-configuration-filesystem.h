// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FAKE_CONFIGURATION_FILESYSTEM_H
#define QUICK_LINT_JS_FAKE_CONFIGURATION_FILESYSTEM_H

#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/padded-string.h>
#include <string>
#include <unordered_map>
#include <utility>

namespace quick_lint_js {
class fake_configuration_filesystem : public configuration_filesystem {
 public:
  void create_file(const canonical_path& path, string8_view content) {
    auto [_file_it, inserted] = this->files_.try_emplace(path, content);
    QLJS_ASSERT(inserted);
  }

  canonical_path rooted(const char* path) const {
    std::string full_path;
#if defined(_WIN32)
    full_path = "X:\\";
#else
    full_path = "/";
#endif
    full_path += path;
#if defined(_WIN32)
    for (char& c : full_path) {
      if (c == '/') {
        c = '\\';
      }
    }
#endif
    return canonical_path(std::move(full_path));
  }

  string8 file_uri_prefix_8() const {
#if defined(_WIN32)
    return u8"file:///X:/";
#else
    return u8"file:///";
#endif
  }

  canonical_path_result canonicalize_path(const std::string& path) override {
    // TODO(strager): Check if path components exist.
    return canonical_path_result(std::string(path), path.size());
  }

  read_file_result read_file(const canonical_path& path) override {
    auto file_it = this->files_.find(path);
    if (file_it == this->files_.end()) {
      read_file_result result =
          read_file_result::failure("file does not exist");
      result.is_not_found_error = true;
      return result;
    }
    read_file_result result;
    result.content = padded_string(string8_view(file_it->second));
    return result;
  }

 private:
  std::unordered_map<canonical_path, string8> files_;
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
