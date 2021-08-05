// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FAKE_CONFIGURATION_FILESYSTEM_H
#define QUICK_LINT_JS_FAKE_CONFIGURATION_FILESYSTEM_H

#if defined(__EMSCRIPTEN__)
// No filesystem on the web.
#else

#include <functional>
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
  using read_file_result =
      result<padded_string, read_file_io_error, watch_io_error>;

  // Create a new file, or modify an existing file.
  void create_file(const canonical_path& path, string8_view content) {
    this->files_.insert_or_assign(
        path, [content_string = string8(content)]() -> read_file_result {
          return padded_string(string8_view(content_string));
        });
  }

  void create_file(const canonical_path& path,
                   std::function<read_file_result()> callback) {
    this->files_.insert_or_assign(path, std::move(callback));
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

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string& path) override {
    // TODO(strager): Check if path components exist.
    return canonical_path_result(std::string(path), path.size());
  }

  result<padded_string, read_file_io_error, watch_io_error> read_file(
      const canonical_path& path) override {
    auto file_it = this->files_.find(path);
    if (file_it == this->files_.end()) {
#if QLJS_HAVE_WINDOWS_H
      windows_file_io_error io_error = {ERROR_FILE_NOT_FOUND};
#endif
#if QLJS_HAVE_UNISTD_H
      posix_file_io_error io_error = {ENOENT};
#endif
      return read_file_result::failure<read_file_io_error>(read_file_io_error{
          .path = std::string(path.path()),
          .io_error = io_error,
      });
    }
    return file_it->second();
  }

 private:
  std::unordered_map<canonical_path, std::function<read_file_result()> > files_;
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
