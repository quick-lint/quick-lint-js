// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FAKE_CONFIGURATION_FILESYSTEM_H
#define QUICK_LINT_JS_FAKE_CONFIGURATION_FILESYSTEM_H

#if defined(__EMSCRIPTEN__)
// No filesystem on the web.
#else

#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/heap-function.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/char8.h>
#include <string>
#include <unordered_map>
#include <utility>

namespace quick_lint_js {
class fake_configuration_filesystem : public configuration_filesystem {
 public:
  using read_file_result = result<padded_string, read_file_io_error>;

  explicit fake_configuration_filesystem();
  ~fake_configuration_filesystem() override;

  // Create a new file, or modify an existing file.
  void create_file(const canonical_path& path, string8_view content);
  void create_file(const canonical_path& path,
                   heap_function<read_file_result()> callback);

  canonical_path rooted(const char* path) const;

  string8 file_uri_prefix_8() const;

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string& path) override;

  result<padded_string, read_file_io_error> read_file(
      const canonical_path& path) override;

  void clear();

 private:
  std::unordered_map<canonical_path, heap_function<read_file_result()> > files_;
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
