// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <quick-lint-js/basic-configuration-filesystem.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/result.h>
#include <string>

namespace quick_lint_js {
basic_configuration_filesystem*
basic_configuration_filesystem::instance() noexcept {
  static basic_configuration_filesystem fs;
  return &fs;
}

result<canonical_path_result, canonicalize_path_io_error>
basic_configuration_filesystem::canonicalize_path(const std::string& path) {
  return quick_lint_js::canonicalize_path(path);
}

result<padded_string, read_file_io_error>
basic_configuration_filesystem::read_file(const canonical_path& path) {
  return quick_lint_js::read_file(path.c_str());
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
