// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <quick-lint-js/assert.h>
#include <quick-lint-js/debug/debug-server-fs.h>
#include <string>

namespace quick_lint_js {
std::string get_debug_server_public_directory() {
  // HACK(strager): GCC 8's std::filesystem implementation is very crashy, so
  // manually manipulate strings instead.
#if 0
  return (std::filesystem::path(__FILE__).parent_path() / "public").string();
#else
  std::string path = __FILE__;
  std::size_t dir_separator_index = path.find_last_of("/\\");
  QLJS_ALWAYS_ASSERT(dir_separator_index != path.npos);
  std::size_t file_name_index = dir_separator_index + 1;
  path.replace(file_name_index, path.size() - file_name_index, "public");
  return path;
#endif
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
