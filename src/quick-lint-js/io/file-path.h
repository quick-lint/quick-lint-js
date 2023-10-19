// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/cpp.h>
#include <string>

#if defined(_WIN32)
#define QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR "\\"
#define QLJS_ALL_PATH_DIRECTORY_SEPARATORS "\\/"
#else
#define QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR "/"
#define QLJS_ALL_PATH_DIRECTORY_SEPARATORS "/"
#endif

#define QLJS_ALL_PATH_DIRECTORY_SEPARATORS_SV \
  QLJS_CPP_CONCAT(QLJS_ALL_PATH_DIRECTORY_SEPARATORS, sv)

namespace quick_lint_js {
std::string parent_path(std::string&&);

std::string_view path_file_name(std::string_view);
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
