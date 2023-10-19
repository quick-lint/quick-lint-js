// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/classify-path.h>
#include <quick-lint-js/util/uri.h>
#include <string_view>

namespace quick_lint_js {
Path_Classification classify_uri(String8_View uri) {
  // FIXME(strager): Should this unescape % encoding?
  return classify_file_base_name(uri_base_name(uri));
}

Path_Classification classify_path(String8_View path) {
  String8_View base_name =
      to_string8_view(path_file_name(to_string_view(path)));
  return classify_file_base_name(base_name);
}

Path_Classification classify_path(const char *path) {
  return classify_path(to_string8_view(std::string_view(path)));
}

Path_Classification classify_file_base_name(String8_View name) {
  QLJS_SLOW_ASSERT(name.find(u8'/') == name.npos);
  return Path_Classification{
      .typescript_definition = name.find(u8".d."_sv) != name.npos,
      .typescript = ends_with(name, u8".ts"_sv),
      .typescript_jsx = ends_with(name, u8".tsx"_sv),
  };
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
