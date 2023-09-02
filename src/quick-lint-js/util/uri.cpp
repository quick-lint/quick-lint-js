// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/uri.h>

namespace quick_lint_js {
String8_View uri_base_name(String8_View uri) {
  // FIXME(strager): This should properly parse query parameters.
  // FIXME(strager): Should this unescape % encoding?
  std::size_t last_slash_index = uri.rfind(u8'/');
  if (last_slash_index == uri.npos) {
    return uri;
  }
  return uri.substr(last_slash_index + 1);
}

bool uri_looks_like_typescript_definition(String8_View uri) {
  // FIXME(strager): Should this unescape % encoding?
  return uri_base_name(uri).find(u8".d."_sv) != uri.npos;
}

bool uri_looks_like_typescript_jsx(String8_View uri) {
  // FIXME(strager): Should this unescape % encoding?
  return uri_base_name(uri).find(u8".tsx"_sv) != uri.npos;
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
