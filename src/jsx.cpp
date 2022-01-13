// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/jsx.h>
#include <string_view>
#include <unordered_map>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
const std::unordered_map<string8_view, jsx_attribute> &jsx_attribute_aliases() {
  static const std::unordered_map<string8_view, jsx_attribute> aliases{
      {u8"onclick"sv, {u8"onClick"sv}},
      {u8"onmouseenter"sv, {u8"onMouseEnter"sv}},
      // TODO(strager): Add more event handler aliases (onFoo).

      // TODO(strager): Should these be aliases or something else?
      {u8"colspan"sv, {u8"colSpan"sv}},

      // TODO(strager): Add more attribute aliases (e.g. class -> className).
  };
  return aliases;
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
