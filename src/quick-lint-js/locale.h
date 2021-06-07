// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LOCALE_H
#define QUICK_LINT_JS_LOCALE_H

#include <cstring>
#include <string>
#include <vector>

namespace quick_lint_js {
template <class T>
struct locale_entry {
  char locale[10];
  T data = T();

  bool valid() const noexcept { return this->locale[0] != '\0'; }

  bool has_locale_name(std::string_view name) const noexcept {
    return std::string_view(this->locale) == name;
  }
};

template <class T>
const locale_entry<T>* find_locale_entry(const locale_entry<T>* files,
                                         const char* locale_name);
extern template const locale_entry<const std::uint8_t*>* find_locale_entry(
    const locale_entry<const std::uint8_t*>* files, const char* locale_name);
extern template const locale_entry<int>* find_locale_entry(
    const locale_entry<int>* files, const char* locale_name);

std::vector<std::string> locale_name_combinations(const char* locale_name);
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
