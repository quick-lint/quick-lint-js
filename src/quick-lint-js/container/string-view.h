// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_STRING_VIEW_H
#define QUICK_LINT_JS_CONTAINER_STRING_VIEW_H

#include <cstddef>
#include <quick-lint-js/util/narrow-cast.h>
#include <string_view>

namespace quick_lint_js {
// C++20 std::string_view constructor
template <class Char>
inline std::basic_string_view<Char> make_string_view(const Char* begin,
                                                     const Char* end) {
  return std::basic_string_view<Char>(begin,
                                      narrow_cast<std::size_t>(end - begin));
}

template <class Char>
inline bool starts_with(std::basic_string_view<Char> haystack,
                        std::basic_string_view<Char> needle) noexcept {
  return haystack.substr(0, needle.size()) == needle;
}

inline bool ends_with(std::string_view haystack,
                      std::string_view needle) noexcept {
  return haystack.size() >= needle.size() &&
         haystack.substr(haystack.size() - needle.size()) == needle;
}

inline std::string_view remove_suffix_if_present(
    std::string_view s, std::string_view suffix) noexcept {
  if (ends_with(s, suffix)) {
    s.remove_suffix(suffix.size());
  }
  return s;
}

inline bool contains(std::string_view haystack, std::string_view needle) {
  return haystack.find(needle) != haystack.npos;
}

template <class Char>
inline bool contains(std::basic_string_view<Char> haystack,
                     Char needle) noexcept {
  return haystack.find(needle) != haystack.npos;
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
