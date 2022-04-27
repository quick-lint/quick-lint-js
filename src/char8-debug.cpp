// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/char8.h>
#include <string_view>

namespace quick_lint_js {
#if QLJS_HAVE_CHAR8_T
std::ostream &operator<<(std::ostream &out, streamable_string8_view sv) {
  out << std::string_view(reinterpret_cast<const char *>(sv.sv_.data()),
                          sv.sv_.size());
  return out;
}
#endif
}

namespace testing::internal {
#if QLJS_HAVE_CHAR8_T
template <>
void PrintTo(const char8_t &c, std::ostream *out) {
  *out << static_cast<char>(c);
}

template <>
void PrintTo(const char8_t *const &s, std::ostream *out) {
  *out << reinterpret_cast<const char *>(s);
}

template <>
void PrintTo(char8_t *const &s, std::ostream *out) {
  PrintTo(const_cast<const char8_t *>(s), out);
}
#endif
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
