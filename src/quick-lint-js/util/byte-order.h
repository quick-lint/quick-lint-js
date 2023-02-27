// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_BYTE_ORDER_H
#define QUICK_LINT_JS_UTIL_BYTE_ORDER_H

#include <cstdint>

namespace quick_lint_js {
inline std::uint32_t load_u32_le(const void* data) {
  const unsigned char* d = reinterpret_cast<const unsigned char*>(data);
  return (static_cast<std::uint32_t>(d[0]) << (8 * 0)) |
         (static_cast<std::uint32_t>(d[1]) << (8 * 1)) |
         (static_cast<std::uint32_t>(d[2]) << (8 * 2)) |
         (static_cast<std::uint32_t>(d[3]) << (8 * 3));
}

inline std::uint64_t load_u64_le(const void* data) {
  const unsigned char* d = reinterpret_cast<const unsigned char*>(data);
  return (static_cast<std::uint64_t>(d[0]) << (8 * 0)) |
         (static_cast<std::uint64_t>(d[1]) << (8 * 1)) |
         (static_cast<std::uint64_t>(d[2]) << (8 * 2)) |
         (static_cast<std::uint64_t>(d[3]) << (8 * 3)) |
         (static_cast<std::uint64_t>(d[4]) << (8 * 4)) |
         (static_cast<std::uint64_t>(d[5]) << (8 * 5)) |
         (static_cast<std::uint64_t>(d[6]) << (8 * 6)) |
         (static_cast<std::uint64_t>(d[7]) << (8 * 7));
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
