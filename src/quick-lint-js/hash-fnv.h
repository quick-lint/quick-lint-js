// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_HASH_FNV_H
#define QUICK_LINT_JS_HASH_FNV_H

#include <cstdint>
#include <string_view>

namespace quick_lint_js {
inline constexpr std::uint64_t hash_fnv_1a_64(
    std::string_view data, std::uint64_t offset_basis) noexcept {
  std::uint64_t hash = offset_basis;
  for (char c : data) {
    hash ^= static_cast<std::uint8_t>(c);
    hash *= 0x00000100'000001b3ULL;
  }
  return hash;
}

inline constexpr std::uint64_t hash_fnv_1a_64(std::string_view data) noexcept {
  return hash_fnv_1a_64(data, 0xcbf29ce484222325ULL);
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
