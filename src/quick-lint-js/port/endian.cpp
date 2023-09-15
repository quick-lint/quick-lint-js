// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <cstring>
#include <quick-lint-js/port/endian.h>
#include <quick-lint-js/port/span.h>

namespace quick_lint_js {
void read_little_endian_in_place(Span<char16_t> data) {
  // TODO(strager): Optimize this on little endian architectures. Clang doesn't
  // optimize this loop away itself. (It does optimize away the stores, though.)
  for (char16_t& c : data) {
    std::uint8_t bytes[2];
    std::memcpy(&bytes, &c, 2);
    c = static_cast<char16_t>(static_cast<char16_t>(bytes[0]) |
                              (static_cast<char16_t>(bytes[1]) << 8));
  }
}

void write_little_endian_in_place(Span<char16_t> data) {
  // TODO(strager): Optimize this on little endian architectures.
  for (char16_t& c : data) {
    std::uint8_t bytes[2] = {
        static_cast<std::uint8_t>(c),
        static_cast<std::uint8_t>(c >> 8),
    };
    std::memcpy(&c, &bytes, 2);
  }
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
