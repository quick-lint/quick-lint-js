// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <cstdint>
#include <cstring>
#include <ostream>
#include <quick-lint-js/simd.h>

namespace quick_lint_js {
std::ostream& operator<<(std::ostream& out, bool_vector_16_sse2 x) {
  std::uint8_t bytes[sizeof(x.data_)];
  std::memcpy(&bytes, &x.data_, sizeof(bytes));

  bool need_comma = false;
  out << '{';
  for (std::uint8_t byte : bytes) {
    if (need_comma) {
      out << ',';
    }
    if (byte == 0x00) {
      out << '0';
    } else if (byte == 0xff) {
      out << '1';
    } else {
      out << '?';
    }
    need_comma = true;
  }
  out << '}';

  return out;
}
}  // namespace quick_lint_js
