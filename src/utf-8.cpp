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

#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-8.h>

namespace quick_lint_js {
// See: https://www.unicode.org/versions/Unicode11.0.0/ch03.pdf
char8* encode_utf_8(char32_t code_point, char8* out) {
  auto byte = [](char32_t x) -> char8 {
    QLJS_ASSERT(x <= 0x100);
    return static_cast<char8>(x);
  };
  char32_t continuation = 0b1000'0000;
  if (code_point >= 0x10000) {
    *out++ = byte(0b1111'0000 | (code_point >> 18));
    *out++ = byte(continuation | ((code_point >> 12) & 0b0011'1111));
    *out++ = byte(continuation | ((code_point >> 6) & 0b0011'1111));
    *out++ = byte(continuation | ((code_point >> 0) & 0b0011'1111));
  } else if (code_point >= 0x0800) {
    *out++ = byte(0b1110'0000 | (code_point >> 12));
    *out++ = byte(continuation | ((code_point >> 6) & 0b0011'1111));
    *out++ = byte(continuation | ((code_point >> 0) & 0b0011'1111));
  } else if (code_point >= 0x80) {
    *out++ = byte(0b1100'0000 | (code_point >> 6));
    *out++ = byte(continuation | ((code_point >> 0) & 0b0011'1111));
  } else {
    *out++ = byte(code_point);
  }
  return out;
}
}
