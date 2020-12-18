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

// See: https://www.unicode.org/versions/Unicode11.0.0/ch03.pdf
decode_utf_8_result decode_utf_8(const char8* begin,
                                 const char8* end) noexcept {
  auto is_continuation_byte = [](std::uint8_t byte) noexcept -> bool {
    return (byte & 0b1100'0000) == 0b1000'0000;
  };
  const std::uint8_t* c = reinterpret_cast<const std::uint8_t*>(begin);
  std::ptrdiff_t available_bytes = end - begin;
  if (available_bytes == 0) {
    return decode_utf_8_result{
        .size = 0,
        .code_point = 0,
        .ok = false,
    };
  } else if (c[0] <= 0x7f) {
    // 1-byte sequence (0x00..0x7f, i.e. ASCII).
    return decode_utf_8_result{
        .size = 1,
        .code_point = *c,
        .ok = true,
    };
  } else if ((c[0] & 0b1110'0000) == 0b1100'0000) {
    // 2-byte sequence (0xc0..0xdf).
    bool byte_0_ok = c[0] >= 0xc2;
    bool byte_1_ok = available_bytes >= 2 && is_continuation_byte(c[1]);
    if (byte_0_ok && byte_1_ok) {
      return decode_utf_8_result{
          .size = 2,
          .code_point =
              (char32_t(c[0] & 0b0001'1111) << 6) | (c[1] & 0b0011'1111),
          .ok = true,
      };
    } else {
      return decode_utf_8_result{
          .size = 1,
          .code_point = 0,
          .ok = false,
      };
    }
  } else if ((c[0] & 0b1111'0000) == 0b1110'0000) {
    // 3-byte sequence (0xe0..0xef).
    bool byte_1_ok = available_bytes >= 2 &&
                     (c[0] == 0xe0 ? 0xa0 <= c[1] && c[1] <= 0xbf
                                   : c[0] == 0xed ? 0x80 <= c[1] && c[1] <= 0x9f
                                                  : is_continuation_byte(c[1]));
    bool byte_2_ok = available_bytes >= 3 && is_continuation_byte(c[2]);
    if (byte_1_ok && byte_2_ok) {
      return decode_utf_8_result{
          .size = 3,
          .code_point = (char32_t(c[0] & 0b0000'1111) << 12) |
                        (char32_t(c[1] & 0b0011'1111) << 6) |
                        (c[2] & 0b0011'1111),
          .ok = true,
      };
    } else {
      return decode_utf_8_result{
          .size = byte_1_ok ? 2 : 1,
          .code_point = 0,
          .ok = false,
      };
    }
  } else if ((c[0] & 0b1111'1000) == 0b1111'0000) {
    // 4-byte sequence (0xf0..0xf7).
    bool byte_0_ok = c[0] <= 0xf4;
    bool byte_1_ok = available_bytes >= 2 &&
                     (c[0] == 0xf0 ? 0x90 <= c[1] && c[1] <= 0xbf
                                   : c[0] == 0xf4 ? 0x80 <= c[1] && c[1] <= 0x8f
                                                  : is_continuation_byte(c[1]));
    bool byte_2_ok = available_bytes >= 3 && is_continuation_byte(c[2]);
    bool byte_3_ok = available_bytes >= 4 && is_continuation_byte(c[3]);
    if (byte_0_ok && byte_1_ok && byte_2_ok && byte_3_ok) {
      return decode_utf_8_result{
          .size = 4,
          .code_point = (char32_t(c[0] & 0b0000'0111) << 18) |
                        (char32_t(c[1] & 0b0011'1111) << 12) |
                        (char32_t(c[2] & 0b0011'1111) << 6) |
                        (c[3] & 0b0011'1111),
          .ok = true,
      };
    } else {
      return decode_utf_8_result{
          .size = byte_0_ok && byte_1_ok ? byte_2_ok ? 3 : 2 : 1,
          .code_point = 0,
          .ok = false,
      };
    }
  } else {
    // Continuation byte (0x80..0xbf), or 5-byte or longer sequence
    // (0xf8..0xff).
    return decode_utf_8_result{
        .size = 1,
        .code_point = 0,
        .ok = false,
    };
  }
}
}
