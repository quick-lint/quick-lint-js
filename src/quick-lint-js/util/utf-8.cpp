// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/utf-8.h>

namespace quick_lint_js {
// See: https://www.unicode.org/versions/Unicode11.0.0/ch03.pdf
Char8* encode_utf_8(char32_t code_point, Char8* out) {
  auto append = [&out](char32_t x) -> void {
    QLJS_ASSERT(x <= 0xff);
    *out = static_cast<Char8>(x);
    ++out;
  };
  char32_t cont_flag = 0b1000'0000;  // 'cont' is short for 'continuation'.
  char32_t cont_mask = 0b0011'1111;
  // clang-format off
  if (code_point >= 0x10000) {
    append((code_point >> (3 * 6)            ) | 0b1111'0000);
    append((code_point >> (2 * 6) & cont_mask) | cont_flag);
    append((code_point >> (1 * 6) & cont_mask) | cont_flag);
    append((code_point >> (0 * 6) & cont_mask) | cont_flag);
  } else if (code_point >= 0x0800) {
    append((code_point >> (2 * 6)            ) | 0b1110'0000);
    append((code_point >> (1 * 6) & cont_mask) | cont_flag);
    append((code_point >> (0 * 6) & cont_mask) | cont_flag);
  } else if (code_point >= 0x80) {
    append((code_point >> (1 * 6)            ) | 0b1100'0000);
    append((code_point >> (0 * 6) & cont_mask) | cont_flag);
  } else {
    append(code_point);
  }
  // clang-format on
  return out;
}

namespace {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wattributes")
// See: https://www.unicode.org/versions/Unicode11.0.0/ch03.pdf
[[gnu::always_inline]] Decode_UTF8_Result
    decode_utf_8_inline(Padded_String_View input) {
  auto is_continuation_byte = [](std::uint8_t byte) -> bool {
    return (byte & 0b1100'0000) == 0b1000'0000;
  };
  const std::uint8_t* c = reinterpret_cast<const std::uint8_t*>(input.data());
  if (input.size() == 0) {
    return Decode_UTF8_Result{
        .size = 0,
        .code_point = 0,
        .ok = false,
    };
  } else if (c[0] <= 0x7f) {
    // 1-byte sequence (0x00..0x7f, i.e. ASCII).
    return Decode_UTF8_Result{
        .size = 1,
        .code_point = *c,
        .ok = true,
    };
  } else if ((c[0] & 0b1110'0000) == 0b1100'0000) {
    // 2-byte sequence (0xc0..0xdf).
    static_assert(Padded_String::padding_size >= 1);
    bool byte_0_ok = c[0] >= 0xc2;
    bool byte_1_ok = is_continuation_byte(c[1]);
    if (byte_0_ok && byte_1_ok) {
      return Decode_UTF8_Result{
          .size = 2,
          .code_point =
              (char32_t(c[0] & 0b0001'1111) << 6) | (c[1] & 0b0011'1111),
          .ok = true,
      };
    } else {
      return Decode_UTF8_Result{
          .size = 1,
          .code_point = 0,
          .ok = false,
      };
    }
  } else if ((c[0] & 0b1111'0000) == 0b1110'0000) {
    // 3-byte sequence (0xe0..0xef).
    static_assert(Padded_String::padding_size >= 2);
    // clang-format off
    bool byte_1_ok = (c[0] == 0xe0 ? 0xa0 <= c[1] && c[1] <= 0xbf
                    : c[0] == 0xed ? 0x80 <= c[1] && c[1] <= 0x9f
                    : is_continuation_byte(c[1]));
    // clang-format on
    bool byte_2_ok = is_continuation_byte(c[2]);
    if (byte_1_ok && byte_2_ok) {
      return Decode_UTF8_Result{
          .size = 3,
          .code_point = (char32_t(c[0] & 0b0000'1111) << 12) |
                        (char32_t(c[1] & 0b0011'1111) << 6) |
                        (c[2] & 0b0011'1111),
          .ok = true,
      };
    } else {
      return Decode_UTF8_Result{
          .size = byte_1_ok ? 2 : 1,
          .code_point = 0,
          .ok = false,
      };
    }
  } else if ((c[0] & 0b1111'1000) == 0b1111'0000) {
    // 4-byte sequence (0xf0..0xf7).
    static_assert(Padded_String::padding_size >= 3);
    bool byte_0_ok = c[0] <= 0xf4;
    bool byte_1_ok = (c[0] == 0xf0 ? 0x90 <= c[1] && c[1] <= 0xbf
                                   : c[0] == 0xf4 ? 0x80 <= c[1] && c[1] <= 0x8f
                                                  : is_continuation_byte(c[1]));
    bool byte_2_ok = is_continuation_byte(c[2]);
    bool byte_3_ok = is_continuation_byte(c[3]);
    if (byte_0_ok && byte_1_ok && byte_2_ok && byte_3_ok) {
      return Decode_UTF8_Result{
          .size = 4,
          .code_point = (char32_t(c[0] & 0b0000'0111) << 18) |
                        (char32_t(c[1] & 0b0011'1111) << 12) |
                        (char32_t(c[2] & 0b0011'1111) << 6) |
                        (c[3] & 0b0011'1111),
          .ok = true,
      };
    } else {
      return Decode_UTF8_Result{
          .size = byte_0_ok && byte_1_ok ? byte_2_ok ? 3 : 2 : 1,
          .code_point = 0,
          .ok = false,
      };
    }
  } else {
    // Continuation byte (0x80..0xbf), or 5-byte or longer sequence
    // (0xf8..0xff).
    return Decode_UTF8_Result{
        .size = 1,
        .code_point = 0,
        .ok = false,
    };
  }
}
QLJS_WARNING_POP
}

Decode_UTF8_Result decode_utf_8(Padded_String_View input) {
  return decode_utf_8_inline(input);
}

const Char8* advance_lsp_characters_in_utf_8(String8_View utf_8,
                                             int character_count) {
  const Char8* c = utf_8.data();
  const Char8* end = c + utf_8.size();
  if (narrow_cast<std::size_t>(character_count) >= utf_8.size()) {
    return end;
  }

  // TODO(strager): Avoid this copy!
  Padded_String utf_8_copy(utf_8);
  c = utf_8_copy.data();
  end = utf_8_copy.null_terminator();

  int characters = 0;
  while (characters < character_count && c != end) {
    Decode_UTF8_Result result = decode_utf_8_inline(Padded_String_View(c, end));
    if (result.ok && result.code_point >= 0x10000) {
      // Count non-BMP characters as two characters.
      if (characters + 1 >= character_count) {
        // Middle of implied UTF-16 surrogate pair. Stop at the beginning of
        // the character. We can't reasonably return the middle of the
        // character; there is no valid middle of a UTF-8 character.
        break;
      }
      c += result.size;
      characters += 2;
    } else if (result.ok) {
      c += result.size;
      characters += 1;
    } else {
      // Count invalid sequences as one character per byte.
      c += 1;
      characters += 1;
    }
  }

  return utf_8.data() + (c - utf_8_copy.data());
}

std::ptrdiff_t count_lsp_characters_in_utf_8(Padded_String_View utf_8,
                                             int offset) {
  const Char8* c = utf_8.data();
  const Char8* end = utf_8.null_terminator();
  const Char8* stop = c + offset;
  std::ptrdiff_t count = 0;
  while (c < stop) {
    Decode_UTF8_Result result = decode_utf_8(Padded_String_View(c, end));
    if (result.ok) {
      if (c + result.size > stop) {
        break;
      }
      c += result.size;
      if (result.code_point >= 0x10000) {
        count += 2;
      } else {
        count += 1;
      }
    } else {
      c += 1;
      count += 1;
    }
  }
  return count;
}

std::size_t count_utf_8_characters(Padded_String_View utf_8,
                                   std::size_t offset) {
  const Char8* c = utf_8.data();
  const Char8* end = utf_8.null_terminator();
  const Char8* stop = c + offset;
  std::size_t count = 0;

  while (c < stop) {
    Decode_UTF8_Result result = decode_utf_8(Padded_String_View(c, end));
    if (!result.ok) {
      c++;
      count += 1;
      continue;
    }

    if (c + result.size > stop) {
      break;
    }
    c += result.size;
    count += 1;
  }

  return count;
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
