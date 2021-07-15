// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-8.h>
#include <quick-lint-js/warning.h>

#define EXPECT_ENCODE_UTF_8(code_point, expected)        \
  do {                                                   \
    string8 out((expected).size(), u8'\0');              \
    char8* end = encode_utf_8((code_point), out.data()); \
    EXPECT_EQ(end - out.data(), (expected).size());      \
    EXPECT_EQ(out, (expected));                          \
  } while (false)

#define EXPECT_DECODE_UTF_8_SINGLE_CODE_POINT(input, expected) \
  do {                                                         \
    padded_string in = (input);                                \
    decode_utf_8_result result = decode_utf_8(&in);            \
    EXPECT_EQ(result.size, in.size());                         \
    EXPECT_TRUE(result.ok);                                    \
    EXPECT_EQ(result.code_point, (expected));                  \
  } while (false)

namespace quick_lint_js {
TEST(test_utf_8_encode, ascii) {
  std::array<char8, 1> out;
  char8* end = encode_utf_8(U'x', out.data());
  EXPECT_EQ(end - out.data(), 1);
  EXPECT_EQ(out, make_array(u8'x'));
}

TEST(test_utf_8_encode, one_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\u0000', "\x00"_s8v);
  EXPECT_ENCODE_UTF_8(U'\u007f', "\x7f"_s8v);
}

TEST(test_utf_8_encode, two_byte_output) {
  EXPECT_ENCODE_UTF_8(U'\u00a2', "\xc2\xa2"_s8v);
}

TEST(test_utf_8_encode, two_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\u0080', "\xc2\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\u07ff', "\xdf\xbf"_s8v);
}

TEST(test_utf_8_encode, three_byte_output) {
  EXPECT_ENCODE_UTF_8(U'\u0939', "\xe0\xa4\xb9"_s8v);
  EXPECT_ENCODE_UTF_8(U'\u20ac', "\xe2\x82\xac"_s8v);
  EXPECT_ENCODE_UTF_8(U'\ud55c', "\xed\x95\x9c"_s8v);
}

TEST(test_utf_8_encode, three_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\u0800', "\xe0\xa0\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\ud7ff', "\xed\x9f\xbf"_s8v);
  EXPECT_ENCODE_UTF_8(U'\ue000', "\xee\x80\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\uffff', "\xef\xbf\xbf"_s8v);
}

TEST(test_utf_8_encode, non_standard_surrogate_code_points) {
  EXPECT_ENCODE_UTF_8(U'\xd800', "\xed\xa0\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\xdfff', "\xed\xbf\xbf"_s8v);
}

TEST(test_utf_8_encode, four_byte_output) {
  EXPECT_ENCODE_UTF_8(U'\U00010348', "\xf0\x90\x8d\x88"_s8v);
}

TEST(test_utf_8_encode, four_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\U00010000', "\xf0\x90\x80\x80"_s8v);
  EXPECT_ENCODE_UTF_8(U'\U0010ffff', "\xf4\x8f\xbf\xbf"_s8v);
}

TEST(test_utf_8_encode, non_standard_four_byte_output_extremes) {
  EXPECT_ENCODE_UTF_8(U'\x001fffff', "\xf7\xbf\xbf\xbf"_s8v);
}

namespace {
decode_utf_8_result decode_utf_8(const padded_string& code_units) noexcept {
  return quick_lint_js::decode_utf_8(&code_units);
}

#if QLJS_HAVE_CHAR8_T
padded_string operator""_padded(const char8_t* chars,
                                std::size_t size) noexcept {
  return padded_string(string8_view(chars, size));
}
#endif

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
padded_string operator""_padded(const char* chars, std::size_t size) noexcept {
  return padded_string(
      string8_view(reinterpret_cast<const char8*>(chars), size));
}
QLJS_WARNING_POP
}

TEST(test_utf_8_decode, empty_string) {
  decode_utf_8_result result = decode_utf_8(u8""_padded);
  EXPECT_EQ(result.size, 0);
  EXPECT_FALSE(result.ok);
}

TEST(test_utf_8_decode, ascii) {
  {
    decode_utf_8_result result = decode_utf_8(u8"a"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_TRUE(result.ok);
    EXPECT_EQ(result.code_point, U'a');
  }

  {
    decode_utf_8_result result = decode_utf_8(u8"12345"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_TRUE(result.ok);
    EXPECT_EQ(result.code_point, U'1');
  }
}

TEST(test_utf_8_decode, leading_continuation_code_unit_is_an_error) {
  {
    decode_utf_8_result result = decode_utf_8("\xa2"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xa2\xa2\xa2"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wimplicit-int-conversion")
QLJS_WARNING_IGNORE_GCC("-Wconversion")
TEST(test_utf_8_decode, always_invalid_code_unit_is_an_error) {
  for (char8 code_unit : {
           0xc0,
           0xc1,
           0xf5,
           0xf6,
           0xf7,
           0xf8,
           0xf9,
           0xfa,
           0xfb,
           0xfc,
           0xfd,
           0xfe,
           0xff,
       }) {
    SCOPED_TRACE(static_cast<int>(code_unit));

    {
      padded_string input(string8() + code_unit);
      decode_utf_8_result result = decode_utf_8(&input);
      EXPECT_EQ(result.size, 1);
      EXPECT_FALSE(result.ok);
    }

    {
      padded_string input(code_unit + string8(u8"?????"));
      decode_utf_8_result result = decode_utf_8(&input);
      EXPECT_EQ(result.size, 1);
      EXPECT_FALSE(result.ok);
    }

    {
      padded_string input(code_unit + string8("\xa2\xa2\xa2\xa2"_s8v));
      decode_utf_8_result result = decode_utf_8(&input);
      EXPECT_EQ(result.size, 1);
      EXPECT_FALSE(result.ok);
    }
  }
}
QLJS_WARNING_POP

TEST(test_utf_8_decode, two_byte_character) {
  EXPECT_DECODE_UTF_8_SINGLE_CODE_POINT("\xc2\xa2"_padded, U'\u00a2');
}

TEST(test_utf_8_decode, truncated_two_byte_character) {
  {
    decode_utf_8_result result = decode_utf_8("\xc2"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xc2?"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xc2\xc2"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }
}

TEST(test_utf_8_decode, two_byte_character_with_trailing_continuation_bytes) {
  {
    decode_utf_8_result result = decode_utf_8("\xc2\xa2\xa2"_padded);
    EXPECT_EQ(result.size, 2);
    EXPECT_TRUE(result.ok);
    EXPECT_EQ(result.code_point, U'\u00a2');
  }
}

TEST(test_utf_8_decode, three_byte_character) {
  EXPECT_DECODE_UTF_8_SINGLE_CODE_POINT("\xe0\xa4\xb9"_padded, U'\u0939');
  EXPECT_DECODE_UTF_8_SINGLE_CODE_POINT("\xe2\x82\xac"_padded, U'\u20ac');
  EXPECT_DECODE_UTF_8_SINGLE_CODE_POINT("\xed\x95\x9c"_padded, U'\ud55c');
}

TEST(test_utf_8_decode, truncated_three_byte_character) {
  {
    decode_utf_8_result result = decode_utf_8("\xe0\xa4"_padded);
    EXPECT_EQ(result.size, 2);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xe0\xa4???"_padded);
    EXPECT_EQ(result.size, 2);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xe0"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xe0?"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xe0????"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }
}

TEST(test_utf_8_decode, four_byte_character) {
  EXPECT_DECODE_UTF_8_SINGLE_CODE_POINT("\xf0\x90\x8d\x88"_padded,
                                        U'\U00010348');
}

TEST(test_utf_8_decode, truncated_four_byte_character) {
  {
    decode_utf_8_result result = decode_utf_8("\xf0\x90\x8d"_padded);
    EXPECT_EQ(result.size, 3);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xf0\x90\x8d?????"_padded);
    EXPECT_EQ(result.size, 3);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xf0\x90"_padded);
    EXPECT_EQ(result.size, 2);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xf0\x90?"_padded);
    EXPECT_EQ(result.size, 2);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xf0\x90??????"_padded);
    EXPECT_EQ(result.size, 2);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xf0"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xf0?"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xf0??"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }

  {
    decode_utf_8_result result = decode_utf_8("\xf0????????"_padded);
    EXPECT_EQ(result.size, 1);
    EXPECT_FALSE(result.ok);
  }
}

TEST(test_utf_8_decode, overlong_sequences_are_an_error_for_each_code_unit) {
  for (const padded_string& input : {
           "\xc0\x80"_padded,                  // U+0000
           "\xe0\x80\x80"_padded,              // U+0000
           "\xf0\x80\x80\x80"_padded,          // U+0000
           "\xf8\x80\x80\x80\x80"_padded,      // U+0000
           "\xfc\x80\x80\x80\x80\x80"_padded,  // U+0000

           "\xc0\xaf"_padded,                  // U+002F
           "\xe0\x80\xaf"_padded,              // U+002F
           "\xf0\x80\x80\xaf"_padded,          // U+002F
           "\xf8\x80\x80\x80\xaf"_padded,      // U+002F
           "\xfc\x80\x80\x80\x80\xaf"_padded,  // U+002F

           "\xc1\xbf"_padded,                  // U+007F
           "\xe0\x9f\xbf"_padded,              // U+07FF
           "\xf0\x8f\xbf\xbf"_padded,          // U+FFFF
           "\xf8\x87\xbf\xbf\xbf"_padded,      // U+001FFFFF
           "\xfc\x83\xbf\xbf\xbf\xbf"_padded,  // U+03FFFFFF
       }) {
    SCOPED_TRACE(input);

    const char8* begin = input.data();
    while (begin != input.null_terminator()) {
      padded_string_view current_input(begin, input.null_terminator());
      SCOPED_TRACE(current_input);
      decode_utf_8_result result = decode_utf_8(current_input);
      EXPECT_EQ(result.size, 1);
      EXPECT_FALSE(result.ok);
      ASSERT_GE(result.size, 1);
      begin += result.size;
    }
  }
}

TEST(test_utf_8_decode, surrogate_sequences_are_an_error_for_each_code_unit) {
  for (const padded_string& input : {
           "\xed\xa0\x80"_padded,  // U+D800
           "\xed\xad\xbf"_padded,  // U+DB7F
           "\xed\xae\x80"_padded,  // U+DB80
           "\xed\xaf\xbf"_padded,  // U+DBFF
           "\xed\xb0\x80"_padded,  // U+DC00
           "\xed\xbe\x80"_padded,  // U+DF80
           "\xed\xbf\xbf"_padded,  // U+DFFF
       }) {
    SCOPED_TRACE(input);

    const char8* begin = input.data();
    while (begin != input.null_terminator()) {
      padded_string_view current_input(begin, input.null_terminator());
      SCOPED_TRACE(current_input);
      decode_utf_8_result result = decode_utf_8(current_input);
      EXPECT_EQ(result.size, 1);
      EXPECT_FALSE(result.ok);
      ASSERT_GE(result.size, 1);
      begin += result.size;
    }
  }
}

TEST(test_advance_lsp_characters_in_utf_8, empty_string) {
  string8_view s = u8""_sv;
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 0), s.data());
}

TEST(test_advance_lsp_characters_in_utf_8, out_of_bounds_gives_end_of_string) {
  {
    string8_view s = u8""_sv;
    EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 1), s.data() + s.size());
  }

  {
    string8_view s = u8"hello"_sv;
    EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 5), s.data() + s.size());
  }

  {
    string8_view s = u8"hello"_sv;
    EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 500), s.data() + s.size());
  }

  {
    string8_view s = u8"hello\u2306world"_sv;
    EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 11), s.data() + s.size());
  }

  {
    string8_view s = u8"hello\u2306world"_sv;
    EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 12), s.data() + s.size());
  }

  {
    string8_view s = u8"hello\u2306world"_sv;
    EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 500), s.data() + s.size());
  }
}

TEST(test_advance_lsp_characters_in_utf_8, ascii) {
  string8_view s = u8"hello"_sv;
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 0), &s[0]);  // h
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 1), &s[1]);  // e
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 2), &s[2]);  // l
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 3), &s[3]);  // l
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 4), &s[4]);  // o
}

TEST(test_advance_lsp_characters_in_utf_8, two_byte_character) {
  string8_view s = u8"\u0101\u0202x"_sv;
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 0), &s[0]);  // \u0101
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 1), &s[2]);  // \u0202
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 2), &s[4]);  // x
}

TEST(test_advance_lsp_characters_in_utf_8, three_byte_character) {
  string8_view s = u8"\u0808\ufefex"_sv;
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 0), &s[0]);  // \u0808
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 1), &s[3]);  // \ufefe
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 2), &s[6]);  // x
}

TEST(test_advance_lsp_characters_in_utf_8, four_byte_character) {
  // Characters with four UTF-8 code units have two UTF-16 code units.
  string8_view s = u8"\U00010000\U0010ffffx"_sv;
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 0), &s[0]);  // \U00010000
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 2), &s[4]);  // \U0010ffff
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 4), &s[8]);  // x
}

TEST(test_advance_lsp_characters_in_utf_8, middle_of_four_byte_character) {
  // Characters with four UTF-8 code units have two UTF-16 code units.
  string8_view s = u8"\U00010000\U0010ffffx"_sv;
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 1), &s[0]);  // \U00010000
  EXPECT_EQ(advance_lsp_characters_in_utf_8(s, 3), &s[4]);  // \U0010ffff
}

TEST(test_advance_lsp_characters_in_utf_8,
     invalid_utf_8_is_one_character_per_byte) {
  for (string8_view input : {
           // Surrogate sequences:
           "\xed\xa0\x80"_s8v,  // U+D800
           "\xed\xad\xbf"_s8v,  // U+DB7F
           "\xed\xae\x80"_s8v,  // U+DB80
           "\xed\xaf\xbf"_s8v,  // U+DBFF
           "\xed\xb0\x80"_s8v,  // U+DC00
           "\xed\xbe\x80"_s8v,  // U+DF80
           "\xed\xbf\xbf"_s8v,  // U+DFFF

           // Overlong sequences:
           "\xc0\x80"_s8v,                  // U+0000
           "\xe0\x80\x80"_s8v,              // U+0000
           "\xf0\x80\x80\x80"_s8v,          // U+0000
           "\xf8\x80\x80\x80\x80"_s8v,      // U+0000
           "\xfc\x80\x80\x80\x80\x80"_s8v,  // U+0000

           "\xc0\xaf"_s8v,                  // U+002F
           "\xe0\x80\xaf"_s8v,              // U+002F
           "\xf0\x80\x80\xaf"_s8v,          // U+002F
           "\xf8\x80\x80\x80\xaf"_s8v,      // U+002F
           "\xfc\x80\x80\x80\x80\xaf"_s8v,  // U+002F

           "\xc1\xbf"_s8v,                  // U+007F
           "\xe0\x9f\xbf"_s8v,              // U+07FF
           "\xf0\x8f\xbf\xbf"_s8v,          // U+FFFF
           "\xf8\x87\xbf\xbf\xbf"_s8v,      // U+001FFFFF
           "\xfc\x83\xbf\xbf\xbf\xbf"_s8v,  // U+03FFFFFF

           // Incomplete sequences:
           "\xf0\x90\x8d"_s8v,
           "\xf0\x90\x8d?????"_s8v,
           "\xf0\x90"_s8v,
           "\xf0\x90?"_s8v,
           "\xf0\x90??????"_s8v,
           "\xf0"_s8v,
           "\xf0?"_s8v,
           "\xf0??"_s8v,
           "\xf0????????"_s8v,
       }) {
    SCOPED_TRACE(out_string8(input));
    for (std::size_t i = 0; i < input.size(); ++i) {
      SCOPED_TRACE(i);
      EXPECT_EQ(advance_lsp_characters_in_utf_8(input, narrow_cast<int>(i)),
                &input[i]);
    }
  }
}

namespace {
std::ptrdiff_t count_lsp_characters_in_utf_8(
    padded_string_view utf_8) noexcept {
  return quick_lint_js::count_lsp_characters_in_utf_8(utf_8, utf_8.size());
}

std::ptrdiff_t count_lsp_characters_in_utf_8(
    const padded_string& utf_8) noexcept {
  return count_lsp_characters_in_utf_8(&utf_8);
}

std::ptrdiff_t count_lsp_characters_in_utf_8(const padded_string& utf_8,
                                             int offset) noexcept {
  return quick_lint_js::count_lsp_characters_in_utf_8(&utf_8, offset);
}
}

TEST(test_count_lsp_characters_in_utf_8, empty_string) {
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8""_padded), 0);
}

TEST(test_count_lsp_characters_in_utf_8, ascii_characters_count_as_one) {
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"abcdef"_padded), 6);
}

TEST(test_count_lsp_characters_in_utf_8,
     non_ascii_basic_multilingual_plane_characters_count_as_one) {
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"\u2306"_padded), 1);
}

TEST(test_count_lsp_characters_in_utf_8,
     supplementary_plane_characters_count_as_two) {
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"\U0001F430"_padded), 2);
}

TEST(test_count_lsp_characters_in_utf_8,
     middle_of_single_character_is_not_counted) {
  // U+0100 has two UTF-8 code units.
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"\u0100"_padded, 1), 0);
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"x\u0100y"_padded, 2), 1);

  // U+2306 has three UTF-8 code units.
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"\u2306"_padded, 1), 0);
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"\u2306"_padded, 2), 0);

  // U+1F430 has four UTF-8 code units.
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"\U0001F430"_padded, 1), 0);
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"\U0001F430"_padded, 2), 0);
  EXPECT_EQ(count_lsp_characters_in_utf_8(u8"\U0001F430"_padded, 3), 0);
}

TEST(test_count_lsp_characters_in_utf_8,
     invalid_surrogate_sequences_count_as_one_per_byte) {
  for (const padded_string& input : {
           "\xed\xa0\x80"_padded,  // U+D800
           "\xed\xad\xbf"_padded,  // U+DB7F
           "\xed\xae\x80"_padded,  // U+DB80
           "\xed\xaf\xbf"_padded,  // U+DBFF
           "\xed\xb0\x80"_padded,  // U+DC00
           "\xed\xbe\x80"_padded,  // U+DF80
           "\xed\xbf\xbf"_padded,  // U+DFFF
       }) {
    SCOPED_TRACE(input);
    EXPECT_EQ(count_lsp_characters_in_utf_8(input, input.size()), input.size());
  }
}

TEST(test_count_lsp_characters_in_utf_8,
     overlong_sequences_count_as_one_per_byte) {
  for (const padded_string& input : {
           "\xc0\x80"_padded,                  // U+0000
           "\xe0\x80\x80"_padded,              // U+0000
           "\xf0\x80\x80\x80"_padded,          // U+0000
           "\xf8\x80\x80\x80\x80"_padded,      // U+0000
           "\xfc\x80\x80\x80\x80\x80"_padded,  // U+0000

           "\xc0\xaf"_padded,                  // U+002F
           "\xe0\x80\xaf"_padded,              // U+002F
           "\xf0\x80\x80\xaf"_padded,          // U+002F
           "\xf8\x80\x80\x80\xaf"_padded,      // U+002F
           "\xfc\x80\x80\x80\x80\xaf"_padded,  // U+002F

           "\xc1\xbf"_padded,                  // U+007F
           "\xe0\x9f\xbf"_padded,              // U+07FF
           "\xf0\x8f\xbf\xbf"_padded,          // U+FFFF
           "\xf8\x87\xbf\xbf\xbf"_padded,      // U+001FFFFF
           "\xfc\x83\xbf\xbf\xbf\xbf"_padded,  // U+03FFFFFF
       }) {
    SCOPED_TRACE(input);
    EXPECT_EQ(count_lsp_characters_in_utf_8(input, input.size()), input.size());
  }
}

TEST(test_count_lsp_characters_in_utf_8,
     incomplete_sequences_count_as_one_per_byte) {
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0\x90\x8d"_padded), 3);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0\x90\x8d?????"_padded), 8);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0\x90"_padded), 2);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0\x90?"_padded), 3);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0\x90??????"_padded), 8);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0"_padded), 1);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0?"_padded), 2);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0??"_padded), 3);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0????????"_padded), 9);

  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0\x90"_padded, 1), 1);
  EXPECT_EQ(count_lsp_characters_in_utf_8("\xf0\x90??"_padded, 1), 1);
}

namespace {
std::size_t count_utf_8_characters(padded_string_view utf_8) noexcept {
  return quick_lint_js::count_utf_8_characters(
      utf_8, static_cast<std::size_t>(utf_8.size()));
}

std::size_t count_utf_8_characters(const padded_string& utf_8) noexcept {
  return count_utf_8_characters(&utf_8);
}

std::size_t count_utf_8_characters(const padded_string& utf_8,
                                   std::size_t offset) noexcept {
  return count_utf_8_characters(&utf_8, offset);
}
}

TEST(test_count_utf_8_characters, empty_string) {
  std::size_t n = count_utf_8_characters(u8""_padded);
  EXPECT_EQ(n, 0);
}

TEST(test_count_utf_8_characters, ascii) {
  std::size_t n = count_utf_8_characters(u8"foobar"_padded);
  EXPECT_EQ(n, 6);
}

TEST(test_count_utf_8_characters, ascii_num) {
  std::size_t n = count_utf_8_characters(u8"1,2,3,4"_padded);
  EXPECT_EQ(n, 7);
}

TEST(test_count_utf_8_characters, multi_byte) {
  std::size_t n = count_utf_8_characters(u8"\u263a\u263b\u2639"_padded);
  EXPECT_EQ(n, 3);
}

TEST(test_count_utf_8_characters, multi_byte_offset) {
  std::size_t n = count_utf_8_characters(u8"\u263a\u263b\u2639"_padded, 6);
  EXPECT_EQ(n, 2);
}

TEST(test_count_utf_8_characters, invalid_counts_as_one) {
  std::size_t n = count_utf_8_characters(u8"\xe2\x80"_padded);
  EXPECT_EQ(n, 2);
}

TEST(test_count_utf_8_characters, invalid_conuts_as_one_with_null) {
  std::size_t n = count_utf_8_characters(u8"\xe2\x00"_padded);
  EXPECT_EQ(n, 2);
}

TEST(test_count_utf_8_characters, mixed_ascii_with_invalid) {
  std::size_t n = count_utf_8_characters(u8"a\xe2\x80"_padded);
  EXPECT_EQ(n, 3);
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
