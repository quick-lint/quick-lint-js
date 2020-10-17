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

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/locale.h>
#include <string_view>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_gmo, empty_gmo_has_no_strings) {
  constexpr const std::uint8_t gmo_file_data[] = {
      0xde, 0x12, 0x04, 0x95,  // magic (little endian)
      0,    0,    0,    0,     // revision
      0,    0,    0,    0,     // number of strings
      0,    0,    0,    0,     // original strings offset
      0,    0,    0,    0,     // translated strings offset
      0,    0,    0,    0,     // hash table size
      0,    0,    0,    0,     // hash table offset
  };
  gmo_file p(gmo_file_data);
  EXPECT_EQ(p.string_count(), 0);
}

// clang-format off
constexpr const std::uint8_t little_endian_single_string_gmo_file[] = {
    0xde, 0x12, 0x04, 0x95,  // 0x00: magic (little endian)

    0,    0, 0,  0,  // 0x04: revision
    1,    0, 0,  0,  // 0x08: number of strings
    0x1c, 0, 0,  0,  // 0x0c: original strings offset
    0x24, 0, 0,  0,  // 0x10: translated strings offset
    0,    0, 0,  0,  // 0x14: hash table size
    0,    0, 0,  0,  // 0x18: hash table offset

    2,    0, 0, 0,  // 0x1c: original_strings[0].length
    0x2c, 0, 0, 0,  // 0x20: original_strings[0].offset

    2,    0, 0, 0,  // 0x24: translated_strings[0].length
    0x2f, 0, 0, 0,  // 0x28: translated_strings[0].offset

    u8'h', u8'i', u8'\0',  // 0x2c: original_strings[0] data
    u8'y', u8'o', u8'\0',  // 0x2f: translated_strings[0] data
};
// clang-format on

TEST(test_gmo, little_endian_gmo_with_one_string) {
  gmo_file p(little_endian_single_string_gmo_file);

  EXPECT_EQ(p.string_count(), 1);
  EXPECT_EQ(p.original_string_at(0), "hi"sv);
  EXPECT_EQ(p.translated_string_at(0), "yo"sv);

  EXPECT_EQ(p.find_translation("hi"sv), "yo"sv);
}

// clang-format off
constexpr const std::uint8_t big_endian_two_string_gmo_file[] = {
    0x95, 0x04, 0x12, 0xde,  // 0x00: magic (big endian)

    0, 0, 0, 0,     // 0x04: revision
    0, 0, 0, 2,     // 0x08: number of strings
    0, 0, 0, 0x1c,  // 0x0c: original strings offset
    0, 0, 0, 0x2c,  // 0x10: translated strings offset
    0, 0, 0, 0,     // 0x14: hash table size
    0, 0, 0, 0,     // 0x18: hash table offset

    0, 0, 0, 5,     // 0x1c: original_strings[0].length
    0, 0, 0, 0x3c,  // 0x20: original_strings[0].offset
    0, 0, 0, 7,     // 0x24: original_strings[1].length
    0, 0, 0, 0x42,  // 0x28: original_strings[1].offset

    0, 0, 0, 7,     // 0x2c: translated_strings[0].length
    0, 0, 0, 0x4a,  // 0x30: translated_strings[0].offset
    0, 0, 0, 9,     // 0x34: translated_strings[1].length
    0, 0, 0, 0x52,  // 0x38: translated_strings[1].offset

    // 0x3c: original_strings[0] data
    u8'h', u8'e', u8'l', u8'l', u8'o', u8'\0',
    // 0x42: original_strings[1] data
    u8'g', u8'o', u8'o', u8'd', u8'b', u8'y', u8'e', u8'\0',
    // 0x4a: translated_strings[0] data
    u8'b', u8'o', u8'n', u8'j', u8'o', u8'u', u8'r', u8'\0',
    // 0x52: translated_strings[1] data
    u8'a', u8'u', u8' ', u8'r', u8'e', u8'v', u8'o', u8'i', u8'r',
    u8'\0',  //
};
// clang-format on

TEST(test_gmo, big_endian_gmo_with_two_strings) {
  gmo_file p(big_endian_two_string_gmo_file);

  EXPECT_EQ(p.string_count(), 2);
  EXPECT_EQ(p.original_string_at(0), "hello"sv);
  EXPECT_EQ(p.translated_string_at(0), "bonjour"sv);
  EXPECT_EQ(p.original_string_at(1), "goodbye"sv);
  EXPECT_EQ(p.translated_string_at(1), "au revoir"sv);

  EXPECT_EQ(p.find_translation("hello"sv), "bonjour"sv);
  EXPECT_EQ(p.find_translation("goodbye"sv), "au revoir"sv);
}

TEST(test_gmo, missing_translation_gives_original_string) {
  gmo_file p(big_endian_two_string_gmo_file);

  std::string_view message = "does not exist"sv;
  std::string_view translated = p.find_translation(message);
  EXPECT_EQ(translated, message);
  EXPECT_EQ(translated.data(), message.data());
}

TEST(test_gmo, find_with_hash_table_with_no_collisions) {
  // clang-format off
  constexpr const std::uint8_t gmo_file_data[] = {
      0xde, 0x12, 0x04, 0x95,  // 0x00: magic (little endian)

      0,    0, 0,  0,  // 0x04: revision
      3,    0, 0,  0,  // 0x08: number of strings
      0x1c, 0, 0,  0,  // 0x0c: original strings offset
      0x34, 0, 0,  0,  // 0x10: translated strings offset
      5,    0, 0,  0,  // 0x14: hash table size
      0x4c, 0, 0,  0,  // 0x18: hash table offset

      1,    0, 0, 0,  // 0x1c: original_strings[0].length
      0x60, 0, 0, 0,  // 0x20: original_strings[0].offset
      1,    0, 0, 0,  // 0x24: original_strings[1].length
      0x62, 0, 0, 0,  // 0x28: original_strings[1].offset
      1,    0, 0, 0,  // 0x2c: original_strings[2].length
      0x64, 0, 0, 0,  // 0x30: original_strings[2].offset

      1,    0, 0, 0,  // 0x34: translated_strings[0].length
      0x66, 0, 0, 0,  // 0x38: translated_strings[0].offset
      1,    0, 0, 0,  // 0x3c: translated_strings[1].length
      0x68, 0, 0, 0,  // 0x40: translated_strings[1].offset
      1,    0, 0, 0,  // 0x44: translated_strings[2].length
      0x6a, 0, 0, 0,  // 0x48: translated_strings[2].offset

      0, 0, 0, 0,  // 0x4c: hash_table[0]
      0, 0, 0, 0,  // 0x50: hash_table[1]
      1, 0, 0, 0,  // 0x54: hash_table[2]
      2, 0, 0, 0,  // 0x58: hash_table[3]
      3, 0, 0, 0,  // 0x5c: hash_table[4]

      u8'a', u8'\0',  // 0x60: original_strings[0] data
      u8'b', u8'\0',  // 0x62: original_strings[1] data
      u8'c', u8'\0',  // 0x64: original_strings[2] data

      u8'A', u8'\0',  // 0x66: translated_strings[0] data
      u8'B', u8'\0',  // 0x68: translated_strings[1] data
      u8'C', u8'\0',  // 0x6a: translated_strings[2] data
  };
  // clang-format on
  gmo_file p(gmo_file_data);

  EXPECT_EQ(p.find_translation("a"sv), "A"sv);
  EXPECT_EQ(p.find_translation("b"sv), "B"sv);
  EXPECT_EQ(p.find_translation("c"sv), "C"sv);

  EXPECT_EQ(p.find_translation("a"_gmo_message), "A"sv);
  EXPECT_EQ(p.find_translation("b"_gmo_message), "B"sv);
  EXPECT_EQ(p.find_translation("c"_gmo_message), "C"sv);

  // Possible collisions:
  for (std::string_view message : {"d"sv, "e"sv, "f"sv, "g"sv, "h"sv}) {
    EXPECT_EQ(p.find_translation(message), message);
  }
}

TEST(test_gmo, find_with_hash_table_with_collisions) {
  // clang-format off
  constexpr const std::uint8_t gmo_file_data[] = {
      0xde, 0x12, 0x04, 0x95,  // 0x00: magic (little endian)

      0,    0, 0,  0,  // 0x04: revision
      4,    0, 0,  0,  // 0x08: number of strings
      0x1c, 0, 0,  0,  // 0x0c: original strings offset
      0x3c, 0, 0,  0,  // 0x10: translated strings offset
      5,    0, 0,  0,  // 0x14: hash table size
      0x5c, 0, 0,  0,  // 0x18: hash table offset

      1,    0, 0, 0,  // 0x1c: original_strings[0].length
      0x70, 0, 0, 0,  // 0x20: original_strings[0].offset
      1,    0, 0, 0,  // 0x24: original_strings[1].length
      0x72, 0, 0, 0,  // 0x28: original_strings[1].offset
      1,    0, 0, 0,  // 0x2c: original_strings[2].length
      0x74, 0, 0, 0,  // 0x30: original_strings[2].offset
      1,    0, 0, 0,  // 0x34: original_strings[3].length
      0x76, 0, 0, 0,  // 0x38: original_strings[3].offset

      1,    0, 0, 0,  // 0x3c: translated_strings[0].length
      0x78, 0, 0, 0,  // 0x40: translated_strings[0].offset
      1,    0, 0, 0,  // 0x44: translated_strings[1].length
      0x7a, 0, 0, 0,  // 0x48: translated_strings[1].offset
      1,    0, 0, 0,  // 0x4c: translated_strings[2].length
      0x7c, 0, 0, 0,  // 0x50: translated_strings[2].offset
      1,    0, 0, 0,  // 0x54: translated_strings[3].length
      0x7e, 0, 0, 0,  // 0x58: translated_strings[3].offset

      1, 0, 0, 0,  // 0x5c: hash_table[0]
      2, 0, 0, 0,  // 0x60: hash_table[1]
      4, 0, 0, 0,  // 0x64: hash_table[2]
      3, 0, 0, 0,  // 0x68: hash_table[3]
      0, 0, 0, 0,  // 0x6c: hash_table[4]

      u8'd', u8'\0',  // 0x70: original_strings[0] data
      u8'i', u8'\0',  // 0x72: original_strings[1] data
      u8'n', u8'\0',  // 0x74: original_strings[2] data
      u8's', u8'\0',  // 0x78: original_strings[3] data

      u8'D', u8'\0',  // 0x78: translated_strings[0] data
      u8'I', u8'\0',  // 0x7a: translated_strings[1] data
      u8'N', u8'\0',  // 0x7c: translated_strings[2] data
      u8'S', u8'\0',  // 0x7e: translated_strings[3] data
  };
  // clang-format on
  gmo_file p(gmo_file_data);

  EXPECT_EQ(p.find_translation("d"sv), "D"sv);
  EXPECT_EQ(p.find_translation("i"sv), "I"sv);
  EXPECT_EQ(p.find_translation("n"sv), "N"sv);
  EXPECT_EQ(p.find_translation("s"sv), "S"sv);
}

TEST(test_gmo, hash_string) {
  EXPECT_EQ(gmo_file::hash_string(""sv), 0);
  EXPECT_EQ(gmo_file::hash_string("a"sv), 97);
  EXPECT_EQ(gmo_file::hash_string("ab"sv), 1650);
  EXPECT_EQ(gmo_file::hash_string("abc"sv), 26499);
  EXPECT_EQ(gmo_file::hash_string("bc"sv), 1667);
  EXPECT_EQ(gmo_file::hash_string("c"sv), 99);
  EXPECT_EQ(gmo_file::hash_string("b"sv), 98);
  EXPECT_EQ(gmo_file::hash_string("aaaaaaaaaa"sv), 124846081);
  EXPECT_EQ(gmo_file::hash_string("aaaaaaaaaaaaaaaaaaaa"sv), 125268225);
}
}
}
