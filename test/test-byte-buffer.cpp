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

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(test_byte_buffer, empty_byte_buffer_is_empty) {
  byte_buffer bb;
  EXPECT_EQ(bb.size(), 0);
  EXPECT_TRUE(bb.empty());
}

TEST(test_byte_buffer, append_small_pieces_within_single_chunk) {
  byte_buffer bb;
  ASSERT_LT(4 + 8 + 4, bb.default_chunk_size);

  void* piece_0 = bb.append(4);
  std::memcpy(piece_0, u8"one ", 4);
  void* piece_1 = bb.append(8);
  std::memcpy(piece_1, u8"and two ", 8);
  void* piece_2 = bb.append(4);
  std::memcpy(piece_2, u8"thr3", 4);

  EXPECT_EQ(bb.size(), 4 + 8 + 4);
  EXPECT_FALSE(bb.empty());
  std::vector<char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(string8_view(data.data(), data.size()), u8"one and two thr3");
}

TEST(test_byte_buffer, appending_copy_behaves_like_append_with_memcpy) {
  byte_buffer bb;

  bb.append_copy(string8_view(u8"hello"));

  EXPECT_EQ(bb.size(), 5);
  std::vector<char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(string8_view(data.data(), data.size()), u8"hello");
}

TEST(test_byte_buffer, append_small_with_callback) {
  byte_buffer bb;

  bb.append(4, [](void* piece) -> byte_buffer::size_type {
    std::memcpy(piece, u8"ab", 2);
    return 2;
  });
  bb.append(4, [](void* piece) -> byte_buffer::size_type {
    std::memcpy(piece, u8"cdef", 4);
    return 4;
  });

  EXPECT_EQ(bb.size(), 2 + 4);
  std::vector<char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(string8_view(data.data(), data.size()), u8"abcdef");
}

TEST(test_byte_buffer,
     reserving_space_but_writing_nothing_preserves_size_of_empty_buffer) {
  byte_buffer bb;

  bb.append(4, [](void*) -> byte_buffer::size_type { return 0; });

  EXPECT_EQ(bb.size(), 0);
  EXPECT_TRUE(bb.empty());

  bb.append(byte_buffer::default_chunk_size * 2,
            [](void*) -> byte_buffer::size_type { return 0; });

  EXPECT_EQ(bb.size(), 0);
  EXPECT_TRUE(bb.empty());
}

TEST(test_byte_buffer, append_small_pieces_within_multiple_chunks) {
  byte_buffer bb;

  static constexpr int piece_size = 3;
  std::vector<char8> expected_data;
  for (byte_buffer::size_type i = 0; i < bb.default_chunk_size * 5;
       i += piece_size) {
    std::array<char8, piece_size> piece;
    std::fill(piece.begin(), piece.end(), u8'a' + (i % 26));
    std::memcpy(bb.append(piece.size()), piece.data(), piece.size());
    expected_data.insert(expected_data.end(), piece.begin(), piece.end());
  }

  EXPECT_EQ(bb.size(), expected_data.size());
  std::vector<char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, expected_data);
}

TEST(test_byte_buffer, append_piece_larger_than_default_chunk_size) {
  byte_buffer bb;

  std::vector<char8> expected_data;
  std::vector<char8> piece;

  piece.resize(bb.default_chunk_size * 2);
  std::fill(piece.begin(), piece.end(), u8'a');
  std::memcpy(bb.append(piece.size()), piece.data(), piece.size());
  expected_data.insert(expected_data.end(), piece.begin(), piece.end());

  piece.resize(bb.default_chunk_size * 3 / 2);
  std::fill(piece.begin(), piece.end(), u8'b');
  std::memcpy(bb.append(piece.size()), piece.data(), piece.size());
  expected_data.insert(expected_data.end(), piece.begin(), piece.end());

  EXPECT_EQ(bb.size(), expected_data.size());
  std::vector<char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, expected_data);
}

TEST(test_byte_buffer, append_integer) {
  byte_buffer bb;

  bb.append_decimal_integer(std::size_t{42});
  bb.append_decimal_integer(int{0});

  EXPECT_EQ(bb.size(), 3);
  string8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"420");
}
}
}
