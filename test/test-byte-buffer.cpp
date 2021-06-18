// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <vector>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#endif

namespace quick_lint_js {
namespace {
string8 get_data(const byte_buffer_iovec&);
byte_buffer_chunk make_chunk(string8_view);
void assert_no_empty_iovec(const byte_buffer_iovec&);

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

TEST(test_byte_buffer, prepend_copy_on_empty_behaves_like_append_copy) {
  byte_buffer bb;

  bb.prepend_copy(u8"hello world");

  EXPECT_EQ(bb.size(), 11);
  string8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"hello world");
}

TEST(test_byte_buffer, prepend_copy_on_non_empty) {
  byte_buffer bb;

  bb.append_copy(u8" w");
  bb.append_copy(u8"orld");
  bb.prepend_copy(u8"hello");

  EXPECT_EQ(bb.size(), 11);
  string8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"hello world");
}

TEST(test_byte_buffer, prepend_copy_multiple_times) {
  byte_buffer bb;

  bb.prepend_copy(u8"rld");
  bb.prepend_copy(u8" wo");
  bb.prepend_copy(u8"lo");
  bb.prepend_copy(u8"hel");

  EXPECT_EQ(bb.size(), 11);
  string8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"hello world");
}

TEST(test_byte_buffer, append_copy_after_prepend_copy) {
  byte_buffer bb;

  bb.append_copy(u8" wor");
  bb.prepend_copy(u8"hello");
  bb.append_copy(u8"ld");

  EXPECT_EQ(bb.size(), 11);
  string8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"hello world");
}

TEST(test_byte_buffer, clear_after_small_appends) {
  byte_buffer bb;

  bb.append_copy(u8"hello ");
  bb.append_copy(u8"world");
  bb.clear();

  EXPECT_EQ(bb.size(), 0);
}

TEST(test_byte_buffer, clear_after_big_appends) {
  byte_buffer bb;

  bb.append_copy(string8(byte_buffer::default_chunk_size * 3, 'x'));
  bb.append_copy(string8(byte_buffer::default_chunk_size * 3, 'y'));
  bb.append_copy(string8(byte_buffer::default_chunk_size * 3, 'z'));
  bb.clear();

  EXPECT_EQ(bb.size(), 0);
}

TEST(test_byte_buffer, clear_then_append) {
  byte_buffer bb;

  bb.append_copy(u8"hello ");
  bb.clear();
  bb.append_copy(u8"world");

  string8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"world");
}

TEST(test_byte_buffer, append_byte_buffer_to_byte_buffer_iovec) {
  // Create three chunks for bb_1.
  byte_buffer bb_1;
  string8 bb_1_expected;
  bb_1.append_copy(u8"hello");
  bb_1_expected.append(u8"hello");
  bb_1.append_copy(string8(byte_buffer::default_chunk_size * 2, '-'));
  bb_1_expected.append(string8(byte_buffer::default_chunk_size * 2, '-'));
  bb_1.append_copy(u8"world");
  bb_1_expected.append(u8"world");

  // Create three chunks for bb_2.
  byte_buffer bb_2;
  string8 bb_2_expected;
  bb_2.append_copy(u8"HELLO");
  bb_2_expected.append(u8"HELLO");
  bb_2.append_copy(string8(byte_buffer::default_chunk_size * 2, '_'));
  bb_2_expected.append(string8(byte_buffer::default_chunk_size * 2, '_'));
  bb_2.append_copy(u8"WORLD");
  bb_2_expected.append(u8"WORLD");

  byte_buffer_iovec iov = std::move(bb_1).to_iovec();
  string8 iov_expected = bb_1_expected;
  iov.append(std::move(bb_2));
  iov_expected.append(bb_2_expected);

  EXPECT_EQ(get_data(iov), iov_expected);
}

TEST(test_byte_buffer, append_empty_byte_buffer_to_byte_buffer_iovec) {
  byte_buffer bb_1;
  bb_1.append_copy(u8"hello");
  byte_buffer_iovec iov = std::move(bb_1).to_iovec();

  byte_buffer bb_2;
  iov.append(std::move(bb_2));

  EXPECT_EQ(get_data(iov), u8"hello");
}

TEST(test_byte_buffer,
     append_byte_buffer_to_indirectly_empty_byte_buffer_iovec) {
  byte_buffer bb_1;
  byte_buffer_iovec iov = std::move(bb_1).to_iovec();

  byte_buffer bb_2;
  bb_2.append_copy(u8"hello");
  iov.append(std::move(bb_2));

  EXPECT_EQ(get_data(iov), u8"hello");
}

TEST(test_byte_buffer, append_byte_buffer_to_empty_byte_buffer_iovec) {
  byte_buffer_iovec iov(std::vector<byte_buffer_chunk>{});

  byte_buffer bb;
  bb.append_copy(u8"hello");
  iov.append(std::move(bb));

  EXPECT_EQ(get_data(iov), u8"hello");
}

TEST(test_byte_buffer, append_byte_buffer_to_exhausted_byte_buffer_iovec) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec iov(std::move(chunks));
  iov.remove_front(strlen(u8"helloworld"));

  byte_buffer bb;
  bb.append_copy(u8"hiya");
  iov.append(std::move(bb));

  EXPECT_EQ(get_data(iov), u8"hiya");
}

TEST(test_byte_buffer, iovec) {
  byte_buffer bb;

  string8 expected_data;

  string8 small_data = u8"hello world";
  bb.append_copy(small_data);
  expected_data.append(small_data);
  bb.append_copy(small_data);
  expected_data.append(small_data);

  string8 big_data(bb.default_chunk_size * 2, u8'A');
  bb.append_copy(big_data);
  expected_data.append(big_data);

  bb.append_copy(small_data);
  expected_data.append(small_data);

  byte_buffer_iovec iovec = std::move(bb).to_iovec();
  EXPECT_EQ(get_data(iovec), expected_data);
}

TEST(test_byte_buffer, empty_byte_buffer_to_iovec_has_no_chunks) {
  byte_buffer bb;
  byte_buffer_iovec iovec = std::move(bb).to_iovec();
  EXPECT_EQ(iovec.iovec_count(), 0);
}

TEST(test_byte_buffer,
     byte_buffer_with_huge_append_to_iovec_has_no_empty_chunks) {
  // byte_buffer used to have a bug. If the first reservation was larger than
  // default_chunk_size, two chunks were created: an empty first chunk, and a
  // large second chunk. The first (empty) chunk stuck around for the conversion
  // to byte_buffer_iovec.
  byte_buffer bb;
  bb.append_copy(string8(byte_buffer::default_chunk_size * 3, 'x'));
  byte_buffer_iovec iovec = std::move(bb).to_iovec();
  assert_no_empty_iovec(iovec);
}

TEST(test_byte_buffer,
     byte_buffer_with_undone_append_to_iovec_has_no_empty_chunks) {
  // byte_buffer used to have a bug. If a reservation was cancelled, an empty
  // chunk was created. The empty chunk stuck around for the conversion to
  // byte_buffer_iovec.
  byte_buffer bb;
  bb.append(1, []([[maybe_unused]] void* data) -> std::size_t { return 0; });
  byte_buffer_iovec iovec = std::move(bb).to_iovec();
  assert_no_empty_iovec(iovec);
}

TEST(test_byte_buffer,
     byte_buffer_with_large_undone_append_to_iovec_has_no_empty_chunks) {
  // byte_buffer used to have a bug. If the first reservation was larger than
  // default_chunk_size, two chunks were created: an empty first chunk, and a
  // large second chunk. The first (empty) chunk stuck around for the conversion
  // to byte_buffer_iovec, and the second (also empty) chunk also stuck around.
  byte_buffer bb;
  bb.append(byte_buffer::default_chunk_size * 3,
            []([[maybe_unused]] void* data) -> std::size_t { return 0; });
  byte_buffer_iovec iovec = std::move(bb).to_iovec();
  assert_no_empty_iovec(iovec);
}

TEST(test_byte_buffer_iovec, remove_front_entire_single_chunk) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8" "),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec bb(std::move(chunks));
  bb.remove_front(strlen(u8"hello"));
  EXPECT_EQ(get_data(bb), u8" world");
}

TEST(test_byte_buffer_iovec, remove_front_entire_multiple_chunks) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8"beautiful"),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec bb(std::move(chunks));
  bb.remove_front(strlen(u8"hello") + strlen(u8"beautiful"));
  EXPECT_EQ(get_data(bb), u8"world");
}

TEST(test_byte_buffer_iovec, remove_front_all_chunks) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8" "),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec bb(std::move(chunks));
  bb.remove_front(strlen(u8"hello") + strlen(u8" ") + strlen(u8"world"));
  EXPECT_EQ(get_data(bb), u8"");
}

TEST(test_byte_buffer_iovec, remove_part_of_first_chunk) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8" "),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec bb(std::move(chunks));
  bb.remove_front(strlen(u8"hel"));
  EXPECT_EQ(get_data(bb), u8"lo world");
}

TEST(test_byte_buffer_iovec, remove_parts_of_first_chunk) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8" "),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec bb(std::move(chunks));
  bb.remove_front(1);
  bb.remove_front(1);
  bb.remove_front(1);
  EXPECT_EQ(get_data(bb), u8"lo world");
}

TEST(test_byte_buffer_iovec, remove_first_chunk_and_part_of_second_chunk) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8"beautiful"),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec bb(std::move(chunks));
  bb.remove_front(strlen(u8"hello") + strlen(u8"beauti"));
  EXPECT_EQ(get_data(bb), u8"fulworld");
}

TEST(test_byte_buffer_iovec, remove_front_all_chunks_byte_by_byte) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8"beautiful"),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec bb(std::move(chunks));
  for (std::size_t i = 0;
       i < strlen(u8"hello") + strlen(u8"beautiful") + strlen(u8"world"); ++i) {
    bb.remove_front(1);
  }
  EXPECT_EQ(get_data(bb), u8"");
}

TEST(test_byte_buffer_iovec, moving_makes_original_empty) {
  std::vector<byte_buffer_chunk> chunks = {
      make_chunk(u8"hello"),
      make_chunk(u8"beautiful"),
      make_chunk(u8"world"),
  };
  byte_buffer_iovec bb_1(std::move(chunks));

  byte_buffer_iovec bb_2(std::move(bb_1));
  EXPECT_EQ(bb_1.iovec_count(), 0);
}

string8 get_data(const byte_buffer_iovec& bb) {
  string8 data;
  for (int i = 0; i < bb.iovec_count(); ++i) {
    const byte_buffer_chunk& chunk = bb.iovec()[i];
#if QLJS_HAVE_WRITEV
    data.append(reinterpret_cast<const char8*>(chunk.iov_base), chunk.iov_len);
#else
    data.append(reinterpret_cast<const char8*>(chunk.data), chunk.size);
#endif
  }
  return data;
}

byte_buffer_chunk make_chunk(string8_view data) {
  char8* chunk_data = new char8[data.size()];
  std::copy_n(data.data(), data.size(), chunk_data);
#if QLJS_HAVE_WRITEV
  return byte_buffer_chunk{.iov_base = chunk_data, .iov_len = data.size()};
#else
  return byte_buffer_chunk{.data = reinterpret_cast<std::byte*>(chunk_data),
                           .size = data.size()};
#endif
}

void assert_no_empty_iovec(const byte_buffer_iovec& iovec) {
  for (int i = 0; i < iovec.iovec_count(); ++i) {
#if QLJS_HAVE_WRITEV
    EXPECT_NE(iovec.iovec()[i].iov_len, 0);
#else
    EXPECT_NE(iovec.iovec()[i].size, 0);
#endif
  }
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
