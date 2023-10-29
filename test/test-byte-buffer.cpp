// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/algorithm.h>
#include <vector>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#endif

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
String8 get_data(const Byte_Buffer_IOVec&);
void assert_no_empty_iovec(const Byte_Buffer_IOVec&);
Byte_Buffer_IOVec make_byte_buffer_iovec_with_chunks(
    Span<const String8_View> chunks);

TEST(Test_Byte_Buffer, empty_byte_buffer_is_empty) {
  Byte_Buffer bb;
  EXPECT_EQ(bb.size(), 0);
  EXPECT_TRUE(bb.empty());
}

TEST(Test_Byte_Buffer, append_small_pieces_within_single_chunk) {
  Byte_Buffer bb;
  ASSERT_LT(4 + 8 + 4, bb.default_chunk_size);

  void* piece_0 = bb.append(4);
  std::memcpy(piece_0, u8"one ", 4);
  void* piece_1 = bb.append(8);
  std::memcpy(piece_1, u8"and two ", 8);
  void* piece_2 = bb.append(4);
  std::memcpy(piece_2, u8"thr3", 4);

  EXPECT_EQ(bb.size(), 4 + 8 + 4);
  EXPECT_FALSE(bb.empty());
  std::vector<Char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(String8_View(data.data(), data.size()), u8"one and two thr3"_sv);
}

TEST(Test_Byte_Buffer, appending_copy_behaves_like_append_with_memcpy) {
  Byte_Buffer bb;

  bb.append_copy(u8"hello"_sv);

  EXPECT_EQ(bb.size(), 5);
  std::vector<Char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(String8_View(data.data(), data.size()), u8"hello"_sv);
}

TEST(Test_Byte_Buffer, append_small_with_callback) {
  Byte_Buffer bb;

  bb.append(4, [](void* piece) -> Byte_Buffer::Size_Type {
    std::memcpy(piece, u8"ab", 2);
    return 2;
  });
  bb.append(4, [](void* piece) -> Byte_Buffer::Size_Type {
    std::memcpy(piece, u8"cdef", 4);
    return 4;
  });

  EXPECT_EQ(bb.size(), 2 + 4);
  std::vector<Char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(String8_View(data.data(), data.size()), u8"abcdef"_sv);
}

TEST(Test_Byte_Buffer,
     reserving_space_but_writing_nothing_preserves_size_of_empty_buffer) {
  Byte_Buffer bb;

  bb.append(4, [](void*) -> Byte_Buffer::Size_Type { return 0; });

  EXPECT_EQ(bb.size(), 0);
  EXPECT_TRUE(bb.empty());

  bb.append(Byte_Buffer::default_chunk_size * 2,
            [](void*) -> Byte_Buffer::Size_Type { return 0; });

  EXPECT_EQ(bb.size(), 0);
  EXPECT_TRUE(bb.empty());
}

TEST(Test_Byte_Buffer, append_small_pieces_within_multiple_chunks) {
  Byte_Buffer bb;

  static constexpr int piece_size = 3;
  std::vector<Char8> expected_data;
  for (Byte_Buffer::Size_Type i = 0; i < bb.default_chunk_size * 5;
       i += piece_size) {
    std::array<Char8, piece_size> piece;
    fill(piece, u8'a' + (i % 26));
    std::memcpy(bb.append(piece.size()), piece.data(), piece.size());
    expected_data.insert(expected_data.end(), piece.begin(), piece.end());
  }

  EXPECT_EQ(bb.size(), expected_data.size());
  std::vector<Char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, expected_data);
}

TEST(Test_Byte_Buffer, append_piece_larger_than_default_chunk_size) {
  Byte_Buffer bb;

  std::vector<Char8> expected_data;
  std::vector<Char8> piece;

  piece.resize(bb.default_chunk_size * 2);
  fill(piece, u8'a');
  std::memcpy(bb.append(piece.size()), piece.data(), piece.size());
  expected_data.insert(expected_data.end(), piece.begin(), piece.end());

  piece.resize(bb.default_chunk_size * 3 / 2);
  fill(piece, u8'b');
  std::memcpy(bb.append(piece.size()), piece.data(), piece.size());
  expected_data.insert(expected_data.end(), piece.begin(), piece.end());

  EXPECT_EQ(bb.size(), expected_data.size());
  std::vector<Char8> data(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, expected_data);
}

TEST(Test_Byte_Buffer, append_integer) {
  Byte_Buffer bb;

  bb.append_decimal_integer(std::size_t{42});
  bb.append_decimal_integer(int{0});

  EXPECT_EQ(bb.size(), 3);
  String8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"420");
}

TEST(Test_Byte_Buffer, prepend_copy_on_empty_behaves_like_append_copy) {
  Byte_Buffer bb;

  bb.prepend_copy(u8"hello world"_sv);

  EXPECT_EQ(bb.size(), 11);
  String8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"hello world");
}

TEST(Test_Byte_Buffer, prepend_copy_on_non_empty) {
  Byte_Buffer bb;

  bb.append_copy(u8" w"_sv);
  bb.append_copy(u8"orld"_sv);
  bb.prepend_copy(u8"hello"_sv);

  EXPECT_EQ(bb.size(), 11);
  String8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"hello world");
}

TEST(Test_Byte_Buffer, prepend_copy_multiple_times) {
  Byte_Buffer bb;

  bb.prepend_copy(u8"rld"_sv);
  bb.prepend_copy(u8" wo"_sv);
  bb.prepend_copy(u8"lo"_sv);
  bb.prepend_copy(u8"hel"_sv);

  EXPECT_EQ(bb.size(), 11);
  String8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"hello world");
}

TEST(Test_Byte_Buffer, append_copy_after_prepend_copy) {
  Byte_Buffer bb;

  bb.append_copy(u8" wor"_sv);
  bb.prepend_copy(u8"hello"_sv);
  bb.append_copy(u8"ld"_sv);

  EXPECT_EQ(bb.size(), 11);
  String8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"hello world");
}

TEST(Test_Byte_Buffer, clear_after_small_appends) {
  Byte_Buffer bb;

  bb.append_copy(u8"hello "_sv);
  bb.append_copy(u8"world"_sv);
  bb.clear();

  EXPECT_EQ(bb.size(), 0);
}

TEST(Test_Byte_Buffer, clear_after_big_appends) {
  Byte_Buffer bb;

  bb.append_copy(String8(Byte_Buffer::default_chunk_size * 3, 'x'));
  bb.append_copy(String8(Byte_Buffer::default_chunk_size * 3, 'y'));
  bb.append_copy(String8(Byte_Buffer::default_chunk_size * 3, 'z'));
  bb.clear();

  EXPECT_EQ(bb.size(), 0);
}

TEST(Test_Byte_Buffer, clear_then_append) {
  Byte_Buffer bb;

  bb.append_copy(u8"hello "_sv);
  bb.clear();
  bb.append_copy(u8"world"_sv);

  String8 data;
  data.resize(bb.size());
  bb.copy_to(data.data());
  EXPECT_EQ(data, u8"world");
}

TEST(Test_Byte_Buffer, append_byte_buffer_to_byte_buffer_iovec) {
  // Create three chunks for bb_1.
  Byte_Buffer bb_1;
  String8 bb_1_expected;
  bb_1.append_copy(u8"hello"_sv);
  bb_1_expected.append(u8"hello");
  bb_1.append_copy(String8(Byte_Buffer::default_chunk_size * 2, '-'));
  bb_1_expected.append(String8(Byte_Buffer::default_chunk_size * 2, '-'));
  bb_1.append_copy(u8"world"_sv);
  bb_1_expected.append(u8"world");

  // Create three chunks for bb_2.
  Byte_Buffer bb_2;
  String8 bb_2_expected;
  bb_2.append_copy(u8"HELLO"_sv);
  bb_2_expected.append(u8"HELLO");
  bb_2.append_copy(String8(Byte_Buffer::default_chunk_size * 2, '_'));
  bb_2_expected.append(String8(Byte_Buffer::default_chunk_size * 2, '_'));
  bb_2.append_copy(u8"WORLD"_sv);
  bb_2_expected.append(u8"WORLD");

  Byte_Buffer_IOVec iov;
  iov.append(std::move(bb_1));
  String8 iov_expected = bb_1_expected;
  iov.append(std::move(bb_2));
  iov_expected.append(bb_2_expected);

  EXPECT_EQ(get_data(iov), iov_expected);
}

TEST(Test_Byte_Buffer, append_empty_byte_buffer_to_byte_buffer_iovec) {
  Byte_Buffer bb_1;
  bb_1.append_copy(u8"hello"_sv);
  Byte_Buffer_IOVec iov;
  iov.append(std::move(bb_1));

  Byte_Buffer bb_2;
  iov.append(std::move(bb_2));

  EXPECT_EQ(get_data(iov), u8"hello");
}

TEST(Test_Byte_Buffer,
     append_byte_buffer_to_indirectly_empty_byte_buffer_iovec) {
  Byte_Buffer bb_1;
  Byte_Buffer_IOVec iov;
  iov.append(std::move(bb_1));

  Byte_Buffer bb_2;
  bb_2.append_copy(u8"hello"_sv);
  iov.append(std::move(bb_2));

  EXPECT_EQ(get_data(iov), u8"hello");
}

TEST(Test_Byte_Buffer, append_byte_buffer_to_empty_byte_buffer_iovec) {
  Byte_Buffer_IOVec iov;

  Byte_Buffer bb;
  bb.append_copy(u8"hello"_sv);
  iov.append(std::move(bb));

  EXPECT_EQ(get_data(iov), u8"hello");
}

TEST(Test_Byte_Buffer, append_byte_buffer_to_exhausted_byte_buffer_iovec) {
  Byte_Buffer_IOVec iov =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8"world"_sv,
      }));
  iov.remove_front(u8"helloworld"_sv.size());

  Byte_Buffer bb;
  bb.append_copy(u8"hiya"_sv);
  iov.append(std::move(bb));

  EXPECT_EQ(get_data(iov), u8"hiya");
}

TEST(Test_Byte_Buffer, iovec) {
  Byte_Buffer bb;

  String8 expected_data;

  String8 small_data = u8"hello world";
  bb.append_copy(String8_View(small_data));
  expected_data.append(small_data);
  bb.append_copy(String8_View(small_data));
  expected_data.append(small_data);

  String8 big_data(bb.default_chunk_size * 2, u8'A');
  bb.append_copy(String8_View(big_data));
  expected_data.append(big_data);

  bb.append_copy(String8_View(small_data));
  expected_data.append(small_data);

  Byte_Buffer_IOVec iovec;
  iovec.append(std::move(bb));
  EXPECT_EQ(get_data(iovec), expected_data);
}

TEST(Test_Byte_Buffer, empty_byte_buffer_to_iovec_has_no_chunks) {
  Byte_Buffer bb;
  Byte_Buffer_IOVec iovec;
  iovec.append(std::move(bb));
  EXPECT_EQ(iovec.iovec_count(), 0);
}

TEST(Test_Byte_Buffer,
     byte_buffer_with_huge_append_to_iovec_has_no_empty_chunks) {
  // Byte_Buffer used to have a bug. If the first reservation was larger than
  // default_chunk_size, two chunks were created: an empty first chunk, and a
  // large second chunk. The first (empty) chunk stuck around for the conversion
  // to Byte_Buffer_IOVec.
  Byte_Buffer bb;
  bb.append_copy(String8(Byte_Buffer::default_chunk_size * 3, 'x'));
  Byte_Buffer_IOVec iovec;
  iovec.append(std::move(bb));
  assert_no_empty_iovec(iovec);
}

TEST(Test_Byte_Buffer,
     byte_buffer_with_undone_append_to_iovec_has_no_empty_chunks) {
  // Byte_Buffer used to have a bug. If a reservation was cancelled, an empty
  // chunk was created. The empty chunk stuck around for the conversion to
  // Byte_Buffer_IOVec.
  Byte_Buffer bb;
  bb.append(1, []([[maybe_unused]] void* data) -> std::size_t { return 0; });
  Byte_Buffer_IOVec iovec;
  iovec.append(std::move(bb));
  assert_no_empty_iovec(iovec);
}

TEST(Test_Byte_Buffer,
     byte_buffer_with_large_undone_append_to_iovec_has_no_empty_chunks) {
  // Byte_Buffer used to have a bug. If the first reservation was larger than
  // default_chunk_size, two chunks were created: an empty first chunk, and a
  // large second chunk. The first (empty) chunk stuck around for the conversion
  // to Byte_Buffer_IOVec, and the second (also empty) chunk also stuck around.
  Byte_Buffer bb;
  bb.append(Byte_Buffer::default_chunk_size * 3,
            []([[maybe_unused]] void* data) -> std::size_t { return 0; });
  Byte_Buffer_IOVec iovec;
  iovec.append(std::move(bb));
  assert_no_empty_iovec(iovec);
}

TEST(Test_Byte_Buffer_Iovec, remove_front_entire_single_chunk) {
  Byte_Buffer_IOVec bb =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8" "_sv,
          u8"world"_sv,
      }));
  bb.remove_front(u8"hello"_sv.size());
  EXPECT_EQ(get_data(bb), u8" world");
}

TEST(Test_Byte_Buffer_Iovec, remove_front_entire_multiple_chunks) {
  Byte_Buffer_IOVec bb =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8"beautiful"_sv,
          u8"world"_sv,
      }));
  bb.remove_front(u8"hello"_sv.size() + u8"beautiful"_sv.size());
  EXPECT_EQ(get_data(bb), u8"world");
}

TEST(Test_Byte_Buffer_Iovec, remove_front_all_chunks) {
  Byte_Buffer_IOVec bb =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8" "_sv,
          u8"world"_sv,
      }));
  bb.remove_front(u8"hello"_sv.size() + u8" "_sv.size() + u8"world"_sv.size());
  EXPECT_EQ(get_data(bb), u8"");
}

TEST(Test_Byte_Buffer_Iovec, remove_part_of_first_chunk) {
  Byte_Buffer_IOVec bb =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8" "_sv,
          u8"world"_sv,
      }));
  bb.remove_front(u8"hel"_sv.size());
  EXPECT_EQ(get_data(bb), u8"lo world");
}

TEST(Test_Byte_Buffer_Iovec, remove_parts_of_first_chunk) {
  Byte_Buffer_IOVec bb =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8" "_sv,
          u8"world"_sv,
      }));
  bb.remove_front(1);
  bb.remove_front(1);
  bb.remove_front(1);
  EXPECT_EQ(get_data(bb), u8"lo world");
}

TEST(Test_Byte_Buffer_Iovec, remove_first_chunk_and_part_of_second_chunk) {
  Byte_Buffer_IOVec bb =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8"beautiful"_sv,
          u8"world"_sv,
      }));
  bb.remove_front(u8"hello"_sv.size() + u8"beauti"_sv.size());
  EXPECT_EQ(get_data(bb), u8"fulworld");
}

TEST(Test_Byte_Buffer_Iovec, remove_front_all_chunks_byte_by_byte) {
  Byte_Buffer_IOVec bb =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8"beautiful"_sv,
          u8"world"_sv,
      }));
  for (std::size_t i = 0;
       i < u8"hello"_sv.size() + u8"beautiful"_sv.size() + u8"world"_sv.size();
       ++i) {
    bb.remove_front(1);
  }
  EXPECT_EQ(get_data(bb), u8"");
}

TEST(Test_Byte_Buffer_Iovec, moving_makes_original_empty) {
  Byte_Buffer_IOVec bb_1 =
      make_byte_buffer_iovec_with_chunks(Span<const String8_View>({
          u8"hello"_sv,
          u8"beautiful"_sv,
          u8"world"_sv,
      }));

  Byte_Buffer_IOVec bb_2(std::move(bb_1));
  EXPECT_EQ(bb_1.iovec_count(), 0);
}

String8 get_data(const Byte_Buffer_IOVec& bb) {
  String8 data;
  for (int i = 0; i < bb.iovec_count(); ++i) {
    const Byte_Buffer_Chunk& chunk = bb.iovec()[i];
#if QLJS_HAVE_WRITEV
    data.append(reinterpret_cast<const Char8*>(chunk.iov_base), chunk.iov_len);
#else
    data.append(reinterpret_cast<const Char8*>(chunk.data), chunk.size);
#endif
  }
  return data;
}

Byte_Buffer_IOVec make_byte_buffer_iovec_with_chunks(
    Span<const String8_View> chunks) {
  Byte_Buffer_IOVec iovec;
  for (String8_View chunk : chunks) {
    Byte_Buffer chunk_byte_buffer;
    chunk_byte_buffer.append_copy(chunk);
    iovec.append(std::move(chunk_byte_buffer));
  }
  return iovec;
}

void assert_no_empty_iovec(const Byte_Buffer_IOVec& iovec) {
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
