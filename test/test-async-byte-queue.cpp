// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/async-byte-queue.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/thread.h>
#include <vector>

namespace quick_lint_js {
namespace {
TEST(test_async_byte_queue, is_empty_after_construction) {
  async_byte_queue q;

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data, u8"");
}

TEST(test_async_byte_queue, append_and_commit_one_byte) {
  async_byte_queue q;
  q.append_copy(u8'x');
  q.commit();

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data, u8"x");
}

TEST(test_async_byte_queue, append_and_commit_one_two_bytes) {
  async_byte_queue q;
  q.append_copy(u8'x');
  q.append_copy(u8'y');
  q.commit();

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data, u8"xy");
}

TEST(test_async_byte_queue, append_two_bytes_commit_one_byte) {
  async_byte_queue q;
  q.append_copy(u8'x');
  q.commit();
  q.append_copy(u8'y');

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data, u8"x");
}

TEST(test_async_byte_queue, append_byte_after_take) {
  async_byte_queue q;

  q.append_copy(u8'x');
  q.commit();
  string8 taken_data_1 = q.take_committed_string8();

  q.append_copy(u8'y');
  q.commit();
  string8 taken_data_2 = q.take_committed_string8();
  EXPECT_EQ(taken_data_2, u8"y");
}

TEST(test_async_byte_queue, appended_data_is_readable_by_another_thread) {
  constexpr int write_count = 1000;

  async_byte_queue q;
  string8 expected_data;

  std::atomic<bool> writer_done = false;
  thread writer_thread([&]() {
    for (int i = 0; i < write_count; ++i) {
      char8 c = static_cast<char8>(u8'0' + (i % 10));
      q.append_copy(c);
      q.commit();
      expected_data.push_back(c);
    }
    writer_done.store(true);
  });

  string8 taken_data;
  while (!writer_done.load()) {
    taken_data += q.take_committed_string8();
  }
  taken_data += q.take_committed_string8();
  writer_thread.join();

  EXPECT_EQ(taken_data, expected_data);
}

TEST(test_async_byte_queue, append_small_pieces_within_single_chunk) {
  async_byte_queue q;
  ASSERT_LT(4 + 8 + 4, q.default_chunk_size);

  void* piece_0 = q.append(4);
  std::memcpy(piece_0, u8"one ", 4);
  void* piece_1 = q.append(8);
  std::memcpy(piece_1, u8"and two ", 8);
  void* piece_2 = q.append(4);
  std::memcpy(piece_2, u8"thr3", 4);
  q.commit();

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data, u8"one and two thr3");
}

TEST(test_async_byte_queue, append_small_pieces_within_multiple_chunks) {
  async_byte_queue q;

  static constexpr int piece_size = 3;
  string8 expected_data;
  for (async_byte_queue::size_type i = 0; i < q.default_chunk_size * 5;
       i += piece_size) {
    std::array<char8, piece_size> piece;
    std::fill(piece.begin(), piece.end(), u8'a' + (i % 26));
    std::memcpy(q.append(piece.size()), piece.data(), piece.size());
    expected_data += string8_view(piece.data(), piece.size());
  }
  q.commit();

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data, expected_data);
}

TEST(test_async_byte_queue, take_multiple_chunks_then_append) {
  async_byte_queue q;

  void* chunk_0 = q.append(q.default_chunk_size);
  std::memset(chunk_0, u8'a', q.default_chunk_size);
  void* chunk_1 = q.append(q.default_chunk_size);
  std::memset(chunk_1, u8'b', q.default_chunk_size);
  q.commit();
  string8 taken_data_01 = q.take_committed_string8();

  void* data_2 = q.append(10);
  std::memcpy(data_2, u8"helloworld", 10);
  q.commit();
  string8 taken_data_2 = q.take_committed_string8();
  EXPECT_EQ(taken_data_2, u8"helloworld");
}

TEST(test_async_byte_queue, append_multiple_chunks_without_taking) {
  async_byte_queue q;
  q.append(q.default_chunk_size);
  q.append(q.default_chunk_size);

  // This test assumes a leak checker will fail if a memory leak occurs.
}

TEST(test_async_byte_queue, commit_big_write_then_small_writes) {
  async_byte_queue q;
  string8 expected_data;

  char8* data_0 = static_cast<char8*>(q.append(q.default_chunk_size));
  std::fill_n(data_0, q.default_chunk_size, u8'a');
  expected_data += string8_view(data_0, q.default_chunk_size);
  q.commit();

  char8* data_1 = static_cast<char8*>(q.append(2));
  std::fill_n(data_1, 2, u8'b');
  expected_data += string8_view(data_1, 2);
  q.commit();

  char8* data_2 = static_cast<char8*>(q.append(2));
  std::fill_n(data_2, 2, u8'c');
  expected_data += string8_view(data_2, 2);
  q.commit();

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data, expected_data);
}

TEST(test_async_byte_queue, oversized_chunk) {
  async_byte_queue q;
  string8 expected_data;

  char8* data_0 = static_cast<char8*>(q.append(q.default_chunk_size * 3));
  std::fill_n(data_0, q.default_chunk_size * 3, u8'a');
  expected_data += string8_view(data_0, q.default_chunk_size * 3);

  char8* data_1 = static_cast<char8*>(q.append(2));
  std::fill_n(data_1, 2, u8'b');
  expected_data += string8_view(data_1, 2);
  q.commit();

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data, expected_data);
}

TEST(test_async_byte_queue,
     taking_multiple_chunks_keeps_all_chunks_alive_for_finalizer) {
  async_byte_queue q;

  char8* chunk_0 = static_cast<char8*>(q.append(q.default_chunk_size));
  std::fill_n(chunk_0, q.default_chunk_size, u8'a');
  char8* chunk_1 = static_cast<char8*>(q.append(q.default_chunk_size));
  std::fill_n(chunk_1, q.default_chunk_size, u8'b');
  char8* chunk_2 = static_cast<char8*>(q.append(q.default_chunk_size / 4));
  std::fill_n(chunk_2, q.default_chunk_size, u8'c');
  q.commit();

  struct chunk {
    const std::byte* data;
    async_byte_queue::size_type size;
  };
  std::vector<chunk> chunks;
  bool finalize_called = false;
  q.take_committed(
      [&](const std::byte* data, async_byte_queue::size_type size) {
        chunks.push_back(chunk{.data = data, .size = size});
      },
      [&]() {
        ASSERT_EQ(chunks.size(), 3);

        EXPECT_EQ(chunks[0].size, q.default_chunk_size);
        EXPECT_EQ(chunks[0].data, reinterpret_cast<std::byte*>(chunk_0));
        EXPECT_EQ(static_cast<char8>(chunks[0].data[0]), u8'a');

        EXPECT_EQ(chunks[1].size, q.default_chunk_size);
        EXPECT_EQ(chunks[1].data, reinterpret_cast<std::byte*>(chunk_1));
        EXPECT_EQ(static_cast<char8>(chunks[1].data[0]), u8'b');

        EXPECT_EQ(chunks[2].size, q.default_chunk_size / 4);
        EXPECT_EQ(chunks[2].data, reinterpret_cast<std::byte*>(chunk_2));
        EXPECT_EQ(static_cast<char8>(chunks[2].data[0]), u8'c');

        finalize_called = true;
      });
  EXPECT_TRUE(finalize_called);
}

TEST(test_async_byte_queue, append_aligned) {
  async_byte_queue q;
  string8 expected_data;

  char8* data_0 = static_cast<char8*>(q.append(1));
  data_0[0] = u8'a';

  std::uint64_t* data_1 = static_cast<std::uint64_t*>(
      q.append_aligned(sizeof(std::uint64_t) * 2, alignof(std::uint64_t)));
  data_1[0] = 0x1122334455667788ULL;
  data_1[1] = 0x99aabbccddeeff00ULL;

  std::uint64_t* data_2 = static_cast<std::uint64_t*>(
      q.append_aligned(sizeof(std::uint64_t), alignof(std::uint64_t)));
  data_2[0] = 0x1010202030304040ULL;
  EXPECT_EQ(reinterpret_cast<std::uintptr_t>(data_1 + 2),
            reinterpret_cast<std::uintptr_t>(data_2))
      << "data_1 should be in the same chunk as data_2";

  q.commit();

  string8 taken_data = q.take_committed_string8();
  EXPECT_EQ(taken_data.size(), 1 + sizeof(std::uint64_t) * 3);
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
