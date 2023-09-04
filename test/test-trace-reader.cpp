// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/binary-reader.h>

namespace quick_lint_js {
namespace {
// clang-format off
constexpr std::array<std::uint8_t, 4+16+8+1> example_packet_header = {
    // CTF magic
    0xc1, 0x1f, 0xfc, 0xc1,

    // quick-lint-js metadata UUID
    0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,  //
    0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,

    // Thread ID
    0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

    // Compression mode
    0x00,
};
// clang-format on

TEST(Test_Trace_Reader, empty_trace_has_packet_header) {
  Trace_Reader reader;
  reader.append_bytes(example_packet_header.data(),
                      example_packet_header.size());

  std::vector<Parsed_Trace_Event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 1);
  EXPECT_EQ(events[0].type, Parsed_Trace_Event_Type::packet_header);
  EXPECT_EQ(events[0].packet_header.thread_id, 0x1234);
}

TEST(Test_Trace_Reader, header_in_two_parts) {
  for (std::size_t first_chunk_size = 1;
       first_chunk_size < example_packet_header.size() - 1;
       ++first_chunk_size) {
    std::size_t second_chunk_size =
        example_packet_header.size() - first_chunk_size;
    SCOPED_TRACE("first_chunk_size=" + std::to_string(first_chunk_size) +
                 " second_chunk_size=" + std::to_string(second_chunk_size));

    Trace_Reader reader;
    reader.append_bytes(example_packet_header.data(), first_chunk_size);
    reader.append_bytes(example_packet_header.data() + first_chunk_size,
                        second_chunk_size);

    std::vector<Parsed_Trace_Event> events = reader.pull_new_events();
    ASSERT_EQ(events.size(), 1);
    EXPECT_EQ(events[0].type, Parsed_Trace_Event_Type::packet_header);
    EXPECT_EQ(events[0].packet_header.thread_id, 0x1234);
  }
}

TEST(Test_Trace_Reader, invalid_magic_reports_error) {
  auto stream = example_packet_header;
  stream[0] = 0xc0;
  stream[3] = 0xc0;
  Trace_Reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<Parsed_Trace_Event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 1);
  EXPECT_EQ(events[0].type, Parsed_Trace_Event_Type::error_invalid_magic);
}

TEST(Test_Trace_Reader, invalid_uuid_reports_error) {
  auto stream = example_packet_header;
  stream[7] = 0xff;
  Trace_Reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<Parsed_Trace_Event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 1);
  EXPECT_EQ(events[0].type, Parsed_Trace_Event_Type::error_invalid_uuid);
}

TEST(Test_Trace_Reader, invalid_compression_mode_reports_error) {
  auto stream = example_packet_header;
  stream[4 + 16 + 8] = 0xfe;

  Trace_Reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<Parsed_Trace_Event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 1);
  EXPECT_EQ(events[0].type,
            Parsed_Trace_Event_Type::error_unsupported_compression_mode);
}

// For most Trace_Reader tests, see test-trace-reader-writer.cpp.
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
