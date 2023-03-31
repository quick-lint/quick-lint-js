// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/binary-reader.h>

using ::testing::ElementsAreArray;
using namespace std::literals::string_view_literals;

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

TEST(test_trace_reader, empty_trace_has_packet_header) {
  trace_reader reader;
  reader.append_bytes(example_packet_header.data(),
                      example_packet_header.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 1);
  EXPECT_EQ(events[0].type, parsed_trace_event_type::packet_header);
  EXPECT_EQ(events[0].packet_header.thread_id, 0x1234);
}

TEST(test_trace_reader, header_in_two_parts) {
  for (std::size_t first_chunk_size = 1;
       first_chunk_size < example_packet_header.size() - 1;
       ++first_chunk_size) {
    std::size_t second_chunk_size =
        example_packet_header.size() - first_chunk_size;
    SCOPED_TRACE("first_chunk_size=" + std::to_string(first_chunk_size) +
                 " second_chunk_size=" + std::to_string(second_chunk_size));

    trace_reader reader;
    reader.append_bytes(example_packet_header.data(), first_chunk_size);
    reader.append_bytes(example_packet_header.data() + first_chunk_size,
                        second_chunk_size);

    std::vector<parsed_trace_event> events = reader.pull_new_events();
    ASSERT_EQ(events.size(), 1);
    EXPECT_EQ(events[0].type, parsed_trace_event_type::packet_header);
    EXPECT_EQ(events[0].packet_header.thread_id, 0x1234);
  }
}

TEST(test_trace_reader, invalid_magic_reports_error) {
  auto stream = example_packet_header;
  stream[0] = 0xc0;
  stream[3] = 0xc0;
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 1);
  EXPECT_EQ(events[0].type, parsed_trace_event_type::error_invalid_magic);
}

TEST(test_trace_reader, invalid_uuid_reports_error) {
  auto stream = example_packet_header;
  stream[7] = 0xff;
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 1);
  EXPECT_EQ(events[0].type, parsed_trace_event_type::error_invalid_uuid);
}

TEST(test_trace_reader, invalid_compression_mode_reports_error) {
  auto stream = example_packet_header;
  stream[4 + 16 + 8] = 0xfe;

  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 1);
  EXPECT_EQ(events[0].type,
            parsed_trace_event_type::error_unsupported_compression_mode);
}

TEST(test_trace_reader, init_event) {
  auto stream = concat(example_packet_header,
                       make_array_explicit<std::uint8_t>(
                           // Timestamp
                           0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Event ID
                           0x01,

                           // Version
                           u8'1', u8'.', u8'0', u8'.', u8'0', u8'\0'));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and init event";
  EXPECT_EQ(events[1].type, parsed_trace_event_type::init_event);
  EXPECT_EQ(events[1].init_event.timestamp, 0x5678);
  EXPECT_EQ(events[1].init_event.version, u8"1.0.0"_sv);
}

TEST(test_trace_reader, vscode_document_opened_event) {
  auto stream =
      concat(example_packet_header,
             make_array_explicit<std::uint8_t>(
                 // Timestamp
                 0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                 // Event ID
                 0x02,

                 // Document ID
                 0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                 // URI
                 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                 't', 0, 'e', 0, 's', 0, 't', 0, '.', 0, 'j', 0, 's', 0,

                 // Language ID
                 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                 'j', 0, 's', 0,

                 // Content
                 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                 'h', 0, 'i', 0));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and vscode event";
  EXPECT_EQ(events[1].type,
            parsed_trace_event_type::vscode_document_opened_event);
  parsed_vscode_document_opened_event& event =
      events[1].vscode_document_opened_event;
  EXPECT_EQ(event.timestamp, 0x5678);
  EXPECT_EQ(event.document_id, 0x1234);
  EXPECT_EQ(event.uri, u"test.js"sv);
  EXPECT_EQ(event.language_id, u"js"sv);
  EXPECT_EQ(event.content, u"hi"sv);
}

TEST(test_trace_reader, vscode_document_closed_event) {
  auto stream =
      concat(example_packet_header,
             make_array_explicit<std::uint8_t>(
                 // Timestamp
                 0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                 // Event ID
                 0x03,

                 // Document ID
                 0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                 // URI
                 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                 't', 0, 'e', 0, 's', 0, 't', 0, '.', 0, 'j', 0, 's', 0,

                 // Language ID
                 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                 'j', 0, 's', 0));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and vscode event";
  EXPECT_EQ(events[1].type,
            parsed_trace_event_type::vscode_document_closed_event);
  parsed_vscode_document_closed_event& event =
      events[1].vscode_document_closed_event;
  EXPECT_EQ(event.timestamp, 0x5678);
  EXPECT_EQ(event.document_id, 0x1234);
  EXPECT_EQ(event.uri, u"test.js"sv);
  EXPECT_EQ(event.language_id, u"js"sv);
}

TEST(test_trace_reader, vscode_document_changed_event) {
  auto stream = concat(
      example_packet_header,
      make_array_explicit<std::uint8_t>(
          // Timestamp
          0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

          // Event ID
          0x04,

          // Document ID
          0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

          // Change count
          0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

          // Change 0 range
          0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Start line
          0x22, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Start character
          0x33, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // End line
          0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // End character
          // Change 0 range offset
          0x55, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          // Change 0 range length
          0x66, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          // Change 0 text
          0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
          'h', 0, 'i', 0,

          // Change 1 range
          0xaa, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Start line
          0xbb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Start character
          0xcc, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // End line
          0xdd, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // End character
          // Change 1 range offset
          0xee, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          // Change 1 range length
          0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          // Change 1 text
          0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
          'b', 0, 'y', 0, 'e', 0));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and vscode event";
  EXPECT_EQ(events[1].type,
            parsed_trace_event_type::vscode_document_changed_event);
  parsed_vscode_document_changed_event& event =
      events[1].vscode_document_changed_event;
  EXPECT_EQ(event.timestamp, 0x5678);
  EXPECT_EQ(event.document_id, 0x1234);
  EXPECT_THAT(event.changes, ElementsAreArray({
                                 parsed_vscode_document_change{
                                     .range =
                                         {
                                             .start =
                                                 {
                                                     .line = 0x11,
                                                     .character = 0x22,
                                                 },
                                             .end =
                                                 {
                                                     .line = 0x33,
                                                     .character = 0x44,
                                                 },
                                         },
                                     .range_offset = 0x55,
                                     .range_length = 0x66,
                                     .text = u"hi",
                                 },
                                 parsed_vscode_document_change{
                                     .range =
                                         {
                                             .start =
                                                 {
                                                     .line = 0xaa,
                                                     .character = 0xbb,
                                                 },
                                             .end =
                                                 {
                                                     .line = 0xcc,
                                                     .character = 0xdd,
                                                 },
                                         },
                                     .range_offset = 0xee,
                                     .range_length = 0xff,
                                     .text = u"bye",
                                 },
                             }));
}

TEST(test_trace_reader, vscode_document_sync_event) {
  auto stream =
      concat(example_packet_header,
             make_array_explicit<std::uint8_t>(
                 // Timestamp
                 0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                 // Event ID
                 0x05,

                 // Document ID
                 0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                 // URI
                 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                 't', 0, 'e', 0, 's', 0, 't', 0, '.', 0, 'j', 0, 's', 0,

                 // Language ID
                 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                 'j', 0, 's', 0,

                 // Content
                 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                 'h', 0, 'i', 0));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and vscode event";
  EXPECT_EQ(events[1].type,
            parsed_trace_event_type::vscode_document_sync_event);
  parsed_vscode_document_sync_event& event =
      events[1].vscode_document_sync_event;
  EXPECT_EQ(event.timestamp, 0x5678);
  EXPECT_EQ(event.document_id, 0x1234);
  EXPECT_EQ(event.uri, u"test.js"sv);
  EXPECT_EQ(event.language_id, u"js"sv);
  EXPECT_EQ(event.content, u"hi"sv);
}

TEST(test_trace_reader, lsp_client_to_server_message_event) {
  auto stream = concat(example_packet_header,
                       make_array_explicit<std::uint8_t>(
                           // Timestamp
                           0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Event ID
                           0x06,

                           // Body
                           2, 0, 0, 0, 0, 0, 0, 0,  // Size
                           '{', '}'));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and lsp event";
  EXPECT_EQ(events[1].type,
            parsed_trace_event_type::lsp_client_to_server_message_event);
  parsed_lsp_client_to_server_message_event& event =
      events[1].lsp_client_to_server_message_event;
  EXPECT_EQ(event.timestamp, 0x5678);
  EXPECT_EQ(event.body, u8"{}"_sv);
}

TEST(test_trace_reader, read_lsp_client_to_server_message_event_in_two_parts) {
  auto event = make_array_explicit<std::uint8_t>(
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

      // Event ID
      0x06,

      // Body
      2, 0, 0, 0, 0, 0, 0, 0,  // Size
      '{', '}');
  trace_reader reader;
  reader.append_bytes(example_packet_header.data(),
                      example_packet_header.size());
  reader.append_bytes(event.data(), event.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and lsp event";
  EXPECT_EQ(events[1].type,
            parsed_trace_event_type::lsp_client_to_server_message_event);
  parsed_lsp_client_to_server_message_event& e =
      events[1].lsp_client_to_server_message_event;
  EXPECT_EQ(e.timestamp, 0x5678);
  EXPECT_EQ(e.body, u8"{}"_sv);
}

TEST(test_trace_reader, vector_max_size_histogram_by_owner_event) {
  auto stream = concat(example_packet_header,
                       make_array_explicit<std::uint8_t>(
                           // Timestamp
                           0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Event ID
                           0x07,

                           // Entry count
                           2, 0, 0, 0, 0, 0, 0, 0,

                           // Entry 0 owner
                           'o', '1', 0,
                           // Entry 0 max size entries
                           2, 0, 0, 0, 0, 0, 0, 0,  // Count
                           0, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 0 max size
                           4, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 0 count
                           1, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 1 max size
                           3, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 1 count

                           // Entry 1 owner
                           'o', '2', 0,
                           // Entry 1 max size entries
                           1, 0, 0, 0, 0, 0, 0, 0,  // Count
                           3, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 0 max size
                           7, 0, 0, 0, 0, 0, 0, 0));  // Max size entry 0 count
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and histogram event";
  EXPECT_EQ(events[1].type,
            parsed_trace_event_type::vector_max_size_histogram_by_owner_event);
  parsed_vector_max_size_histogram_by_owner_event& event =
      events[1].vector_max_size_histogram_by_owner_event;
  EXPECT_EQ(event.timestamp, 0x5678);

  ASSERT_EQ(event.entries.size(), 2);
  EXPECT_EQ(event.entries[0].owner, u8"o1"_sv);
  EXPECT_THAT(
      event.entries[0].max_size_entries,
      ElementsAreArray({
          parsed_vector_max_size_histogram_entry{.max_size = 0, .count = 4},
          parsed_vector_max_size_histogram_entry{.max_size = 1, .count = 3},
      }));
  EXPECT_EQ(event.entries[1].owner, u8"o2"_sv);
  EXPECT_THAT(
      event.entries[1].max_size_entries,
      ElementsAreArray({
          parsed_vector_max_size_histogram_entry{.max_size = 3, .count = 7},
      }));
}

TEST(test_trace_reader, process_id_event) {
  auto stream = concat(example_packet_header,
                       make_array_explicit<std::uint8_t>(
                           // Timestamp
                           0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Event ID
                           0x08,

                           // Process ID
                           0x23, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and process event";
  EXPECT_EQ(events[1].type, parsed_trace_event_type::process_id_event);
  parsed_process_id_event& event = events[1].process_id_event;
  EXPECT_EQ(event.timestamp, 0x5678);
  EXPECT_EQ(event.process_id, 0x0123);
}

TEST(test_trace_reader, lsp_documents_event) {
  auto stream = concat(example_packet_header,
                       make_array_explicit<std::uint8_t>(
                           // Timestamp
                           0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Event ID
                           0x09,

                           // Document count
                           0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Document 0: type
                           0x02,

                           // Document 0: URI
                           0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                           'f', 'i', 'l', 'e', ':', '/', '/', '/', 'f',

                           // Document 0: text
                           0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
                           'h', 'e', 'l', 'l', 'o'));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 2) << "expected packet header and lsp event";
  EXPECT_EQ(events[1].type, parsed_trace_event_type::lsp_documents_event);
  parsed_lsp_documents_event& event = events[1].lsp_documents_event;
  EXPECT_EQ(event.timestamp, 0x5678);
  ASSERT_EQ(event.documents.size(), 1);
  EXPECT_EQ(event.documents[0].type, parsed_lsp_document_type::lintable);
  EXPECT_EQ(event.documents[0].uri, u8"file:///f"_sv);
  EXPECT_EQ(event.documents[0].text, u8"hello"_sv);
}

TEST(test_trace_reader, invalid_lsp_document_type) {
  auto stream = concat(example_packet_header,
                       make_array_explicit<std::uint8_t>(
                           // Timestamp
                           0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Event ID
                           0x09,

                           // Document count
                           0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Document 0: type (invalid)
                           0x69,

                           // Document 0: URI
                           0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Document 0: text
                           0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00));
  trace_reader reader;
  reader.append_bytes(stream.data(), stream.size());

  std::vector<parsed_trace_event> events = reader.pull_new_events();
  ASSERT_EQ(events.size(), 3)
      << "expected packet header, error event, and lsp event";
  EXPECT_EQ(events[1].type,
            parsed_trace_event_type::error_unsupported_lsp_document_type);
  EXPECT_EQ(events[2].type, parsed_trace_event_type::lsp_documents_event);
  parsed_lsp_documents_event& event = events[2].lsp_documents_event;
  ASSERT_EQ(event.documents.size(), 1);
  EXPECT_EQ(event.documents[0].type, parsed_lsp_document_type::unknown);
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
