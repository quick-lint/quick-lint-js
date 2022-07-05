// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/binary-reader.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/trace-stream-reader-mock.h>
#include <quick-lint-js/trace-stream-reader.h>

using ::testing::ElementsAre;
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

TEST(test_trace_stream_reader, empty_trace_has_no_events) {
  strict_mock_trace_stream_event_visitor v;
  EXPECT_CALL(
      v, visit_packet_header(::testing::Field(
             &trace_stream_event_visitor::packet_header::thread_id, 0x1234)));
  read_trace_stream(example_packet_header.data(), example_packet_header.size(),
                    v);
}

TEST(test_trace_stream_reader, invalid_magic_reports_error) {
  auto stream = example_packet_header;
  stream[0] = 0xc0;
  stream[3] = 0xc0;

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(v, visit_error_invalid_magic());
  read_trace_stream(stream.data(), stream.size(), v);
}

TEST(test_trace_stream_reader, invalid_uuid_reports_error) {
  auto stream = example_packet_header;
  stream[7] = 0xff;

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(v, visit_error_invalid_uuid());
  read_trace_stream(stream.data(), stream.size(), v);
}

TEST(test_trace_stream_reader, invalid_compression_mode_reports_error) {
  auto stream = example_packet_header;
  stream[4 + 16 + 8] = 0xfe;

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(v, visit_error_unsupported_compression_mode(0xfe));
  read_trace_stream(stream.data(), stream.size(), v);
}

TEST(test_trace_stream_reader, init_event) {
  auto stream = concat(example_packet_header,
                       make_array_explicit<std::uint8_t>(
                           // Timestamp
                           0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Event ID
                           0x01,

                           // Version
                           u8'1', u8'.', u8'0', u8'.', u8'0', u8'\0'));

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(
      v, visit_init_event(::testing::AllOf(
             ::testing::Field(
                 &trace_stream_event_visitor::init_event::timestamp, 0x5678),
             ::testing::Field(&trace_stream_event_visitor::init_event::version,
                              ::testing::StrEq("1.0.0")))));
  read_trace_stream(stream.data(), stream.size(), v);
}

TEST(test_trace_stream_reader, vscode_document_opened_event) {
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

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(
      v, visit_vscode_document_opened_event(::testing::AllOf(
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_opened_event::timestamp,
                              0x5678),
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_opened_event::document_id,
                              0x1234),
             ::testing::Field(
                 &trace_stream_event_visitor::vscode_document_opened_event::uri,
                 u"test.js"),
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_opened_event::language_id,
                              u"js"),
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_opened_event::content,
                              u"hi"))));
  read_trace_stream(stream.data(), stream.size(), v);
}

TEST(test_trace_stream_reader, vscode_document_closed_event) {
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

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(
      v, visit_vscode_document_closed_event(::testing::AllOf(
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_closed_event::timestamp,
                              0x5678),
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_closed_event::document_id,
                              0x1234),
             ::testing::Field(
                 &trace_stream_event_visitor::vscode_document_closed_event::uri,
                 u"test.js"),
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_closed_event::language_id,
                              u"js"))));
  read_trace_stream(stream.data(), stream.size(), v);
}

TEST(test_trace_stream_reader, vscode_document_changed_event) {
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

  nice_mock_trace_stream_event_visitor v;
  auto changes_matcher = ::testing::ElementsAre(
      trace_stream_event_visitor::vscode_document_change{
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
      trace_stream_event_visitor::vscode_document_change{
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
      });
  EXPECT_CALL(
      v, visit_vscode_document_changed_event(::testing::AllOf(
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_changed_event::timestamp,
                              0x5678),
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_changed_event::document_id,
                              0x1234),
             ::testing::Field(&trace_stream_event_visitor::
                                  vscode_document_changed_event::changes,
                              changes_matcher))));
  read_trace_stream(stream.data(), stream.size(), v);
}

TEST(test_trace_stream_reader, vscode_document_sync_event) {
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

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(
      v,
      visit_vscode_document_sync_event(::testing::AllOf(
          ::testing::Field(&trace_stream_event_visitor::
                               vscode_document_sync_event::timestamp,
                           0x5678),
          ::testing::Field(&trace_stream_event_visitor::
                               vscode_document_sync_event::document_id,
                           0x1234),
          ::testing::Field(
              &trace_stream_event_visitor::vscode_document_sync_event::uri,
              u"test.js"),
          ::testing::Field(&trace_stream_event_visitor::
                               vscode_document_sync_event::language_id,
                           u"js"),
          ::testing::Field(
              &trace_stream_event_visitor::vscode_document_sync_event::content,
              u"hi"))));
  read_trace_stream(stream.data(), stream.size(), v);
}

TEST(test_trace_stream_reader, lsp_client_to_server_message_event) {
  auto stream = concat(example_packet_header,
                       make_array_explicit<std::uint8_t>(
                           // Timestamp
                           0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                           // Event ID
                           0x06,

                           // Body
                           2, 0, 0, 0, 0, 0, 0, 0,  // Size
                           '{', '}'));

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(
      v, visit_lsp_client_to_server_message_event(::testing::AllOf(
             ::testing::Field(&trace_stream_event_visitor::
                                  lsp_client_to_server_message_event::timestamp,
                              0x5678),
             ::testing::Field(&trace_stream_event_visitor::
                                  lsp_client_to_server_message_event::body,
                              u8"{}"sv))));
  read_trace_stream(stream.data(), stream.size(), v);
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
