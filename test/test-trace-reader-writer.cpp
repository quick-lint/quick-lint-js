// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/source-location.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/binary-writer.h>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
template <class Event>
void check_read_event(
    std::initializer_list<std::uint8_t> bytes_excluding_packet_header,
    const Trace_Event_Header& expected_header, const Event& expected_event,
    Source_Location caller) {
  // clang-format off
  static constexpr std::array<std::uint8_t, 4+16+8+1> packet_header = {
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

  Trace_Reader reader;
  reader.append_bytes(packet_header.data(), packet_header.size());
  reader.append_bytes(bytes_excluding_packet_header.begin(),
                      bytes_excluding_packet_header.size());

  std::vector<Parsed_Trace_Event> events = reader.pull_new_events();
  EXPECT_EQ_AT_CALLER(events.size(), 2)
      << "expected packet header and event from Trace_Reader";
  if (events.size() < 2) return;
  EXPECT_EQ_AT_CALLER(events[1].header, expected_header)
      << "header from Trace_Reader";
  EXPECT_EQ_AT_CALLER(events[1].template get_event<Event>(), expected_event)
      << "event from Trace_Reader";
}

template <class Event>
void check_write_event(const Trace_Event_Header& header, const Event& event,
                       std::initializer_list<std::uint8_t> expected_bytes,
                       Source_Location caller) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);
  w.write_event(header, event);
  data.commit();
  EXPECT_THAT_AT_CALLER(data.take_committed_string8(),
                        ElementsAreArray<std::uint8_t>(expected_bytes))
      << "bytes from Trace_Writer";
}

template <class Event>
void check_event(
    const Trace_Event_Header& header, const Event& event,
    std::initializer_list<std::uint8_t> bytes_excluding_packet_header,
    Source_Location caller = Source_Location::current()) {
  check_write_event(header, event, bytes_excluding_packet_header, caller);
  check_read_event(bytes_excluding_packet_header, header, event, caller);
}

TEST(Test_Trace_Reader_Writer, event_init) {
  check_event(Trace_Event_Header{.timestamp = 0x5678},
              Trace_Event_Init{
                  .version = u8"1.0.0"_sv,
              },
              {
                  // clang-format off
                        // Timestamp
                        0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                        // Event ID
                        0x01,

                        // Version
                        u8'1', u8'.', u8'0', u8'.', u8'0', u8'\0',
                  // clang-format on
              });
}

TEST(Test_Trace_Reader_Writer, event_vscode_document_opened) {
  check_event(Trace_Event_Header{.timestamp = 0x5678},
              Trace_Event_VSCode_Document_Opened<std::u16string_view>{
                  .document_id = 0x1234,
                  .uri = u"test.js",
                  .language_id = u"js",
                  .content = u"hi",
              },
              {
                  // clang-format off
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
                        'h', 0, 'i', 0,
                  // clang-format on
              });
}

TEST(Test_Trace_Reader_Writer, event_vscode_document_closed) {
  check_event(Trace_Event_Header{.timestamp = 0x5678},
              Trace_Event_VSCode_Document_Closed<std::u16string_view>{
                  .document_id = 0x1234,
                  .uri = u"test.js",
                  .language_id = u"js",
              },
              {
                  // clang-format off
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
                        'j', 0, 's', 0,
                  // clang-format on
              });
}

TEST(Test_Trace_Reader_Writer, event_vscode_document_changed) {
  std::array changes{
      Trace_VSCode_Document_Change<std::u16string_view>{
          .range =
              {
                  .start = {.line = 0x11, .character = 0x22},
                  .end = {.line = 0x33, .character = 0x44},
              },
          .range_offset = 0x55,
          .range_length = 0x66,
          .text = u"hi",
      },
      Trace_VSCode_Document_Change<std::u16string_view>{
          .range =
              {
                  .start = {.line = 0xaa, .character = 0xbb},
                  .end = {.line = 0xcc, .character = 0xdd},
              },
          .range_offset = 0xee,
          .range_length = 0xff,
          .text = u"bye",
      },
  };

  check_event(
      Trace_Event_Header{.timestamp = 0x5678},
      Trace_Event_VSCode_Document_Changed<std::u16string_view>{
          .document_id = 0x1234,
          .changes =
              Span<const Trace_VSCode_Document_Change<std::u16string_view>>(
                  changes),
      },
      {
          // clang-format off
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
          'b', 0, 'y', 0, 'e', 0,
          // clang-format on
      });
}

TEST(Test_Trace_Reader_Writer, event_vscode_document_sync) {
  check_event(Trace_Event_Header{.timestamp = 0x5678},
              Trace_Event_VSCode_Document_Sync<std::u16string_view>{
                  .document_id = 0x1234,
                  .uri = u"test.js",
                  .language_id = u"js",
                  .content = u"hi",
              },
              {
                  // clang-format off
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
                        'h', 0, 'i', 0,
                  // clang-format on
              });
}

TEST(Test_Trace_Reader_Writer, event_lsp_client_to_server_message) {
  check_event(Trace_Event_Header{.timestamp = 0x5678},
              Trace_Event_LSP_Client_To_Server_Message{
                  .body = u8"{ }"_sv,
              },
              {
                  // clang-format off
                        // Timestamp
                        0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                        // Event ID
                        0x06,

                        // Body
                        3, 0, 0, 0, 0, 0, 0, 0,  // Size
                        '{', ' ', '}',
                  // clang-format on
              });
}

TEST(Test_Trace_Reader_Writer, event_vector_max_size_histogram_by_owner) {
  Trace_Vector_Max_Size_Histogram_Entry o1_entries[] = {
      {.max_size = 0, .count = 4},
      {.max_size = 1, .count = 3},
  };
  Trace_Vector_Max_Size_Histogram_Entry o2_entries[] = {
      {.max_size = 3, .count = 7},
  };
  Trace_Vector_Max_Size_Histogram_By_Owner_Entry entries[] = {
      {
          .owner = u8"o1"_sv,
          .max_size_entries =
              Span<const Trace_Vector_Max_Size_Histogram_Entry>(o1_entries),
      },
      {
          .owner = u8"o2"_sv,
          .max_size_entries =
              Span<const Trace_Vector_Max_Size_Histogram_Entry>(o2_entries),
      },
  };
  check_event(
      Trace_Event_Header{.timestamp = 0x5678},
      Trace_Event_Vector_Max_Size_Histogram_By_Owner{
          .entries = Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>(
              entries),
      },
      {
          // clang-format off
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
          7, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 0 count
                                   // clang-format on
      });
}

TEST(Test_Trace_Reader_Writer, event_process_id) {
  check_event(Trace_Event_Header{.timestamp = 0x5678},
              Trace_Event_Process_ID{
                  .process_id = 0x0123,
              },
              {
                  // clang-format off
                        // Timestamp
                        0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                        // Event ID
                        0x08,

                        // Process ID
                        0x23, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                  // clang-format on
              });
}

TEST(Test_Trace_Reader_Writer, event_lsp_documents) {
  std::array<Trace_LSP_Document_State, 1> documents = {
      Trace_LSP_Document_State{
          .type = Trace_LSP_Document_Type::lintable,
          .uri = u8"file:///f"_sv,
          .text = u8"hello"_sv,
          .language_id = u8"js"_sv,
      },
  };
  check_event(Trace_Event_Header{.timestamp = 0x5678},
              Trace_Event_LSP_Documents{
                  .documents = Span<const Trace_LSP_Document_State>(documents),
              },
              {
                  // clang-format off
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
          'h', 'e', 'l', 'l', 'o',

          // Document 0: langauge ID
          0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  //
          'j', 's',
                  // clang-format on
              });
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
