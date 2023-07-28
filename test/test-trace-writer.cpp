// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/binary-writer.h>

using ::testing::ElementsAreArray;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
class U16_CString_Trace_String_Writer {
 public:
  std::size_t string_size(void* string) const {
    return this->get(string).size();
  }

  void copy_string(void* string, char16_t* out, std::size_t capacity) const {
    std::u16string_view s = this->get(string);
    QLJS_ASSERT(capacity >= s.size());
    std::copy(s.begin(), s.end(), out);
  }

 private:
  static std::u16string_view get(void* string) {
    return static_cast<const char16_t*>(string);
  }
};

TEST(Test_Trace_Writer, write_header) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);
  w.write_header(Trace_Context{
      .thread_id = 0x1234,
  });

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
                  // clang-format off
                  // CTF magic
                  0xc1, 0x1f, 0xfc, 0xc1,

                  // quick-lint-js metadata UUID
                  0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,  //
                  0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,

                  // Thread ID
                  0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                  // Compression mode
                  0x00,
                  // clang-format on
              }));
}

TEST(Test_Trace_Writer, write_event_init) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);
  w.write_event_init(Trace_Event_Init{
      .timestamp = 0x5678,
      .version = u8"1.0.0"_sv,
  });

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
                  // clang-format off
                  // Timestamp
                  0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                  // Event ID
                  0x01,

                  // Version
                  u8'1', u8'.', u8'0', u8'.', u8'0', u8'\0',
                  // clang-format on
              }));
}

TEST(Test_Trace_Writer, write_event_vscode_document_opened) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);

  w.write_event_vscode_document_opened(
      Trace_Event_VSCode_Document_Opened{
          .timestamp = 0x5678,
          .document_id = 0x1234,
          .uri = const_cast<char16_t*>(u"test.js"),
          .language_id = const_cast<char16_t*>(u"js"),
          .content = const_cast<char16_t*>(u"hi"),
      },
      U16_CString_Trace_String_Writer());

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
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
              }));
}

TEST(Test_Trace_Writer, write_event_vscode_document_closed) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);

  w.write_event_vscode_document_closed(
      Trace_Event_VSCode_Document_Closed{
          .timestamp = 0x5678,
          .document_id = 0x1234,
          .uri = const_cast<char16_t*>(u"test.js"),
          .language_id = const_cast<char16_t*>(u"js"),
      },
      U16_CString_Trace_String_Writer());

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
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
              }));
}

TEST(Test_Trace_Writer, write_event_vscode_document_changed) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);

  std::array changes{
      Trace_VSCode_Document_Change{
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
          .text = const_cast<char16_t*>(u"hi"),
      },
      Trace_VSCode_Document_Change{
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
          .text = const_cast<char16_t*>(u"bye"),
      },
  };

  w.write_event_vscode_document_changed(
      Trace_Event_VSCode_Document_Changed{
          .timestamp = 0x5678,
          .document_id = 0x1234,
          .changes = changes.data(),
          .change_count = changes.size(),
      },
      U16_CString_Trace_String_Writer());

  data.commit();
  EXPECT_THAT(
      data.take_committed_string8(),
      ElementsAreArray<std::uint8_t>({
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
      }));
}

TEST(Test_Trace_Writer, write_event_vscode_document_sync) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);

  w.write_event_vscode_document_sync(
      Trace_Event_VSCode_Document_Sync{
          .timestamp = 0x5678,
          .document_id = 0x1234,
          .uri = const_cast<char16_t*>(u"test.js"),
          .language_id = const_cast<char16_t*>(u"js"),
          .content = const_cast<char16_t*>(u"hi"),
      },
      U16_CString_Trace_String_Writer());

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
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
              }));
}

TEST(Test_Trace_Writer, write_event_lsp_client_to_server_message) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);

  w.write_event_lsp_client_to_server_message(
      Trace_Event_LSP_Client_To_Server_Message{
          .timestamp = 0x5678,
          .body = u8"{ }"_sv,
      });

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
                  // clang-format off
                  // Timestamp
                  0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                  // Event ID
                  0x06,

                  // Body
                  3, 0, 0, 0, 0, 0, 0, 0,  // Size
                  '{', ' ', '}',
                  // clang-format on
              }));
}

TEST(Test_Trace_Writer, write_event_vector_max_size_histogram_by_owner) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);

  std::map<std::string_view, std::map<std::size_t, int>> histogram = {
      {"o1"sv,
       {
           {0, 4},
           {1, 3},
       }},
      {"o2"sv,
       {
           {3, 7},
       }},
  };
  w.write_event_vector_max_size_histogram_by_owner(
      Trace_Event_Vector_Max_Size_Histogram_By_Owner{
          .timestamp = 0x5678,
          .histogram = &histogram,
      });

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
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
                  1, 0, 0, 0, 0, 0, 0, 0,    // Count
                  3, 0, 0, 0, 0, 0, 0, 0,    // Max size entry 0 max size
                  7, 0, 0, 0, 0, 0, 0, 0,
                  // clang-format on
              }));  // Max size entry 0 count
}

TEST(Test_Trace_Writer, write_event_process_id) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);

  w.write_event_process_id(Trace_Event_Process_ID{
      .timestamp = 0x5678,
      .process_id = 0x0123,
  });

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
                  // clang-format off
                  // Timestamp
                  0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                  // Event ID
                  0x08,

                  // Process ID
                  0x23, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                  // clang-format on
              }));
}

TEST(Test_Trace_Writer, write_event_lsp_documents) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);

  std::array<Trace_LSP_Document_State, 1> documents = {
      Trace_LSP_Document_State{
          .type = Trace_LSP_Document_Type::lintable,
          .uri = u8"file:///f"_sv,
          .text = u8"hello"_sv,
          .language_id = u8"js"_sv,
      },
  };
  w.write_event_lsp_documents(Trace_Event_LSP_Documents{
      .timestamp = 0x5678,
      .documents = Span<const Trace_LSP_Document_State>(documents),
  });

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
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
              }));
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
