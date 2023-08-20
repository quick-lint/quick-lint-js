// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/port/char8.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
class Checked_Binary_Reader;
enum class Parsed_Trace_Event_Type;
struct Parsed_Trace_Event;

// Designed for convenience, not efficiency.
class Trace_Reader {
 public:
  explicit Trace_Reader();
  ~Trace_Reader();

  void append_bytes(const void* data, std::size_t data_size);

  std::vector<Parsed_Trace_Event> pull_new_events();
  void pull_new_events(std::vector<Parsed_Trace_Event>& out);

 private:
  void parse_one(Checked_Binary_Reader&);
  void parse_header(Checked_Binary_Reader&);
  void parse_event(Checked_Binary_Reader&);

  void on_error(Parsed_Trace_Event_Type);

  std::vector<std::uint8_t> queue_;
  std::vector<Parsed_Trace_Event> parsed_events_;
  bool parsed_header_ = false;
  bool encountered_error_ = false;
};

enum class Parsed_Trace_Event_Type {
  error_invalid_magic,
  error_invalid_uuid,
  error_unsupported_compression_mode,
  error_unsupported_lsp_document_type,

  packet_header,

  init_event,
  vscode_document_opened_event,
  vscode_document_closed_event,
  vscode_document_changed_event,
  vscode_document_sync_event,
  lsp_client_to_server_message_event,
  vector_max_size_histogram_by_owner_event,
  process_id_event,
  lsp_documents_event,
};

struct Parsed_Packet_Header {
  std::uint64_t thread_id;
};

struct Parsed_Init_Event {
  std::uint64_t timestamp;
  String8 version;
};

struct Parsed_VSCode_Document_Opened_Event {
  std::uint64_t timestamp;
  std::uint64_t document_id;
  std::u16string uri;
  std::u16string language_id;
  std::u16string content;
};

struct Parsed_VSCode_Document_Closed_Event {
  std::uint64_t timestamp;
  std::uint64_t document_id;
  std::u16string uri;
  std::u16string language_id;
};

struct Parsed_VSCode_Document_Position {
  std::uint64_t line;
  std::uint64_t character;
};

struct Parsed_VSCode_Document_Range {
  Parsed_VSCode_Document_Position start;
  Parsed_VSCode_Document_Position end;
};

struct Parsed_VSCode_Document_Change {
  Parsed_VSCode_Document_Range range;
  std::uint64_t range_offset;
  std::uint64_t range_length;
  std::u16string text;

  // For testing.
  bool operator==(const Parsed_VSCode_Document_Change& other) const;
  bool operator!=(const Parsed_VSCode_Document_Change& other) const;
};

struct Parsed_VSCode_Document_Changed_Event {
  std::uint64_t timestamp;
  std::uint64_t document_id;
  std::vector<Parsed_VSCode_Document_Change> changes;
};

struct Parsed_VSCode_Document_Sync_Event {
  std::uint64_t timestamp;
  std::uint64_t document_id;
  std::u16string uri;
  std::u16string language_id;
  std::u16string content;
};

struct Parsed_LSP_Client_To_Server_Message_Event {
  std::uint64_t timestamp;
  String8 body;
};

struct Parsed_Vector_Max_Size_Histogram_Entry {
  std::uint64_t max_size;
  std::uint64_t count;

  friend bool operator==(const Parsed_Vector_Max_Size_Histogram_Entry& lhs,
                         const Parsed_Vector_Max_Size_Histogram_Entry& rhs) {
    return lhs.max_size == rhs.max_size && lhs.count == rhs.count;
  }

  friend bool operator!=(const Parsed_Vector_Max_Size_Histogram_Entry& lhs,
                         const Parsed_Vector_Max_Size_Histogram_Entry& rhs) {
    return !(lhs == rhs);
  }
};

struct Parsed_Vector_Max_Size_Histogram_By_Owner_Entry {
  String8 owner;
  std::vector<Parsed_Vector_Max_Size_Histogram_Entry> max_size_entries;
};

struct Parsed_Vector_Max_Size_Histogram_By_Owner_Event {
  std::uint64_t timestamp;
  std::vector<Parsed_Vector_Max_Size_Histogram_By_Owner_Entry> entries;
};

struct Parsed_Process_ID_Event {
  std::uint64_t timestamp;
  std::uint64_t process_id;
};

enum class Parsed_LSP_Document_Type : std::uint8_t {
  unknown = 0,
  config = 1,
  lintable = 2,
};
inline constexpr Parsed_LSP_Document_Type last_parsed_lsp_document_type =
    Parsed_LSP_Document_Type::lintable;

struct Parsed_LSP_Document_State {
  Parsed_LSP_Document_Type type;
  String8 uri;
  String8 text;
  String8 language_id;
};

struct Parsed_LSP_Documents_Event {
  std::uint64_t timestamp;
  std::vector<Parsed_LSP_Document_State> documents;
};

struct Parsed_Trace_Event {
  Parsed_Trace_Event_Type type;

  // NOTE(strager): The below fields have an explicit initializer to silence
  // GCC's -Wmissing-field-initializers warning in users.

  Parsed_Packet_Header packet_header = {};

  Parsed_Init_Event init_event = {};
  Parsed_VSCode_Document_Opened_Event vscode_document_opened_event = {};
  Parsed_VSCode_Document_Closed_Event vscode_document_closed_event = {};
  Parsed_VSCode_Document_Changed_Event vscode_document_changed_event = {};
  Parsed_VSCode_Document_Sync_Event vscode_document_sync_event = {};
  Parsed_LSP_Client_To_Server_Message_Event lsp_client_to_server_message_event =
      {};
  Parsed_Vector_Max_Size_Histogram_By_Owner_Event
      vector_max_size_histogram_by_owner_event = {};
  Parsed_Process_ID_Event process_id_event = {};
  Parsed_LSP_Documents_Event lsp_documents_event = {};
};
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
