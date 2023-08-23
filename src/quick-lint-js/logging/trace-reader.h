// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/logging/trace-writer.h>
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

  // The memory returned by pull_new_events is only valid until the next call to
  // pull_new_events, append_bytes, or ~Trace_Reader. In other words,
  // pull_new_events and append_bytes deallocate memory allocated by the
  // previous call to pull_new_events.
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
  // The number of bytes at the beginning of this->queue_ which pull_new_events
  // has already parsed. These bytes can be erased.
  std::size_t parsed_bytes_ = 0;

  Monotonic_Allocator memory_{"Trace_Reader::memory"};
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

struct Parsed_Trace_Event {
  Parsed_Trace_Event_Type type;

  union {
    Trace_Context packet_header;

    Trace_Event_Init init_event;
    Trace_Event_VSCode_Document_Opened<std::u16string_view>
        vscode_document_opened_event;
    Trace_Event_VSCode_Document_Closed<std::u16string_view>
        vscode_document_closed_event;
    Trace_Event_VSCode_Document_Changed<std::u16string_view>
        vscode_document_changed_event;
    Trace_Event_VSCode_Document_Sync<std::u16string_view>
        vscode_document_sync_event;
    Trace_Event_LSP_Client_To_Server_Message lsp_client_to_server_message_event;
    Trace_Event_Vector_Max_Size_Histogram_By_Owner
        vector_max_size_histogram_by_owner_event;
    Trace_Event_Process_ID process_id_event;
    Trace_Event_LSP_Documents lsp_documents_event;
  };
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
