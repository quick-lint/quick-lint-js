// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRACE_STREAM_READER_H
#define QUICK_LINT_JS_TRACE_STREAM_READER_H

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/char8.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
class trace_stream_event_visitor;

void read_trace_stream(const void* data, std::size_t data_size,
                       trace_stream_event_visitor&);

class trace_stream_event_visitor {
 public:
  struct packet_header {
    std::uint64_t thread_id;
  };

  struct init_event {
    std::uint64_t timestamp;
    const char* version;  // null-terminated
  };

  struct vscode_document_opened_event {
    std::uint64_t timestamp;
    std::uint64_t document_id;
    std::u16string_view uri;
    std::u16string_view language_id;
    std::u16string_view content;
  };

  struct vscode_document_closed_event {
    std::uint64_t timestamp;
    std::uint64_t document_id;
    std::u16string_view uri;
    std::u16string_view language_id;
  };

  struct vscode_document_position {
    std::uint64_t line;
    std::uint64_t character;
  };

  struct vscode_document_range {
    vscode_document_position start;
    vscode_document_position end;
  };

  struct vscode_document_change {
    vscode_document_range range;
    std::uint64_t range_offset;
    std::uint64_t range_length;
    std::u16string_view text;

    // For testing.
    bool operator==(const vscode_document_change& other) const noexcept;
    bool operator!=(const vscode_document_change& other) const noexcept;
  };

  struct vscode_document_changed_event {
    std::uint64_t timestamp;
    std::uint64_t document_id;
    std::vector<vscode_document_change> changes;
  };

  struct vscode_document_sync_event {
    std::uint64_t timestamp;
    std::uint64_t document_id;
    std::u16string_view uri;
    std::u16string_view language_id;
    std::u16string_view content;
  };

  struct lsp_client_to_server_message_event {
    std::uint64_t timestamp;
    string8_view body;
  };

  virtual ~trace_stream_event_visitor() = 0;

  virtual void visit_error_invalid_magic() = 0;
  virtual void visit_error_invalid_uuid() = 0;
  virtual void visit_error_unsupported_compression_mode(std::uint8_t) = 0;

  virtual void visit_packet_header(const packet_header&) = 0;

  virtual void visit_init_event(const init_event&) = 0;
  virtual void visit_vscode_document_opened_event(
      const vscode_document_opened_event&) = 0;
  virtual void visit_vscode_document_closed_event(
      const vscode_document_closed_event&) = 0;
  virtual void visit_vscode_document_changed_event(
      const vscode_document_changed_event&) = 0;
  virtual void visit_vscode_document_sync_event(
      const vscode_document_sync_event&) = 0;
  virtual void visit_lsp_client_to_server_message_event(
      const lsp_client_to_server_message_event&) = 0;
};
}

#endif

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
