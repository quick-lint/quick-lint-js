// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LOGGING_TRACE_STREAM_READER_H
#define QUICK_LINT_JS_LOGGING_TRACE_STREAM_READER_H

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/port/char8.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
class checked_binary_reader;
class trace_stream_event_visitor;

// Designed for convenience, not efficiency.
class trace_stream_reader {
 public:
  explicit trace_stream_reader(trace_stream_event_visitor*);

  void append_bytes(const void* data, std::size_t data_size);

 private:
  void parse_one(checked_binary_reader&);
  void parse_stream_header(checked_binary_reader&);
  void parse_event(checked_binary_reader&);

  trace_stream_event_visitor* visitor_;
  std::vector<std::uint8_t> queue_;
  bool parsed_header_ = false;
};

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

  struct vector_max_size_histogram_entry {
    std::uint64_t max_size;
    std::uint64_t count;

    friend bool operator==(
        const vector_max_size_histogram_entry& lhs,
        const vector_max_size_histogram_entry& rhs) noexcept {
      return lhs.max_size == rhs.max_size && lhs.count == rhs.count;
    }

    friend bool operator!=(
        const vector_max_size_histogram_entry& lhs,
        const vector_max_size_histogram_entry& rhs) noexcept {
      return !(lhs == rhs);
    }
  };

  struct vector_max_size_histogram_by_owner_entry {
    string8_view owner;
    std::vector<vector_max_size_histogram_entry> max_size_entries;
  };

  struct vector_max_size_histogram_by_owner_event {
    std::uint64_t timestamp;
    std::vector<vector_max_size_histogram_by_owner_entry> entries;
  };

  struct process_id_event {
    std::uint64_t timestamp;
    std::uint64_t process_id;
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
  virtual void visit_vector_max_size_histogram_by_owner_event(
      const vector_max_size_histogram_by_owner_event&) = 0;
  virtual void visit_process_id_event(const process_id_event&) = 0;
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
