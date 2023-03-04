// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LOGGING_TRACE_READER_H
#define QUICK_LINT_JS_LOGGING_TRACE_READER_H

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/port/char8.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
class checked_binary_reader;
enum class parsed_trace_event_type;
struct parsed_trace_event;

// Designed for convenience, not efficiency.
class trace_reader {
 public:
  explicit trace_reader();
  ~trace_reader();

  void append_bytes(const void* data, std::size_t data_size);

  std::vector<parsed_trace_event> pull_new_events();
  void pull_new_events(std::vector<parsed_trace_event>& out);

 private:
  void parse_one(checked_binary_reader&);
  void parse_header(checked_binary_reader&);
  void parse_event(checked_binary_reader&);

  void on_error(parsed_trace_event_type);

  std::vector<std::uint8_t> queue_;
  std::vector<parsed_trace_event> parsed_events_;
  bool parsed_header_ = false;
  bool encountered_error_ = false;
};

enum class parsed_trace_event_type {
  error_invalid_magic,
  error_invalid_uuid,
  error_unsupported_compression_mode,

  packet_header,

  init_event,
  vscode_document_opened_event,
  vscode_document_closed_event,
  vscode_document_changed_event,
  vscode_document_sync_event,
  lsp_client_to_server_message_event,
  vector_max_size_histogram_by_owner_event,
  process_id_event,
};

struct parsed_packet_header {
  std::uint64_t thread_id;
};

struct parsed_init_event {
  std::uint64_t timestamp;
  string8 version;
};

struct parsed_vscode_document_opened_event {
  std::uint64_t timestamp;
  std::uint64_t document_id;
  std::u16string uri;
  std::u16string language_id;
  std::u16string content;
};

struct parsed_vscode_document_closed_event {
  std::uint64_t timestamp;
  std::uint64_t document_id;
  std::u16string uri;
  std::u16string language_id;
};

struct parsed_vscode_document_position {
  std::uint64_t line;
  std::uint64_t character;
};

struct parsed_vscode_document_range {
  parsed_vscode_document_position start;
  parsed_vscode_document_position end;
};

struct parsed_vscode_document_change {
  parsed_vscode_document_range range;
  std::uint64_t range_offset;
  std::uint64_t range_length;
  std::u16string text;

  // For testing.
  bool operator==(const parsed_vscode_document_change& other) const noexcept;
  bool operator!=(const parsed_vscode_document_change& other) const noexcept;
};

struct parsed_vscode_document_changed_event {
  std::uint64_t timestamp;
  std::uint64_t document_id;
  std::vector<parsed_vscode_document_change> changes;
};

struct parsed_vscode_document_sync_event {
  std::uint64_t timestamp;
  std::uint64_t document_id;
  std::u16string uri;
  std::u16string language_id;
  std::u16string content;
};

struct parsed_lsp_client_to_server_message_event {
  std::uint64_t timestamp;
  string8 body;
};

struct parsed_vector_max_size_histogram_entry {
  std::uint64_t max_size;
  std::uint64_t count;

  friend bool operator==(
      const parsed_vector_max_size_histogram_entry& lhs,
      const parsed_vector_max_size_histogram_entry& rhs) noexcept {
    return lhs.max_size == rhs.max_size && lhs.count == rhs.count;
  }

  friend bool operator!=(
      const parsed_vector_max_size_histogram_entry& lhs,
      const parsed_vector_max_size_histogram_entry& rhs) noexcept {
    return !(lhs == rhs);
  }
};

struct parsed_vector_max_size_histogram_by_owner_entry {
  string8 owner;
  std::vector<parsed_vector_max_size_histogram_entry> max_size_entries;
};

struct parsed_vector_max_size_histogram_by_owner_event {
  std::uint64_t timestamp;
  std::vector<parsed_vector_max_size_histogram_by_owner_entry> entries;
};

struct parsed_process_id_event {
  std::uint64_t timestamp;
  std::uint64_t process_id;
};

struct parsed_trace_event {
  parsed_trace_event_type type;

  // NOTE(strager): The below fields have an explicit initializer to silence
  // GCC's -Wmissing-field-initializers warning in users.

  parsed_packet_header packet_header = {};

  parsed_init_event init_event = {};
  parsed_vscode_document_opened_event vscode_document_opened_event = {};
  parsed_vscode_document_closed_event vscode_document_closed_event = {};
  parsed_vscode_document_changed_event vscode_document_changed_event = {};
  parsed_vscode_document_sync_event vscode_document_sync_event = {};
  parsed_lsp_client_to_server_message_event lsp_client_to_server_message_event =
      {};
  parsed_vector_max_size_histogram_by_owner_event
      vector_max_size_histogram_by_owner_event = {};
  parsed_process_id_event process_id_event = {};
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
