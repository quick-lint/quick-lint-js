// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <quick-lint-js/binary-reader.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/trace-stream-reader.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
trace_stream_event_visitor::~trace_stream_event_visitor() = default;

void read_trace_stream(const void* data, std::size_t data_size,
                       trace_stream_event_visitor& v) {
  checked_binary_reader r(reinterpret_cast<const std::uint8_t*>(data),
                          data_size);
  auto read_utf16le_string = [&r]() -> std::u16string_view {
    std::uint64_t length = r.u64_le();
    const std::uint8_t* bytes = r.advance(length * 2);
    static_assert(sizeof(char16_t) == 2 * sizeof(std::uint8_t));
    // TODO(strager): This assumes the native endian is little endian.
    return std::u16string_view(reinterpret_cast<const char16_t*>(bytes),
                               length);
  };
  auto read_utf8_string = [&r]() -> string8_view {
    std::uint64_t length = r.u64_le();
    const std::uint8_t* bytes = r.advance(length);
    static_assert(sizeof(char8) == sizeof(std::uint8_t));
    return string8_view(reinterpret_cast<const char8*>(bytes), length);
  };

  std::uint32_t magic = r.u32_le();
  if (magic != 0xc1fc1fc1) {
    v.visit_error_invalid_magic();
  }

  static constexpr std::uint8_t quick_lint_js_uuid[] = {
      0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,
      0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,
  };
  const std::uint8_t* uuid = r.advance(std::size(quick_lint_js_uuid));
  if (!std::equal(std::begin(quick_lint_js_uuid), std::end(quick_lint_js_uuid),
                  uuid)) {
    v.visit_error_invalid_uuid();
  }

  std::uint64_t thread_id = r.u64_le();
  v.visit_packet_header(trace_stream_event_visitor::packet_header{
      .thread_id = thread_id,
  });

  std::uint8_t compression_mode = r.u8();
  if (compression_mode != 0) {
    v.visit_error_unsupported_compression_mode(compression_mode);
  }

  while (!r.eof()) {
    std::uint64_t timestamp = r.u64_le();
    std::uint8_t event_id = r.u8();
    switch (event_id) {
    case 0x01: {
      const std::uint8_t* version_string = r.cursor();
      r.find_and_skip_byte(0x00);
      v.visit_init_event(trace_stream_event_visitor::init_event{
          .timestamp = timestamp,
          .version = reinterpret_cast<const char*>(version_string),
      });
      break;
    }

    case 0x02:
      v.visit_vscode_document_opened_event(
          trace_stream_event_visitor::vscode_document_opened_event{
              .timestamp = timestamp,
              .document_id = r.u64_le(),
              .uri = read_utf16le_string(),
              .language_id = read_utf16le_string(),
              .content = read_utf16le_string(),
          });
      break;

    case 0x03:
      v.visit_vscode_document_closed_event(
          trace_stream_event_visitor::vscode_document_closed_event{
              .timestamp = timestamp,
              .document_id = r.u64_le(),
              .uri = read_utf16le_string(),
              .language_id = read_utf16le_string(),
          });
      break;

    case 0x04: {
      std::uint64_t document_id = r.u64_le();
      std::uint64_t change_count = r.u64_le();
      std::vector<trace_stream_event_visitor::vscode_document_change> changes;
      for (std::uint64_t i = 0; i < change_count; ++i) {
        changes.push_back(trace_stream_event_visitor::vscode_document_change{
            .range =
                {
                    .start =
                        {
                            .line = r.u64_le(),
                            .character = r.u64_le(),
                        },
                    .end =
                        {
                            .line = r.u64_le(),
                            .character = r.u64_le(),
                        },
                },
            .range_offset = r.u64_le(),
            .range_length = r.u64_le(),
            .text = read_utf16le_string(),
        });
      }
      v.visit_vscode_document_changed_event(
          trace_stream_event_visitor::vscode_document_changed_event{
              .timestamp = timestamp,
              .document_id = document_id,
              .changes = std::move(changes),
          });
      break;
    }

    case 0x05:
      v.visit_vscode_document_sync_event(
          trace_stream_event_visitor::vscode_document_sync_event{
              .timestamp = timestamp,
              .document_id = r.u64_le(),
              .uri = read_utf16le_string(),
              .language_id = read_utf16le_string(),
              .content = read_utf16le_string(),
          });
      break;

    case 0x06:
      v.visit_lsp_client_to_server_message_event(
          trace_stream_event_visitor::lsp_client_to_server_message_event{
              .timestamp = timestamp,
              .body = read_utf8_string(),
          });
      break;

    default:
      // TODO(strager): Report an error.
      return;
    }
  }
}

bool trace_stream_event_visitor::vscode_document_change::operator==(
    const vscode_document_change& other) const noexcept {
  return this->range.start.line == other.range.start.line &&
         this->range.start.character == other.range.start.character &&
         this->range.end.line == other.range.end.line &&
         this->range.end.character == other.range.end.character &&
         this->range_offset == other.range_offset &&
         this->range_length == other.range_length && this->text == other.text;
}

bool trace_stream_event_visitor::vscode_document_change::operator!=(
    const vscode_document_change& other) const noexcept {
  return !(*this == other);
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
