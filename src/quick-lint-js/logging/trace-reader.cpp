// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <csetjmp>
#include <cstddef>
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/binary-reader.h>
#include <vector>

namespace quick_lint_js {
trace_reader::trace_reader() = default;

trace_reader::~trace_reader() = default;

void trace_reader::append_bytes(const void* data, std::size_t data_size) {
  const std::uint8_t* new_bytes = reinterpret_cast<const std::uint8_t*>(data);
  this->queue_.insert(this->queue_.end(), new_bytes, new_bytes + data_size);

  std::jmp_buf buf;
  auto unexpected_end_of_file = [&buf]() {
    std::longjmp(buf, 1);
    QLJS_UNREACHABLE();
  };
  checked_binary_reader r(this->queue_.data(), this->queue_.size(),
                          unexpected_end_of_file);

  const std::uint8_t* committed = r.cursor();
  if (setjmp(buf) == 0) {
    while (!r.eof() && !this->encountered_error_) {
      this->parse_one(r);
      committed = r.cursor();
    }
  } else {
    // End of file prematurely reached.
  }

  this->queue_.erase(this->queue_.begin(),
                     this->queue_.begin() + (committed - this->queue_.data()));
}

std::vector<parsed_trace_event> trace_reader::pull_new_events() {
  std::vector<parsed_trace_event> result;
  this->pull_new_events(result);
  return result;
}

void trace_reader::pull_new_events(std::vector<parsed_trace_event>& out) {
  out.insert(out.end(), this->parsed_events_.begin(),
             this->parsed_events_.end());
  this->parsed_events_.clear();
}

void trace_reader::parse_one(checked_binary_reader& r) {
  if (this->parsed_header_) {
    this->parse_event(r);
  } else {
    this->parse_header(r);
  }
}

void trace_reader::parse_header(checked_binary_reader& r) {
  QLJS_ASSERT(!this->parsed_header_);

  std::uint32_t magic = r.u32_le();
  if (magic != 0xc1fc1fc1) {
    this->on_error(parsed_trace_event_type::error_invalid_magic);
    return;
  }

  static constexpr std::uint8_t quick_lint_js_uuid[] = {
      0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,
      0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,
  };
  const std::uint8_t* uuid = r.advance(std::size(quick_lint_js_uuid));
  if (!std::equal(std::begin(quick_lint_js_uuid), std::end(quick_lint_js_uuid),
                  uuid)) {
    this->on_error(parsed_trace_event_type::error_invalid_uuid);
    return;
  }

  std::uint64_t thread_id = r.u64_le();

  std::uint8_t compression_mode = r.u8();
  if (compression_mode != 0) {
    this->on_error(parsed_trace_event_type::error_unsupported_compression_mode);
    return;
  }

  this->parsed_events_.push_back(parsed_trace_event{
      .type = parsed_trace_event_type::packet_header,
      .packet_header =
          parsed_packet_header{
              .thread_id = thread_id,
          },
  });
  this->parsed_header_ = true;
}

void trace_reader::parse_event(checked_binary_reader& r) {
  auto read_utf16le_string = [&r]() -> std::u16string {
    std::uint64_t length = r.u64_le();
    const std::uint8_t* bytes = r.advance(length * 2);
    static_assert(sizeof(char16_t) == 2 * sizeof(std::uint8_t));
    // TODO(strager): This assumes the native endian is little endian.
    return std::u16string(reinterpret_cast<const char16_t*>(bytes), length);
  };
  auto read_utf8_string = [&r]() -> string8 {
    std::uint64_t length = r.u64_le();
    const std::uint8_t* bytes = r.advance(length);
    static_assert(sizeof(char8) == sizeof(std::uint8_t));
    return string8(reinterpret_cast<const char8*>(bytes), length);
  };
  auto read_utf8_zstring = [&r]() -> string8 {
    const std::uint8_t* bytes = r.cursor();
    r.find_and_skip_byte(0x00);
    const std::uint8_t* end = r.cursor() - 1;
    return string8(reinterpret_cast<const char8*>(bytes),
                   narrow_cast<std::size_t>(end - bytes));
  };

  std::uint64_t timestamp = r.u64_le();
  std::uint8_t event_id = r.u8();
  switch (event_id) {
  case 0x01: {
    const std::uint8_t* version_string = r.cursor();
    r.find_and_skip_byte(0x00);
    this->parsed_events_.push_back(parsed_trace_event{
        .type = parsed_trace_event_type::init_event,
        .init_event =
            parsed_init_event{
                .timestamp = timestamp,
                .version =
                    string8(reinterpret_cast<const char8*>(version_string)),
            },
    });
    break;
  }

  case 0x02:
    this->parsed_events_.push_back(parsed_trace_event{
        .type = parsed_trace_event_type::vscode_document_opened_event,
        .vscode_document_opened_event =
            parsed_vscode_document_opened_event{
                .timestamp = timestamp,
                .document_id = r.u64_le(),
                .uri = read_utf16le_string(),
                .language_id = read_utf16le_string(),
                .content = read_utf16le_string(),
            },
    });
    break;

  case 0x03:
    this->parsed_events_.push_back(parsed_trace_event{
        .type = parsed_trace_event_type::vscode_document_closed_event,
        .vscode_document_closed_event =
            parsed_vscode_document_closed_event{
                .timestamp = timestamp,
                .document_id = r.u64_le(),
                .uri = read_utf16le_string(),
                .language_id = read_utf16le_string(),
            },
    });
    break;

  case 0x04: {
    std::uint64_t document_id = r.u64_le();
    std::uint64_t change_count = r.u64_le();
    std::vector<parsed_vscode_document_change> changes;
    for (std::uint64_t i = 0; i < change_count; ++i) {
      changes.push_back(parsed_vscode_document_change{
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
    this->parsed_events_.push_back(parsed_trace_event{
        .type = parsed_trace_event_type::vscode_document_changed_event,
        .vscode_document_changed_event =
            parsed_vscode_document_changed_event{
                .timestamp = timestamp,
                .document_id = document_id,
                .changes = std::move(changes),
            },
    });
    break;
  }

  case 0x05:
    this->parsed_events_.push_back(parsed_trace_event{
        .type = parsed_trace_event_type::vscode_document_sync_event,
        .vscode_document_sync_event =
            parsed_vscode_document_sync_event{
                .timestamp = timestamp,
                .document_id = r.u64_le(),
                .uri = read_utf16le_string(),
                .language_id = read_utf16le_string(),
                .content = read_utf16le_string(),
            },
    });
    break;

  case 0x06:
    this->parsed_events_.push_back(parsed_trace_event{
        .type = parsed_trace_event_type::lsp_client_to_server_message_event,
        .lsp_client_to_server_message_event =
            parsed_lsp_client_to_server_message_event{
                .timestamp = timestamp,
                .body = read_utf8_string(),
            },
    });
    break;

  case 0x07: {
    std::uint64_t entry_count = r.u64_le();
    std::vector<parsed_vector_max_size_histogram_by_owner_entry> entries;
    for (std::uint64_t i = 0; i < entry_count; ++i) {
      entries.emplace_back();
      parsed_vector_max_size_histogram_by_owner_entry& entry = entries.back();
      entry.owner = read_utf8_zstring();
      std::uint64_t max_size_entry_count = r.u64_le();
      for (std::uint64_t j = 0; j < max_size_entry_count; ++j) {
        entry.max_size_entries.push_back(parsed_vector_max_size_histogram_entry{
            .max_size = r.u64_le(),
            .count = r.u64_le(),
        });
      }
    }
    this->parsed_events_.push_back(parsed_trace_event{
        .type =
            parsed_trace_event_type::vector_max_size_histogram_by_owner_event,
        .vector_max_size_histogram_by_owner_event =
            parsed_vector_max_size_histogram_by_owner_event{
                .timestamp = timestamp,
                .entries = std::move(entries),
            },
    });
    break;
  }

  case 0x08: {
    std::uint64_t process_id = r.u64_le();
    this->parsed_events_.push_back(parsed_trace_event{
        .type = parsed_trace_event_type::process_id_event,
        .process_id_event =
            parsed_process_id_event{
                .timestamp = timestamp,
                .process_id = process_id,
            },
    });
    break;
  }

  default:
    // TODO(strager): Report an error.
    return;
  }
}

void trace_reader::on_error(parsed_trace_event_type error) {
  this->parsed_events_.push_back(parsed_trace_event{
      .type = error,
  });
  this->encountered_error_ = true;
}

bool parsed_vscode_document_change::operator==(
    const parsed_vscode_document_change& other) const noexcept {
  return this->range.start.line == other.range.start.line &&
         this->range.start.character == other.range.start.character &&
         this->range.end.line == other.range.end.line &&
         this->range.end.character == other.range.end.character &&
         this->range_offset == other.range_offset &&
         this->range_length == other.range_length && this->text == other.text;
}

bool parsed_vscode_document_change::operator!=(
    const parsed_vscode_document_change& other) const noexcept {
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
