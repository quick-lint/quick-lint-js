// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <csetjmp>
#include <cstddef>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/binary-reader.h>
#include <vector>

namespace quick_lint_js {
Trace_Reader::Trace_Reader() = default;

Trace_Reader::~Trace_Reader() = default;

void Trace_Reader::append_bytes(const void* data, std::size_t data_size) {
  const std::uint8_t* new_bytes = reinterpret_cast<const std::uint8_t*>(data);
  this->queue_.insert(this->queue_.end(), new_bytes, new_bytes + data_size);
}

std::vector<Parsed_Trace_Event> Trace_Reader::pull_new_events() {
  std::vector<Parsed_Trace_Event> result;
  this->pull_new_events(result);
  return result;
}

void Trace_Reader::pull_new_events(std::vector<Parsed_Trace_Event>& out) {
  // Delete already-parsed data from this->queue_. We can't do this after
  // parsing because the parsed event objects point to memory inside
  // this->queue_.
  {
    std::vector<std::uint8_t>::iterator begin = this->queue_.begin();
    std::vector<std::uint8_t>::iterator end =
        begin + narrow_cast<std::ptrdiff_t>(this->parsed_bytes_);
#if defined(QLJS_DEBUG) && QLJS_DEBUG
    std::fill(begin, end, 'x');
#endif
    this->queue_.erase(begin, end);
  }

  std::jmp_buf buf;
  auto unexpected_end_of_file = [&buf]() {
    std::longjmp(buf, 1);
    QLJS_UNREACHABLE();
  };
  Checked_Binary_Reader r(this->queue_.data(), this->queue_.size(),
                          unexpected_end_of_file);

  const std::uint8_t* volatile committed = r.cursor();
  if (setjmp(buf) == 0) {
    while (!r.eof() && !this->encountered_error_) {
      this->parse_one(r);
      committed = r.cursor();
    }
  } else {
    // End of file prematurely reached.
  }
  this->parsed_bytes_ =
      narrow_cast<std::size_t>(committed - this->queue_.data());

  out.insert(out.end(), this->parsed_events_.begin(),
             this->parsed_events_.end());
  this->parsed_events_.clear();
}

void Trace_Reader::parse_one(Checked_Binary_Reader& r) {
  if (this->parsed_header_) {
    this->parse_event(r);
  } else {
    this->parse_header(r);
  }
}

void Trace_Reader::parse_header(Checked_Binary_Reader& r) {
  QLJS_ASSERT(!this->parsed_header_);

  std::uint32_t magic = r.u32_le();
  if (magic != 0xc1fc1fc1) {
    this->on_error(Parsed_Trace_Event_Type::error_invalid_magic);
    return;
  }

  static constexpr std::uint8_t quick_lint_js_uuid[] = {
      0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,
      0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,
  };
  const std::uint8_t* uuid = r.advance(std::size(quick_lint_js_uuid));
  if (!std::equal(std::begin(quick_lint_js_uuid), std::end(quick_lint_js_uuid),
                  uuid)) {
    this->on_error(Parsed_Trace_Event_Type::error_invalid_uuid);
    return;
  }

  std::uint64_t thread_id = r.u64_le();

  std::uint8_t compression_mode = r.u8();
  if (compression_mode != 0) {
    this->on_error(Parsed_Trace_Event_Type::error_unsupported_compression_mode);
    return;
  }

  this->parsed_events_.push_back(Parsed_Trace_Event{
      .type = Parsed_Trace_Event_Type::packet_header,
      // TODO(strager): Don't initialize .header. (Move it inside the union?)
      .header =
          Trace_Event_Header{
              .timestamp = 0,
          },
      .packet_header =
          Trace_Context{
              .thread_id = thread_id,
          },
  });
  this->parsed_header_ = true;
}

void Trace_Reader::parse_event(Checked_Binary_Reader& r) {
  auto read_utf16le_string_in_place = [&r]() -> std::u16string_view {
    std::uint64_t length = r.u64_le();
    const std::uint8_t* bytes = r.advance(length * 2);
    static_assert(sizeof(char16_t) == 2 * sizeof(std::uint8_t));
    // TODO(strager): This assumes the native endian is little endian.
    return std::u16string_view(reinterpret_cast<const char16_t*>(bytes),
                               length);
  };
  auto read_utf8_string_in_place = [&r]() -> String8_View {
    std::uint64_t length = r.u64_le();
    const std::uint8_t* bytes = r.advance(length);
    static_assert(sizeof(Char8) == sizeof(std::uint8_t));
    return String8_View(reinterpret_cast<const Char8*>(bytes), length);
  };
  auto read_utf8_zstring_in_place = [&r]() -> String8_View {
    const std::uint8_t* bytes = r.cursor();
    r.find_and_skip_byte(0x00);
    const std::uint8_t* end = r.cursor() - 1;
    return String8_View(reinterpret_cast<const Char8*>(bytes),
                        narrow_cast<std::size_t>(end - bytes));
  };
  auto read_lsp_document_type = [&]() -> Trace_LSP_Document_Type {
    std::uint8_t raw_type = r.u8();
    if (raw_type > static_cast<std::uint8_t>(last_trace_lsp_document_type)) {
      this->on_error(
          Parsed_Trace_Event_Type::error_unsupported_lsp_document_type);
      return Trace_LSP_Document_Type::unknown;
    }
    return static_cast<Trace_LSP_Document_Type>(raw_type);
  };

  Trace_Event_Header header = {
      .timestamp = r.u64_le(),
  };
  std::uint8_t event_id = r.u8();
  switch (event_id) {
  case 0x01: {
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type = Parsed_Trace_Event_Type::init_event,
        .header = header,
        .init_event =
            Trace_Event_Init{
                .version = read_utf8_zstring_in_place(),
            },
    });
    break;
  }

  case 0x02:
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type = Parsed_Trace_Event_Type::vscode_document_opened_event,
        .header = header,
        .vscode_document_opened_event =
            Trace_Event_VSCode_Document_Opened<std::u16string_view>{
                .document_id = r.u64_le(),
                .uri = read_utf16le_string_in_place(),
                .language_id = read_utf16le_string_in_place(),
                .content = read_utf16le_string_in_place(),
            },
    });
    break;

  case 0x03:
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type = Parsed_Trace_Event_Type::vscode_document_closed_event,
        .header = header,
        .vscode_document_closed_event =
            Trace_Event_VSCode_Document_Closed<std::u16string_view>{
                .document_id = r.u64_le(),
                .uri = read_utf16le_string_in_place(),
                .language_id = read_utf16le_string_in_place(),
            },
    });
    break;

  case 0x04: {
    std::uint64_t document_id = r.u64_le();
    std::uint64_t change_count = r.u64_le();
    Span<Trace_VSCode_Document_Change<std::u16string_view>> changes =
        this->memory_
            .allocate_span<Trace_VSCode_Document_Change<std::u16string_view>>(
                narrow_cast<std::size_t>(change_count));
    for (Span_Size i = 0; i < changes.size(); ++i) {
      changes[i] = Trace_VSCode_Document_Change<std::u16string_view>{
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
          .text = read_utf16le_string_in_place(),
      };
    }
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type = Parsed_Trace_Event_Type::vscode_document_changed_event,
        .header = header,
        .vscode_document_changed_event =
            Trace_Event_VSCode_Document_Changed<std::u16string_view>{
                .document_id = document_id,
                .changes = changes,
            },
    });
    break;
  }

  case 0x05:
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type = Parsed_Trace_Event_Type::vscode_document_sync_event,
        .header = header,
        .vscode_document_sync_event =
            Trace_Event_VSCode_Document_Sync<std::u16string_view>{
                .document_id = r.u64_le(),
                .uri = read_utf16le_string_in_place(),
                .language_id = read_utf16le_string_in_place(),
                .content = read_utf16le_string_in_place(),
            },
    });
    break;

  case 0x06:
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type = Parsed_Trace_Event_Type::lsp_client_to_server_message_event,
        .header = header,
        .lsp_client_to_server_message_event =
            Trace_Event_LSP_Client_To_Server_Message{
                .body = read_utf8_string_in_place(),
            },
    });
    break;

  case 0x07: {
    std::uint64_t entry_count = r.u64_le();
    Span<Trace_Vector_Max_Size_Histogram_By_Owner_Entry> entries =
        this->memory_
            .allocate_span<Trace_Vector_Max_Size_Histogram_By_Owner_Entry>(
                narrow_cast<std::size_t>(entry_count));
    for (Span_Size i = 0; i < entries.size(); ++i) {
      Trace_Vector_Max_Size_Histogram_By_Owner_Entry& entry = entries[i];
      entry.owner = to_string_view(read_utf8_zstring_in_place());

      std::uint64_t max_size_entry_count = r.u64_le();
      Span<Trace_Vector_Max_Size_Histogram_Entry> max_size_entries =
          this->memory_.allocate_span<Trace_Vector_Max_Size_Histogram_Entry>(
              narrow_cast<std::size_t>(max_size_entry_count));
      for (Span_Size j = 0; j < max_size_entries.size(); ++j) {
        max_size_entries[j] = Trace_Vector_Max_Size_Histogram_Entry{
            .max_size = r.u64_le(),
            .count = r.u64_le(),
        };
      }
      entry.max_size_entries = max_size_entries;
    }
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type =
            Parsed_Trace_Event_Type::vector_max_size_histogram_by_owner_event,
        .header = header,
        .vector_max_size_histogram_by_owner_event =
            Trace_Event_Vector_Max_Size_Histogram_By_Owner{
                .entries = entries,
            },
    });
    break;
  }

  case 0x08: {
    std::uint64_t process_id = r.u64_le();
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type = Parsed_Trace_Event_Type::process_id_event,
        .header = header,
        .process_id_event =
            Trace_Event_Process_ID{
                .process_id = process_id,
            },
    });
    break;
  }

  case 0x09: {
    std::uint64_t document_count = r.u64_le();
    Span<Trace_LSP_Document_State> documents =
        this->memory_.allocate_span<Trace_LSP_Document_State>(
            narrow_cast<std::size_t>(document_count));
    for (Span_Size i = 0; i < documents.size(); ++i) {
      documents[i] = Trace_LSP_Document_State{
          .type = read_lsp_document_type(),
          .uri = read_utf8_string_in_place(),
          .text = read_utf8_string_in_place(),
          .language_id = read_utf8_string_in_place(),
      };
    }
    this->parsed_events_.push_back(Parsed_Trace_Event{
        .type = Parsed_Trace_Event_Type::lsp_documents_event,
        .header = header,
        .lsp_documents_event =
            Trace_Event_LSP_Documents{
                .documents = documents,
            },
    });
    break;
  }

  default:
    // TODO(strager): Report an error.
    return;
  }
}

void Trace_Reader::on_error(Parsed_Trace_Event_Type error) {
  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")
  this->parsed_events_.push_back(Parsed_Trace_Event{
      .type = error,
      // Don't initialize any union member.
  });
  QLJS_WARNING_POP
  this->encountered_error_ = true;
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
