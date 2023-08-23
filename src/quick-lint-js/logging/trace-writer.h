// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/binary-writer.h>
#include <string_view>

namespace quick_lint_js {
struct Trace_Context {
  std::uint64_t thread_id;
};

struct Trace_Event_Init {
  static constexpr std::uint8_t id = 0x01;

  std::uint64_t timestamp;
  String8_View version;
};

template <class String>
struct Trace_Event_VSCode_Document_Opened {
  static constexpr std::uint8_t id = 0x02;

  std::uint64_t timestamp;
  std::uint64_t document_id;
  String uri;
  String language_id;
  String content;
};

template <class String>
struct Trace_Event_VSCode_Document_Closed {
  static constexpr std::uint8_t id = 0x03;

  std::uint64_t timestamp;
  std::uint64_t document_id;
  String uri;
  String language_id;
};

struct Trace_VSCode_Document_Position {
  std::uint64_t line;
  std::uint64_t character;
};

struct Trace_VSCode_Document_Range {
  Trace_VSCode_Document_Position start;
  Trace_VSCode_Document_Position end;
};

template <class String>
struct Trace_VSCode_Document_Change {
  Trace_VSCode_Document_Range range;
  std::uint64_t range_offset;
  std::uint64_t range_length;
  String text;

  // For testing.
  friend bool operator==(const Trace_VSCode_Document_Change& lhs,
                         const Trace_VSCode_Document_Change& rhs) {
    return lhs.range.start.line == rhs.range.start.line &&
           lhs.range.start.character == rhs.range.start.character &&
           lhs.range.end.line == rhs.range.end.line &&
           lhs.range.end.character == rhs.range.end.character &&
           lhs.range_offset == rhs.range_offset &&
           lhs.range_length == rhs.range_length && lhs.text == rhs.text;
  }

  friend bool operator!=(const Trace_VSCode_Document_Change& lhs,
                         const Trace_VSCode_Document_Change& rhs) {
    return !(lhs == rhs);
  }
};

template <class String>
struct Trace_Event_VSCode_Document_Changed {
  static constexpr std::uint8_t id = 0x04;

  std::uint64_t timestamp;
  std::uint64_t document_id;
  Span<const Trace_VSCode_Document_Change<String>> changes;
};

template <class String>
struct Trace_Event_VSCode_Document_Sync {
  static constexpr std::uint8_t id = 0x05;

  std::uint64_t timestamp;
  std::uint64_t document_id;
  String uri;
  String language_id;
  String content;
};

struct Trace_Event_LSP_Client_To_Server_Message {
  static constexpr std::uint8_t id = 0x06;

  std::uint64_t timestamp;
  String8_View body;
};

struct Trace_Vector_Max_Size_Histogram_Entry {
  std::uint64_t max_size;
  std::uint64_t count;

  friend bool operator==(Trace_Vector_Max_Size_Histogram_Entry,
                         Trace_Vector_Max_Size_Histogram_Entry);
  friend bool operator!=(Trace_Vector_Max_Size_Histogram_Entry,
                         Trace_Vector_Max_Size_Histogram_Entry);
};

struct Trace_Vector_Max_Size_Histogram_By_Owner_Entry {
  std::string_view owner;
  Span<const Trace_Vector_Max_Size_Histogram_Entry> max_size_entries;

  friend bool operator==(const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&,
                         const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&);
  friend bool operator!=(const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&,
                         const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&);
};

struct Trace_Event_Vector_Max_Size_Histogram_By_Owner {
  static constexpr std::uint8_t id = 0x07;

  std::uint64_t timestamp;
  Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry> entries;
};

struct Trace_Event_Process_ID {
  static constexpr std::uint8_t id = 0x08;

  std::uint64_t timestamp;
  std::uint64_t process_id;
};

enum class Trace_LSP_Document_Type : std::uint8_t {
  unknown = 0,
  config = 1,
  lintable = 2,
};
inline constexpr Trace_LSP_Document_Type last_trace_lsp_document_type =
    Trace_LSP_Document_Type::lintable;

struct Trace_LSP_Document_State {
  Trace_LSP_Document_Type type;
  String8_View uri;
  String8_View text;
  String8_View language_id;
};

struct Trace_Event_LSP_Documents {
  static constexpr std::uint8_t id = 0x09;

  std::uint64_t timestamp;
  Span<const Trace_LSP_Document_State> documents;
};

// Specialize this class to define writing for different string types.
//
// See Trace_String_Writer<std::u16string_view> for an example specialization.
template <class String>
struct Trace_String_Writer;

template <>
struct Trace_String_Writer<std::u16string_view> {
  static std::size_t string_size_u16(std::u16string_view string) {
    return string.size();
  }

  static void copy_string_u16(std::u16string_view string, char16_t* out,
                              std::size_t capacity) {
    QLJS_ASSERT(capacity >= string.size());
    std::copy(string.begin(), string.end(), out);
  }
};

class Trace_Writer {
 public:
  explicit Trace_Writer(Async_Byte_Queue*);

  // Calls async_byte_queue::commit.
  void commit();

  void write_header(const Trace_Context&);

  void write_event_init(const Trace_Event_Init&);

  template <class String>
  void write_event_vscode_document_opened(
      const Trace_Event_VSCode_Document_Opened<String>&);

  template <class String>
  void write_event_vscode_document_closed(
      const Trace_Event_VSCode_Document_Closed<String>&);

  template <class String>
  void write_event_vscode_document_changed(
      const Trace_Event_VSCode_Document_Changed<String>&);

  template <class String>
  void write_event_vscode_document_sync(
      const Trace_Event_VSCode_Document_Sync<String>&);

  void write_event_lsp_client_to_server_message(
      const Trace_Event_LSP_Client_To_Server_Message&);

  void write_event_vector_max_size_histogram_by_owner(
      const Trace_Event_Vector_Max_Size_Histogram_By_Owner&);

  void write_event_process_id(const Trace_Event_Process_ID&);

  void write_event_lsp_documents(const Trace_Event_LSP_Documents&);

 private:
  template <class Func>
  void append_binary(Async_Byte_Queue::Size_Type size, Func&& callback);

  template <class String>
  void write_utf16le_string(String string);

  void write_utf8_string(String8_View);

  Async_Byte_Queue* out_;
};

template <class String>
void Trace_Writer::write_event_vscode_document_opened(
    const Trace_Event_VSCode_Document_Opened<String>& event) {
  this->append_binary(8 + 1 + 8, [&](Binary_Writer& w) {
    w.u64_le(event.timestamp);
    w.u8(event.id);
    w.u64_le(event.document_id);
  });
  this->write_utf16le_string(event.uri);
  this->write_utf16le_string(event.language_id);
  this->write_utf16le_string(event.content);
}

template <class String>
void Trace_Writer::write_event_vscode_document_closed(
    const Trace_Event_VSCode_Document_Closed<String>& event) {
  this->append_binary(8 + 1 + 8, [&](Binary_Writer& w) {
    w.u64_le(event.timestamp);
    w.u8(event.id);
    w.u64_le(event.document_id);
  });
  this->write_utf16le_string(event.uri);
  this->write_utf16le_string(event.language_id);
}

template <class String>
void Trace_Writer::write_event_vscode_document_changed(
    const Trace_Event_VSCode_Document_Changed<String>& event) {
  this->append_binary(8 + 1 + 8 + 8, [&](Binary_Writer& w) {
    w.u64_le(event.timestamp);
    w.u8(event.id);
    w.u64_le(event.document_id);
    w.u64_le(narrow_cast<std::uint64_t>(event.changes.size()));
  });
  for (const Trace_VSCode_Document_Change<String>& change : event.changes) {
    this->append_binary(8 * 6, [&](Binary_Writer& w) {
      w.u64_le(change.range.start.line);
      w.u64_le(change.range.start.character);
      w.u64_le(change.range.end.line);
      w.u64_le(change.range.end.character);
      w.u64_le(change.range_offset);
      w.u64_le(change.range_length);
    });
    this->write_utf16le_string(change.text);
  }
}

template <class Func>
void Trace_Writer::append_binary(Async_Byte_Queue::Size_Type size,
                                 Func&& callback) {
  std::uint8_t* data_begin =
      reinterpret_cast<std::uint8_t*>(this->out_->append(size));
  Binary_Writer w(data_begin);
  callback(w);
  QLJS_ASSERT(w.bytes_written_since(data_begin) == size);
}

template <class String>
void Trace_Writer::write_utf16le_string(String string) {
  using String_Writer = Trace_String_Writer<String>;
  std::size_t code_unit_count = String_Writer::string_size_u16(string);
  // HACK(strager): Reserve an extra code unit for a null terminator. This is
  // required when interacting with N-API in the Visual Studio Code extension.
  std::size_t capacity = code_unit_count + 1;
  this->append_binary(8, [&](Binary_Writer& w) { w.u64_le(code_unit_count); });
  this->out_->append_aligned(
      capacity * sizeof(char16_t), alignof(char16_t), [&](void* data) {
        String_Writer::copy_string_u16(
            string, reinterpret_cast<char16_t*>(data), capacity);
        return code_unit_count * sizeof(char16_t);
      });
}

template <class String>
void Trace_Writer::write_event_vscode_document_sync(
    const Trace_Event_VSCode_Document_Sync<String>& event) {
  this->append_binary(8 + 1 + 8, [&](Binary_Writer& w) {
    w.u64_le(event.timestamp);
    w.u8(event.id);
    w.u64_le(event.document_id);
  });
  this->write_utf16le_string(event.uri);
  this->write_utf16le_string(event.language_id);
  this->write_utf16le_string(event.content);
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
