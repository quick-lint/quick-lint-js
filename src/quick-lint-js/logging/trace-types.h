// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdint>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wunknown-attributes")
QLJS_WARNING_IGNORE_GCC("-Wattributes")

namespace quick_lint_js {
struct Trace_Context {
  std::uint64_t thread_id;
};

struct Trace_Event_Header {
  std::uint64_t timestamp;
};

// 0 is a sentinal value meaning no document ID.
using Trace_Document_ID = std::uint64_t;

struct Trace_Event_Init {
  static constexpr std::uint8_t id = 0x01;

  [[qljs::trace_zero_terminated]]  //
  String8_View version;
};

template <class String16>
struct Trace_Event_VSCode_Document_Opened {
  static constexpr std::uint8_t id = 0x02;

  Trace_Document_ID document_id;
  String16 uri;
  String16 language_id;
  String16 content;
};

template <class String16>
struct Trace_Event_VSCode_Document_Closed {
  static constexpr std::uint8_t id = 0x03;

  Trace_Document_ID document_id;
  String16 uri;
  String16 language_id;
};

// vscode.Position
struct Trace_VSCode_Document_Position {
  std::uint64_t line;
  std::uint64_t character;
};

// vscode.Range
struct Trace_VSCode_Document_Range {
  Trace_VSCode_Document_Position start;
  Trace_VSCode_Document_Position end;
};

// vscode.TextDocumentContentChangeEvent
template <class String16>
struct Trace_VSCode_Document_Change {
  Trace_VSCode_Document_Range range;
  std::uint64_t range_offset;
  std::uint64_t range_length;
  String16 text;

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

// vscode.TextDocumentChangeEvent
template <class String16>
struct Trace_Event_VSCode_Document_Changed {
  static constexpr std::uint8_t id = 0x04;

  Trace_Document_ID document_id;                 // Cannot be 0.
  [[qljs::trace_ctf_size_name("change_count")]]  //
  Span<const Trace_VSCode_Document_Change<String16>>
      changes;
};

// Not related to any particular Visual Studio Code event.
template <class String16>
struct Trace_Event_VSCode_Document_Sync {
  static constexpr std::uint8_t id = 0x05;

  Trace_Document_ID document_id;  // Cannot be 0.
  String16 uri;
  String16 language_id;
  String16 content;
};

// An LSP message received by quick-lint-js.
struct Trace_Event_LSP_Client_To_Server_Message {
  static constexpr std::uint8_t id = 0x06;

  // body is the JSON content only, excluding the header.
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
  [[qljs::trace_zero_terminated]]  //
  String8_View owner;
  [[qljs::trace_ctf_size_name("max_size_entry_count")]]  //
  Span<const Trace_Vector_Max_Size_Histogram_Entry>
      max_size_entries;

  friend bool operator==(const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&,
                         const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&);
  friend bool operator!=(const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&,
                         const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&);
};

struct Trace_Event_Vector_Max_Size_Histogram_By_Owner {
  static constexpr std::uint8_t id = 0x07;

  [[qljs::trace_ctf_size_name("entry_count")]]  //
  Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>
      entries;
};

struct Trace_Event_Process_ID {
  static constexpr std::uint8_t id = 0x08;

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
  // TODO(strager): String8_View version_json;
  // TODO(strager): Lint settings.
};

struct Trace_Event_LSP_Documents {
  static constexpr std::uint8_t id = 0x09;

  [[qljs::trace_ctf_size_name("document_count")]]  //
  Span<const Trace_LSP_Document_State>
      documents;
};
}

QLJS_WARNING_POP

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
