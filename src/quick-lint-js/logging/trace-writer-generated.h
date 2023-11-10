// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// Code generated by tools/generate-trace-sources.cpp. DO NOT EDIT.
// source: src/quick-lint-js/logging/trace-types.h

#pragma once

#include <cstdint>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/logging/trace-types.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/binary-writer.h>
#include <quick-lint-js/util/cast.h>
#include <string_view>

// clang-format off
namespace quick_lint_js {
class Trace_Writer {
 public:
  explicit Trace_Writer(Async_Byte_Queue*);

  // Calls Async_Byte_Queue::commit.
  void commit();

  void write_header(const Trace_Context&);

  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_Init&);
  template <class String16>
  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_VSCode_Document_Opened<String16>&);
  template <class String16>
  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_VSCode_Document_Closed<String16>&);
  template <class String16>
  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_VSCode_Document_Changed<String16>&);
  template <class String16>
  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_VSCode_Document_Sync<String16>&);
  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_LSP_Client_To_Server_Message&);
  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_Vector_Max_Size_Histogram_By_Owner&);
  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_Process_ID&);
  void write_event(
      const Trace_Event_Header&,
      const Trace_Event_LSP_Documents&);

 private:
  void write_Trace_VSCode_Document_Position(
      const Trace_VSCode_Document_Position&);
  void write_Trace_VSCode_Document_Range(
      const Trace_VSCode_Document_Range&);
  template <class String16>
  void write_Trace_VSCode_Document_Change(
      const Trace_VSCode_Document_Change<String16>&);
  void write_Trace_Vector_Max_Size_Histogram_Entry(
      const Trace_Vector_Max_Size_Histogram_Entry&);
  void write_Trace_Vector_Max_Size_Histogram_By_Owner_Entry(
      const Trace_Vector_Max_Size_Histogram_By_Owner_Entry&);
  void write_Trace_LSP_Document_State(
      const Trace_LSP_Document_State&);

  template <class Func>
  void append_binary(Async_Byte_Queue::Size_Type size, Func&& callback);

  template <class String>
  void write_utf16le_string(String string);

  void write_utf8_string(String8_View);
  void write_utf8_zstring(String8_View);

  Async_Byte_Queue* out_;
};

inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_Init& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(9)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  /* Done with w. */
  this->write_utf8_zstring(s.version);
}

template <class String16>
inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_VSCode_Document_Opened<String16>& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(17)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  w.u64_le(s.document_id);
  /* Done with w. */
  this->write_utf16le_string(s.uri);
  this->write_utf16le_string(s.language_id);
  this->write_utf16le_string(s.content);
}

template <class String16>
inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_VSCode_Document_Closed<String16>& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(17)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  w.u64_le(s.document_id);
  /* Done with w. */
  this->write_utf16le_string(s.uri);
  this->write_utf16le_string(s.language_id);
}

inline void Trace_Writer::write_Trace_VSCode_Document_Position(
      const Trace_VSCode_Document_Position& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(16)));
  w.u64_le(s.line);
  w.u64_le(s.character);
  /* Done with w. */
}

inline void Trace_Writer::write_Trace_VSCode_Document_Range(
      const Trace_VSCode_Document_Range& s) {
  this->write_Trace_VSCode_Document_Position(s.start);
  this->write_Trace_VSCode_Document_Position(s.end);
}

template <class String16>
inline void Trace_Writer::write_Trace_VSCode_Document_Change(
      const Trace_VSCode_Document_Change<String16>& s) {
  this->write_Trace_VSCode_Document_Range(s.range);
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(16)));
  w.u64_le(s.range_offset);
  w.u64_le(s.range_length);
  /* Done with w. */
  this->write_utf16le_string(s.text);
}

template <class String16>
inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_VSCode_Document_Changed<String16>& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(25)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  w.u64_le(s.document_id);
  w.u64_le(narrow_cast<std::uint64_t>(s.changes.size()));
  /* Done with w. */
for (const Trace_VSCode_Document_Change<String16>& item : s.changes) {
  this->write_Trace_VSCode_Document_Change(item);
}
}

template <class String16>
inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_VSCode_Document_Sync<String16>& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(17)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  w.u64_le(s.document_id);
  /* Done with w. */
  this->write_utf16le_string(s.uri);
  this->write_utf16le_string(s.language_id);
  this->write_utf16le_string(s.content);
}

inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_LSP_Client_To_Server_Message& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(9)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  /* Done with w. */
  this->write_utf8_string(s.body);
}

inline void Trace_Writer::write_Trace_Vector_Max_Size_Histogram_Entry(
      const Trace_Vector_Max_Size_Histogram_Entry& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(16)));
  w.u64_le(s.max_size);
  w.u64_le(s.count);
  /* Done with w. */
}

inline void Trace_Writer::write_Trace_Vector_Max_Size_Histogram_By_Owner_Entry(
      const Trace_Vector_Max_Size_Histogram_By_Owner_Entry& s) {
  this->write_utf8_zstring(s.owner);
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(8)));
  w.u64_le(narrow_cast<std::uint64_t>(s.max_size_entries.size()));
  /* Done with w. */
for (const Trace_Vector_Max_Size_Histogram_Entry& item : s.max_size_entries) {
  this->write_Trace_Vector_Max_Size_Histogram_Entry(item);
}
}

inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_Vector_Max_Size_Histogram_By_Owner& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(17)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  w.u64_le(narrow_cast<std::uint64_t>(s.entries.size()));
  /* Done with w. */
for (const Trace_Vector_Max_Size_Histogram_By_Owner_Entry& item : s.entries) {
  this->write_Trace_Vector_Max_Size_Histogram_By_Owner_Entry(item);
}
}

inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_Process_ID& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(17)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  w.u64_le(s.process_id);
  /* Done with w. */
}

inline void Trace_Writer::write_Trace_LSP_Document_State(
      const Trace_LSP_Document_State& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(1)));
  w.u8(enum_to_int_cast(s.type));
  /* Done with w. */
  this->write_utf8_string(s.uri);
  this->write_utf8_string(s.text);
  this->write_utf8_string(s.language_id);
}

inline void Trace_Writer::write_event(
      const Trace_Event_Header& header,
      const Trace_Event_LSP_Documents& s) {
  Binary_Writer w = Binary_Writer(reinterpret_cast<std::uint8_t*>(this->out_->append(17)));
  w.u64_le(header.timestamp);
  w.u8(s.id);
  w.u64_le(narrow_cast<std::uint64_t>(s.documents.size()));
  /* Done with w. */
for (const Trace_LSP_Document_State& item : s.documents) {
  this->write_Trace_LSP_Document_State(item);
}
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
