// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRACE_WRITER_H
#define QUICK_LINT_JS_TRACE_WRITER_H

#include <cstdint>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/async-byte-queue.h>
#include <quick-lint-js/binary-writer.h>
#include <quick-lint-js/char8.h>

namespace quick_lint_js {
struct trace_context {
  std::uint64_t thread_id;
};

struct trace_event_init {
  static constexpr std::uint8_t id = 0x01;

  std::uint64_t timestamp;
  string8_view version;
};

struct trace_event_vscode_document_opened {
  static constexpr std::uint8_t id = 0x02;

  std::uint64_t timestamp;
  std::uint64_t document_id;
  void* uri;
  void* language_id;
  void* content;
};

struct trace_event_vscode_document_closed {
  static constexpr std::uint8_t id = 0x03;

  std::uint64_t timestamp;
  std::uint64_t document_id;
  void* uri;
  void* language_id;
};

struct trace_vscode_document_position {
  std::uint64_t line;
  std::uint64_t character;
};

struct trace_vscode_document_range {
  trace_vscode_document_position start;
  trace_vscode_document_position end;
};

struct trace_vscode_document_change {
  trace_vscode_document_range range;
  std::uint64_t range_offset;
  std::uint64_t range_length;
  void* text;
};

struct trace_event_vscode_document_changed {
  static constexpr std::uint8_t id = 0x04;

  std::uint64_t timestamp;
  std::uint64_t document_id;
  const trace_vscode_document_change* changes;
  std::uint64_t change_count;
};

struct trace_event_vscode_document_sync {
  static constexpr std::uint8_t id = 0x05;

  std::uint64_t timestamp;
  std::uint64_t document_id;
  void* uri;
  void* language_id;
  void* content;
};

struct trace_event_lsp_client_to_server_message {
  static constexpr std::uint8_t id = 0x06;

  std::uint64_t timestamp;
  string8_view body;
};

class trace_writer {
 public:
  explicit trace_writer(async_byte_queue*);

  // Calls async_byte_queue::commit.
  void commit();

  void write_header(const trace_context&);

  void write_event_init(const trace_event_init&);

  template <class StringWriter>
  void write_event_vscode_document_opened(
      const trace_event_vscode_document_opened&, StringWriter&&);

  template <class StringWriter>
  void write_event_vscode_document_closed(
      const trace_event_vscode_document_closed&, StringWriter&&);

  template <class StringWriter>
  void write_event_vscode_document_changed(
      const trace_event_vscode_document_changed&, StringWriter&&);

  template <class StringWriter>
  void write_event_vscode_document_sync(const trace_event_vscode_document_sync&,
                                        StringWriter&&);

  void write_event_lsp_client_to_server_message(
      const trace_event_lsp_client_to_server_message&);

 private:
  template <class Func>
  void append_binary(async_byte_queue::size_type size, Func&& callback);

  template <class StringWriter>
  void write_utf16le_string(void* string, StringWriter&);

  async_byte_queue* out_;
};

template <class StringWriter>
void trace_writer::write_event_vscode_document_opened(
    const trace_event_vscode_document_opened& event,
    StringWriter&& string_writer) {
  this->append_binary(8 + 1 + 8, [&](binary_writer& w) {
    w.u64_le(event.timestamp);
    w.u8(event.id);
    w.u64_le(event.document_id);
  });
  this->write_utf16le_string(event.uri, string_writer);
  this->write_utf16le_string(event.language_id, string_writer);
  this->write_utf16le_string(event.content, string_writer);
}

template <class StringWriter>
void trace_writer::write_event_vscode_document_closed(
    const trace_event_vscode_document_closed& event,
    StringWriter&& string_writer) {
  this->append_binary(8 + 1 + 8, [&](binary_writer& w) {
    w.u64_le(event.timestamp);
    w.u8(event.id);
    w.u64_le(event.document_id);
  });
  this->write_utf16le_string(event.uri, string_writer);
  this->write_utf16le_string(event.language_id, string_writer);
}

template <class StringWriter>
void trace_writer::write_event_vscode_document_changed(
    const trace_event_vscode_document_changed& event,
    StringWriter&& string_writer) {
  this->append_binary(8 + 1 + 8 + 8, [&](binary_writer& w) {
    w.u64_le(event.timestamp);
    w.u8(event.id);
    w.u64_le(event.document_id);
    w.u64_le(event.change_count);
  });
  for (std::uint64_t i = 0; i < event.change_count; ++i) {
    const trace_vscode_document_change* change = &event.changes[i];
    this->append_binary(8 * 6, [&](binary_writer& w) {
      w.u64_le(change->range.start.line);
      w.u64_le(change->range.start.character);
      w.u64_le(change->range.end.line);
      w.u64_le(change->range.end.character);
      w.u64_le(change->range_offset);
      w.u64_le(change->range_length);
    });
    this->write_utf16le_string(change->text, string_writer);
  }
}

template <class Func>
void trace_writer::append_binary(async_byte_queue::size_type size,
                                 Func&& callback) {
  std::uint8_t* data_begin =
      reinterpret_cast<std::uint8_t*>(this->out_->append(size));
  binary_writer w(data_begin);
  callback(w);
  QLJS_ASSERT(w.bytes_written_since(data_begin) == size);
}

template <class StringWriter>
void trace_writer::write_utf16le_string(void* string,
                                        StringWriter& string_writer) {
  std::size_t code_unit_count = string_writer.string_size(string);
  // HACK(strager): Reserve an extra code unit for a null terminator. This is
  // required when interacting with N-API in the Visual Studio Code extension.
  std::size_t capacity = code_unit_count + 1;
  this->append_binary(8, [&](binary_writer& w) { w.u64_le(code_unit_count); });
  this->out_->append_aligned(
      capacity * sizeof(char16_t), alignof(char16_t), [&](void* data) {
        string_writer.copy_string(string, reinterpret_cast<char16_t*>(data),
                                  capacity);
        return code_unit_count * sizeof(char16_t);
      });
}

template <class StringWriter>
void trace_writer::write_event_vscode_document_sync(
    const trace_event_vscode_document_sync& event,
    StringWriter&& string_writer) {
  this->append_binary(8 + 1 + 8, [&](binary_writer& w) {
    w.u64_le(event.timestamp);
    w.u8(event.id);
    w.u64_le(event.document_id);
  });
  this->write_utf16le_string(event.uri, string_writer);
  this->write_utf16le_string(event.language_id, string_writer);
  this->write_utf16le_string(event.content, string_writer);
}
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
