#include <quick-lint-js/trace-metadata.h>

namespace quick_lint_js {
const char8 trace_metadata[] =
    u8R"(/* CTF 1.8 */
// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// This file is a Common Trace Format metadata file in the Trace Stream
// Description Language. https://diamon.org/ctf/
//
// This file describes the binary trace files produced by quick-lint-js.

typealias integer { size = 8;  align = 8; signed = false; byte_order = le; } := u8;
typealias integer { size = 16; align = 8; signed = false; byte_order = le; } := u16;
typealias integer { size = 32; align = 8; signed = false; byte_order = le; } := u32;
typealias integer { size = 64; align = 8; signed = false; byte_order = le; } := u64;

typealias string { encoding = utf8; } := utf8_string;

// Allows null code points.
typealias struct {
  u64 code_unit_count;
  u16 code_units[code_unit_count];
} := utf16le_string;

typealias struct {
  u64 byte_count;
  u8 bytes[byte_count];
} := utf8_string;

// 0 is a sentinal value meaning no document ID.
typealias u64 := document_id;

clock {
  name = monotonic_ns_clock;
  freq = 1000000000;
  absolute = false;
};
typealias integer {
  size = 64;
  align = 8;
  signed = false;
  byte_order = le;
  map = clock.monotonic_ns_clock.value;
} := monotonic_ns_timestamp;

trace {
  major = 1;
  minor = 8;
  uuid = "63697571-2d6b-495f-b93e-736a746e696c";
  byte_order = le;
  packet.header := struct {
    u32 magic;
    u8 uuid[16];
  };
};

stream {
  packet.context := struct {
    u64 thread_id;
    u8 compression_scheme;
  };
  event.header := struct {
    monotonic_ns_timestamp timestamp;
    u8 id;
  };
};

event {
  id = 1;
  name = "init";
  fields := struct {
    utf8_string quick_lint_js_version;
  };
};

event {
  id = 2;
  name = "vscode_document_opened";
  fields := struct {
    document_id doc_id;
    utf16le_string uri;
    utf16le_string language_id;
    utf16le_string content;
  };
};

event {
  id = 3;
  name = "vscode_document_closed";
  fields := struct {
    document_id doc_id;
    utf16le_string uri;
    utf16le_string language_id;
  };
};

// vscode.Position
typealias struct {
  u64 line;
  u64 character;
} := vscode_document_position;

// vscode.Range
typealias struct {
  vscode_document_position start;
  vscode_document_position end;
} := vscode_document_range;

// vscode.TextDocumentContentChangeEvent
typealias struct {
  vscode_document_range range;
  u64 range_offset;
  u64 range_length;
  utf16le_string text;
} := vscode_document_change;

// vscode.TextDocumentChangeEvent
event {
  id = 4;
  name = "vscode_document_changed";
  fields := struct {
    document_id doc_id; // Cannot be 0.
    u64 change_count;
    vscode_document_change changes[change_count];
  };
};

// Not related to any particular Visual Studio Code event.
event {
  id = 5;
  name = "vscode_document_sync";
  fields := struct {
    document_id doc_id; // Cannot be 0.
    utf16le_string uri;
    utf16le_string language_id;
    utf16le_string content;
  };
};

// An LSP message received by quick-lint-js.
event {
  id = 6;
  name = "lsp_client_to_server_message";
  fields := struct {
    // body is the JSON content only, excluding the header.
    utf8_string body;
  };
};

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
)";
}
