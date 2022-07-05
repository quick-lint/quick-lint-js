// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRACE_STREAM_READER_MOCK_H
#define QUICK_LINT_JS_TRACE_STREAM_READER_MOCK_H

#include <gmock/gmock.h>
#include <quick-lint-js/trace-stream-reader.h>

namespace quick_lint_js {
class mock_trace_stream_event_visitor : public trace_stream_event_visitor {
 public:
  MOCK_METHOD(void, visit_error_invalid_magic, (), (override));
  MOCK_METHOD(void, visit_error_invalid_uuid, (), (override));
  MOCK_METHOD(void, visit_error_unsupported_compression_mode, (std::uint8_t),
              (override));

  MOCK_METHOD(void, visit_packet_header, (const packet_header&), (override));

  MOCK_METHOD(void, visit_init_event, (const init_event&), (override));
  MOCK_METHOD(void, visit_vscode_document_opened_event,
              (const vscode_document_opened_event&), (override));
  MOCK_METHOD(void, visit_vscode_document_changed_event,
              (const vscode_document_changed_event&), (override));
  MOCK_METHOD(void, visit_vscode_document_closed_event,
              (const vscode_document_closed_event&), (override));
  MOCK_METHOD(void, visit_vscode_document_sync_event,
              (const vscode_document_sync_event&), (override));
  MOCK_METHOD(void, visit_lsp_client_to_server_message_event,
              (const lsp_client_to_server_message_event&), (override));
};

using nice_mock_trace_stream_event_visitor =
    ::testing::NiceMock<mock_trace_stream_event_visitor>;
using strict_mock_trace_stream_event_visitor =
    ::testing::StrictMock<mock_trace_stream_event_visitor>;
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
