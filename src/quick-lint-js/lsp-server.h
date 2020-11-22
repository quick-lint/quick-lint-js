// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_LSP_SERVER_H
#define QUICK_LINT_JS_LSP_SERVER_H

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <simdjson.h>
#include <vector>

namespace quick_lint_js {
class byte_buffer;

// A linting_lsp_server_handler listens for JavaScript code changes and notifies
// the client of diagnostics.
class linting_lsp_server_handler {
 public:
  void handle_request(::simdjson::dom::element& request,
                      byte_buffer& response_json);
  void handle_notification(::simdjson::dom::element& request,
                           byte_buffer& notification_json);

 private:
  void handle_initialize_request(::simdjson::dom::element& request,
                                 byte_buffer& response_json);

  void handle_text_document_did_change_notification(
      ::simdjson::dom::element& request, byte_buffer& notification_json);
  void handle_text_document_did_close_notification(
      ::simdjson::dom::element& request);
  void handle_text_document_did_open_notification(
      ::simdjson::dom::element& request, byte_buffer& notification_json);

  void lint_and_get_diagnostics_notification(
      padded_string_view code, ::simdjson::dom::element& text_document,
      byte_buffer& notification_json);

  void lint_and_get_diagnostics(padded_string_view code,
                                byte_buffer& diagnostics_json);

  static padded_string make_padded_string(
      const ::simdjson::simdjson_result<::simdjson::dom::element>& string);

  std::vector<string8> lintable_uris_;
};
}

#endif
