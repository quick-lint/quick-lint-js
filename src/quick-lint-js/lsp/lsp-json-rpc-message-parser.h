// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_LSP_JSON_RPC_MESSAGE_PARSER_H
#define QUICK_LINT_JS_LSP_LSP_JSON_RPC_MESSAGE_PARSER_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp/lsp-message-parser.h>
#include <quick-lint-js/lsp/outgoing-json-rpc-message-queue.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/simdjson-fwd.h>
#include <tuple>
#include <utility>
#include <vector>

namespace quick_lint_js {
// Receives JSON-RPC messages parsed by lsp_json_rpc_message_parser.
class JSON_RPC_Message_Handler {
 public:
  // The type of IDs used for requests sent by this handler (thus responses
  // handled by this handler).
  using Request_ID_Type = std::uint64_t;

  virtual ~JSON_RPC_Message_Handler();

  // It is the responsibility of the json_rpc_message_handler to create and send
  // a response back to the peer.
  virtual void handle_request(::simdjson::ondemand::object& request,
                              std::string_view method,
                              String8_View id_json) = 0;

  virtual void handle_response(Request_ID_Type request_id,
                               ::simdjson::ondemand::value& result) = 0;

  virtual void handle_error_response(Request_ID_Type request_id,
                                     std::int64_t code,
                                     std::string_view message) = 0;

  virtual void handle_notification(::simdjson::ondemand::object& request,
                                   std::string_view method) = 0;
};

// An lsp_json_rpc_message_parser parses Language Server Protocol messages and
// dispatches them to json_rpc_message_handler.
//
// lsp_json_rpc_message_parser implements JSON-RPC.
class LSP_JSON_RPC_Message_Parser
    : private LSP_Message_Parser<LSP_JSON_RPC_Message_Parser> {
 private:
  using Message_Parser = LSP_Message_Parser<LSP_JSON_RPC_Message_Parser>;

 public:
  explicit LSP_JSON_RPC_Message_Parser(JSON_RPC_Message_Handler* handler);
  ~LSP_JSON_RPC_Message_Parser();

  LSP_JSON_RPC_Message_Parser(const LSP_JSON_RPC_Message_Parser&) = delete;
  LSP_JSON_RPC_Message_Parser& operator=(const LSP_JSON_RPC_Message_Parser&) =
      delete;

  using Message_Parser::append;

  void message_parsed(String8_View message);

  void flush_error_responses(LSP_Endpoint_Remote&);

 private:
  void handle_message(::simdjson::ondemand::object& request);

  void write_json_parse_error_response();
  void write_json_batch_messages_not_supported_error();

  void write_invalid_request_error_response();

  JSON_RPC_Message_Handler* handler_;
  std::unique_ptr< ::simdjson::ondemand::parser> json_parser_;
  Outgoing_JSON_RPC_Message_Queue error_responses_;

  friend Message_Parser;
};

extern template void LSP_Message_Parser<LSP_JSON_RPC_Message_Parser>::append(
    String8_View);
}

#endif

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
