// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_LSP_ENDPOINT_H
#define QUICK_LINT_JS_LSP_LSP_ENDPOINT_H

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
class json_rpc_message_handler {
 public:
  // The type of IDs used for requests sent by this handler (thus responses
  // handled by this handler).
  using request_id_type = std::uint64_t;

  virtual ~json_rpc_message_handler();

  // It is the responsibility of the json_rpc_message_handler to create and send
  // a response back to the peer.
  virtual void handle_request(::simdjson::ondemand::object& request,
                              std::string_view method,
                              string8_view id_json) = 0;

  virtual void handle_response(request_id_type request_id,
                               ::simdjson::ondemand::value& result) = 0;

  virtual void handle_error_response(request_id_type request_id,
                                     std::int64_t code,
                                     std::string_view message) = 0;

  virtual void handle_notification(::simdjson::ondemand::object& request,
                                   std::string_view method) = 0;
};

// An lsp_json_rpc_message_parser parses Language Server Protocol messages and
// dispatches them to json_rpc_message_handler.
//
// lsp_json_rpc_message_parser implements JSON-RPC.
class lsp_json_rpc_message_parser
    : private lsp_message_parser<lsp_json_rpc_message_parser> {
 private:
  using message_parser = lsp_message_parser<lsp_json_rpc_message_parser>;

 public:
  explicit lsp_json_rpc_message_parser(json_rpc_message_handler* handler);
  ~lsp_json_rpc_message_parser();

  lsp_json_rpc_message_parser(const lsp_json_rpc_message_parser&) = delete;
  lsp_json_rpc_message_parser& operator=(const lsp_json_rpc_message_parser&) =
      delete;

  using message_parser::append;

  void message_parsed(string8_view message);

  void flush_error_responses(lsp_endpoint_remote&);

 private:
  void handle_message(::simdjson::ondemand::object& request);

  void write_json_parse_error_response();
  void write_json_batch_messages_not_supported_error();

  void write_invalid_request_error_response();

  json_rpc_message_handler* handler_;
  std::unique_ptr< ::simdjson::ondemand::parser> json_parser_;
  outgoing_json_rpc_message_queue error_responses_;

  friend message_parser;
};

extern template void lsp_message_parser<lsp_json_rpc_message_parser>::append(
    string8_view);
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
