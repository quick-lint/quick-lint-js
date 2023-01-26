// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_LSP_ENDPOINT_H
#define QUICK_LINT_JS_LSP_LSP_ENDPOINT_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp/lsp-message-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/simdjson.h>
#include <simdjson.h>
#include <tuple>
#include <utility>
#include <vector>

namespace quick_lint_js {
class lsp_endpoint_remote {
 public:
  virtual ~lsp_endpoint_remote();

  virtual void send_message(byte_buffer&& message) = 0;
};

class lsp_endpoint_handler {
 public:
  // The type of IDs used for requests sent by this handler (thus responses
  // handled by this handler).
  using request_id_type = std::uint64_t;

  virtual ~lsp_endpoint_handler();

  virtual void handle_request(::simdjson::ondemand::object& request,
                              std::string_view method, string8_view id_json,
                              byte_buffer& reply) = 0;
  virtual void handle_response(request_id_type request_id,
                               ::simdjson::ondemand::value& result) = 0;
  virtual void handle_error_response(request_id_type request_id,
                                     std::int64_t code,
                                     std::string_view message) = 0;
  virtual void handle_notification(::simdjson::ondemand::object& request,
                                   std::string_view method) = 0;
};

// An lsp_endpoint parses Language Server Protocol messages, dispatches them to
// lsp_endpoint_handler, and sends responses to lsp_endpoint_remote.
//
// lsp_endpoint implements JSON-RPC.
class lsp_endpoint : private lsp_message_parser<lsp_endpoint> {
 private:
  using message_parser = lsp_message_parser<lsp_endpoint>;

 public:
  // TODO(strager): It would be nice if we didn't have to pass an
  // lsp_endpoint_remote* here. It makes tests uglier and control flow
  // non-obvious. Some options I considered:
  //
  // * lsp_endpoint stores an outgoing_lsp_message_queue and exposes a function:
  //   void lsp_endpoint::flush_outgoing_messages(lsp_endpoint_remote*);
  //   * This means that both lsp_endpoint and linting_lsp_server_handler have
  //     flush functions which need to be called, which is probably confusing.
  //
  // * The above option, and additionally a reference to the
  //   outgoing_lsp_message_queue object is given to lsp_endpoint_handler.
  //   * Asynchronous events (e.g. filesystem notifications) can cause
  //     asynchronous messages from linting_lsp_server_handler, so
  //     linting_lsp_server_handler still needs its own
  //     outgoing_lsp_message_queue. Therefore, the two-flush-function issue
  //     isn't solved.
  //   * Tests are even harder to write than they already are.
  //
  // * lsp_endpoint accepts an outgoing_lsp_message_queue* instead of an
  //   lsp_endpoint_remote*.
  //   * This runs the risk of someone else using the outgoing_lsp_message_queue
  //     while we're working with it. Perhaps this can be solved by making
  //     outgoing_lsp_message_queue safer to use.
  //   * Tests are slightly harder to write because outgoing_lsp_message_queue
  //     isn't polymorphic. However, we could add test utilities directly to
  //     outgoing_lsp_message_queue to paper over this, because
  //     outgoing_lsp_message_queue now buffers messages like
  //     spy_lsp_endpoint_remote does.
  //
  // * lsp_endpoint sends responses by calling a new function:
  //   void lsp_endpoint_handler::send_message_to_client_later(byte_buffer&);
  //   * From this class' perspective, this new design effectively merges
  //     lsp_endpoint_remote and lsp_endpoint_handler.
  //   * This centralizes the outgoing_lsp_message_queue inside each
  //     lsp_endpoint_handler. This means flushing is more consistent.
  //   * From a design perspective, it's weird that lsp_endpoint_handler is
  //     responsible for publicly holding the outgoing_lsp_message_queue. But
  //     this design quirk already exists.
  //   * I don't know whether tests would change for the worse or for the
  //     better.
  //
  // * Drop support for batched requests and responses, and remove the
  //   reply parameter from lsp_endpoint_handler::handle_request.
  //   * In practice, no LSP client sends batched requests anyway.
  //     * TODO(strager): Codify this in the LSP spec.
  //       https://github.com/microsoft/language-server-protocol/pull/1651
  //   * This gives lsp_endpoint_handler full responsibility in sending
  //     responses.
  //     * This means flushing is more consistent.
  //     * This means lsp_endpoint_handler implementations need more code.
  //   * I don't know whether tests would change for the worse or for the
  //     better.
  explicit lsp_endpoint(lsp_endpoint_handler* handler,
                        lsp_endpoint_remote* remote);

  using message_parser::append;

  void message_parsed(string8_view message);

 private:
  void handle_message(::simdjson::ondemand::object& request,
                      byte_buffer& response_json,
                      bool add_comma_before_response);

  void write_json_parse_error_response(byte_buffer& response_json);

  static void write_invalid_request_error_response(byte_buffer& response_json);

  lsp_endpoint_remote* remote_;
  lsp_endpoint_handler* handler_;
  // TODO(strager): Make this a pointer so we can avoid #include-ing
  // <simdjson.h>.
  ::simdjson::ondemand::parser json_parser_;

  friend message_parser;
};

extern template void lsp_message_parser<lsp_endpoint>::append(string8_view);
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
