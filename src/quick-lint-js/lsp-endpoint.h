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

#ifndef QUICK_LINT_JS_LSP_ENDPOINT_H
#define QUICK_LINT_JS_LSP_ENDPOINT_H

#include <cstddef>
#include <json/value.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-message-parser.h>

#if QLJS_HAVE_CXX_CONCEPTS
#define QLJS_LSP_ENDPOINT_HANDLER ::quick_lint_js::lsp_endpoint_handler
#define QLJS_LSP_ENDPOINT_REMOTE ::quick_lint_js::lsp_endpoint_remote
#else
#define QLJS_LSP_ENDPOINT_HANDLER class
#define QLJS_LSP_ENDPOINT_REMOTE class
#endif

namespace quick_lint_js {
#if QLJS_HAVE_CXX_CONCEPTS
template <class Remote>
concept lsp_endpoint_remote = requires(Remote r, string8_view message) {
  {r.send_message(message)};
};

template <class Handler>
concept lsp_endpoint_handler = requires(Handler h, const char8* raw_message,
                                        ::Json::Value request, string8 reply) {
  {h.handle_request(raw_message, request, reply)};
  {h.handle_notification(raw_message, request, reply)};
};
#endif

// An lsp_endpoint parses Language Server Protocol messages, dispatches them to
// Handler, and sends responses to Remote.
//
// lsp_endpoint implements JSON-RPC.
template <QLJS_LSP_ENDPOINT_HANDLER Handler, QLJS_LSP_ENDPOINT_REMOTE Remote>
class lsp_endpoint : private lsp_message_parser<lsp_endpoint<Handler, Remote>> {
 private:
  using message_parser = lsp_message_parser<lsp_endpoint<Handler, Remote>>;

 public:
  template <class... Args>
  explicit lsp_endpoint(Args&&... client_args)
      : remote_(std::forward<Args>(client_args)...) {}

  using message_parser::append;

  Remote& remote() noexcept { return this->remote_; }

 private:
  void message_parsed(string8_view message) {
    ::Json::Value request;
    ::Json::String errors;
    bool ok = parse_json(message, &request, &errors);
    if (!ok) {
      QLJS_UNIMPLEMENTED();
    }

    string8 response_json;
    string8 notification_json;
    bool is_batch_request = request.isArray();
    if (is_batch_request) {
      response_json += u8'[';
      std::size_t empty_response_json_size = response_json.size();
      for (::Json::Value& sub_request : request) {
        this->handle_message(
            message.data(), sub_request, response_json, notification_json,
            /*add_comma_before_response=*/response_json.size() !=
                empty_response_json_size);
      }
      response_json += u8']';
    } else {
      this->handle_message(message.data(), request, response_json,
                           notification_json,
                           /*add_comma_before_response=*/false);
    }

    if (is_batch_request) {
      // Batch requests require batch responses.
      QLJS_ASSERT(!response_json.empty());
    }

    if (!response_json.empty()) {
      this->remote_.send_message(response_json);
    }
    if (!notification_json.empty()) {
      this->remote_.send_message(notification_json);
    }
  }

  void handle_message(const char8* message_begin, ::Json::Value& request,
                      string8& response_json, string8& notification_json,
                      bool add_comma_before_response) {
    if (request.isMember("id")) {
      if (add_comma_before_response) {
        response_json += u8',';
      }
      this->handler_.handle_request(message_begin, request, response_json);
    } else {
      this->handler_.handle_notification(message_begin, request,
                                         notification_json);
    }
  }

  Remote remote_;
  Handler handler_;

  friend message_parser;
};
}

#endif
