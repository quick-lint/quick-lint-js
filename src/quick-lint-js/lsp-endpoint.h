// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_ENDPOINT_H
#define QUICK_LINT_JS_LSP_ENDPOINT_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/unreachable.h>
#include <simdjson.h>
#include <tuple>
#include <utility>
#include <vector>

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
concept lsp_endpoint_remote = requires(Remote r, byte_buffer message) {
  {r.send_message(std::move(message))};
};

template <class Handler>
concept lsp_endpoint_handler =
    requires(Handler h, ::simdjson::ondemand::object request,
             std::string_view method, string8_view id_json, byte_buffer reply,
             void (*write_notification_json)(byte_buffer&&)) {
  {h.handle_request(request, method, id_json, reply)};
  {h.handle_notification(request, method)};
  {h.take_pending_notification_jsons(write_notification_json)};
};
#endif

// An lsp_endpoint parses Language Server Protocol messages, dispatches them to
// Handler, and sends responses to Remote.
//
// lsp_endpoint implements JSON-RPC.
template <QLJS_LSP_ENDPOINT_HANDLER Handler, QLJS_LSP_ENDPOINT_REMOTE Remote>
class lsp_endpoint
    : private lsp_message_parser<lsp_endpoint<Handler, Remote> > {
 private:
  using message_parser = lsp_message_parser<lsp_endpoint<Handler, Remote> >;

 public:
  explicit lsp_endpoint() {}

  template <class... HandlerArgs, class... RemoteArgs>
  explicit lsp_endpoint(const std::tuple<HandlerArgs...>& handler_args,
                        const std::tuple<RemoteArgs...>& remote_args)
      : lsp_endpoint(handler_args, std::index_sequence_for<HandlerArgs...>(),
                     remote_args, std::index_sequence_for<RemoteArgs...>()) {}

  template <class... HandlerArgs, std::size_t... HandlerArgsI,
            class... RemoteArgs, std::size_t... RemoteArgsI>
  explicit lsp_endpoint(const std::tuple<HandlerArgs...>& handler_args,
                        std::index_sequence<HandlerArgsI...>,
                        const std::tuple<RemoteArgs...>& remote_args,
                        std::index_sequence<RemoteArgsI...>)
      : remote_(std::get<RemoteArgsI>(remote_args)...),
        handler_(std::get<HandlerArgsI>(handler_args)...) {}

  using message_parser::append;

  Handler& handler() noexcept { return this->handler_; }
  Remote& remote() noexcept { return this->remote_; }

  void flush_pending_notifications() {
    this->handler_.take_pending_notification_jsons(
        [&](byte_buffer&& notification_json) {
          if (notification_json.empty()) {
            // TODO(strager): Fix our tests so they don't make empty
            // byte_buffer-s.
            return;
          }
          this->remote_.send_message(std::move(notification_json));
        });
  }

  void message_parsed(string8_view message) {
    // TODO(strager): Avoid copying the message.
    ::simdjson::padded_string padded_message(
        reinterpret_cast<const char*>(message.data()), message.size());
    ::simdjson::ondemand::document request_document;
    ::simdjson::error_code parse_error;
    this->json_parser_.iterate(padded_message)
        .tie(request_document, parse_error);
    if (parse_error != ::simdjson::error_code::SUCCESS) {
      byte_buffer error_json;
      this->write_json_parse_error_response(error_json);
      this->remote_.send_message(std::move(error_json));
      return;
    }

    byte_buffer response_json;

    ::simdjson::ondemand::array batched_requests;
    bool is_batch_request = request_document.get(batched_requests) ==
                            ::simdjson::error_code::SUCCESS;
    if (is_batch_request) {
      response_json.append_copy(u8"[");
      std::size_t empty_response_json_size = response_json.size();
      for (::simdjson::simdjson_result< ::simdjson::ondemand::value>
               sub_request_or_error : batched_requests) {
        ::simdjson::ondemand::object sub_request;
        if (sub_request_or_error.get(sub_request) ==
            ::simdjson::error_code::SUCCESS) {
          this->handle_message(
              sub_request, response_json,
              /*add_comma_before_response=*/response_json.size() !=
                  empty_response_json_size);
        } else {
          if (response_json.size() != empty_response_json_size) {
            response_json.append_copy(u8",");
          }
          this->write_json_parse_error_response(response_json);
        }
      }
      response_json.append_copy(u8"]");
    } else {
      ::simdjson::ondemand::object request;
      if (request_document.get(request) == ::simdjson::error_code::SUCCESS) {
        this->handle_message(request, response_json,
                             /*add_comma_before_response=*/false);
      } else {
        this->write_json_parse_error_response(response_json);
      }
    }

    if (is_batch_request) {
      // Batch requests require batch responses.
      QLJS_ASSERT(!response_json.empty());
    }

    if (!response_json.empty()) {
      this->remote_.send_message(std::move(response_json));
    }
    this->flush_pending_notifications();
  }

 private:
  void handle_message(::simdjson::ondemand::object& request,
                      byte_buffer& response_json,
                      bool add_comma_before_response) {
    ::simdjson::ondemand::value id;
    switch (request["id"].get(id)) {
    case ::simdjson::error_code::SUCCESS: {
      if (add_comma_before_response) {
        response_json.append_copy(u8",");
      }
      std::string_view method;
      if (request["method"].get(method) != ::simdjson::error_code::SUCCESS) {
        this->write_invalid_request_error_response(response_json);
        break;
      }
      this->handler_.handle_request(request, method, get_raw_json(id),
                                    response_json);
      break;
    }

    case ::simdjson::error_code::NO_SUCH_FIELD: {
      std::string_view method;
      if (request["method"].get(method) != ::simdjson::error_code::SUCCESS) {
        this->write_invalid_request_error_response(response_json);
        break;
      }
      this->handler_.handle_notification(request, method);
      break;
    }

    case ::simdjson::error_code::TAPE_ERROR:
      this->write_json_parse_error_response(response_json);
      break;

    default:
      QLJS_UNIMPLEMENTED();
      break;
    }
  }

  void write_json_parse_error_response(byte_buffer& response_json) {
    // clang-format off
    response_json.append_copy(u8R"({)"
      u8R"("jsonrpc":"2.0",)"
      u8R"("id":null,)"
      u8R"("error":{)"
        u8R"("code":-32700,)"
        u8R"("message":"Parse error")"
      u8R"(})"
    u8R"(})");
    // clang-format on
  }

  static void write_invalid_request_error_response(byte_buffer& response_json) {
    // clang-format off
    response_json.append_copy(u8R"({)"
      u8R"("jsonrpc":"2.0",)"
      u8R"("id":null,)"
      u8R"("error":{)"
        u8R"("code":-32600,)"
        u8R"("message":"Invalid Request")"
      u8R"(})"
    u8R"(})");
    // clang-format on
  }

  static string8_view get_raw_json(::simdjson::ondemand::value& value) {
    ::simdjson::ondemand::json_type type;
    if (value.type().get(type) != ::simdjson::error_code::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }
    switch (type) {
    case ::simdjson::ondemand::json_type::boolean:
    case ::simdjson::ondemand::json_type::null:
    case ::simdjson::ondemand::json_type::number:
    case ::simdjson::ondemand::json_type::string:
      return to_string8_view(value.raw_json_token());

    case ::simdjson::ondemand::json_type::array:
    case ::simdjson::ondemand::json_type::object:
      QLJS_UNIMPLEMENTED();
    }
    QLJS_UNREACHABLE();
  }

  Remote remote_;
  Handler handler_;
  ::simdjson::ondemand::parser json_parser_;

  friend message_parser;
};
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
