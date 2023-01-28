// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/lsp/lsp-endpoint.h>
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

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

namespace quick_lint_js {
lsp_endpoint_remote::~lsp_endpoint_remote() = default;

lsp_endpoint_handler::~lsp_endpoint_handler() = default;

lsp_endpoint::lsp_endpoint(lsp_endpoint_handler* handler,
                           lsp_endpoint_remote* remote)
    : remote_(remote),
      handler_(handler),
      json_parser_(std::make_unique< ::simdjson::ondemand::parser>()) {}

lsp_endpoint::~lsp_endpoint() = default;

void lsp_endpoint::message_parsed(string8_view message) {
  trace_writer* tw =
      trace_flusher::instance()->trace_writer_for_current_thread();
  if (tw) {
    tw->write_event_lsp_client_to_server_message(
        trace_event_lsp_client_to_server_message{
            .timestamp = 0,  // TODO(strager)
            .body = message,
        });
    tw->commit();
  }

  using namespace std::literals::string_view_literals;

  // TODO(strager): Avoid copying the message.
  ::simdjson::padded_string padded_message(
      reinterpret_cast<const char*>(message.data()), message.size());
  ::simdjson::ondemand::document message_document;
  ::simdjson::error_code parse_error;
  this->json_parser_->iterate(padded_message)
      .tie(message_document, parse_error);
  if (parse_error != ::simdjson::error_code::SUCCESS) {
    byte_buffer error_json;
    this->write_json_parse_error_response(error_json);
    this->remote_->send_message(std::move(error_json));
    return;
  }

  byte_buffer response_json;

  ::simdjson::ondemand::array batched_messages;
  bool is_batch_message =
      message_document.get(batched_messages) == ::simdjson::error_code::SUCCESS;
  if (is_batch_message) {
    this->write_json_batch_messages_not_supported_error(response_json);
  } else {
    ::simdjson::ondemand::object message_object;
    if (message_document.get(message_object) ==
        ::simdjson::error_code::SUCCESS) {
      this->handle_message(message_object, response_json);
    } else {
      this->write_json_parse_error_response(response_json);
    }
  }

  // TODO(strager): Remove this code.
  if (!response_json.empty()) {
    this->remote_->send_message(std::move(response_json));
  }
}

void lsp_endpoint::flush_error_responses(lsp_endpoint_remote& remote) {
  this->error_responses_.send(remote);
}

void lsp_endpoint::handle_message(::simdjson::ondemand::object& message,
                                  byte_buffer& response_json) {
  using namespace std::literals::string_view_literals;

  bool have_id;
  string8_view id_json;
  lsp_endpoint_handler::request_id_type int_id = 0;
  ::simdjson::error_code int_id_rc = ::simdjson::NO_SUCH_FIELD;

  ::simdjson::ondemand::value id;
  switch (message["id"].get(id)) {
  case ::simdjson::error_code::SUCCESS: {
    ::simdjson::ondemand::json_type id_type;
    if (id.type().get(id_type) != ::simdjson::error_code::SUCCESS) {
      this->write_json_parse_error_response(response_json);
      return;
    }
    switch (id_type) {
    case ::simdjson::ondemand::json_type::null:
    case ::simdjson::ondemand::json_type::number:
    case ::simdjson::ondemand::json_type::string:
      break;

    default:
      this->write_invalid_request_error_response(response_json);
      return;
    }

    id_json = get_raw_json(id);
    int_id_rc = id.get(int_id);

    have_id = true;
    break;
  }

  case ::simdjson::error_code::NO_SUCH_FIELD:
    have_id = false;
    break;

  case ::simdjson::error_code::TAPE_ERROR:
    this->write_json_parse_error_response(response_json);
    return;

  default:
    QLJS_UNIMPLEMENTED();
    return;
  }

  bool have_method;
  std::string_view method;
  switch (message["method"].get(method)) {
  case ::simdjson::error_code::SUCCESS:
    have_method = true;
    break;
  case ::simdjson::error_code::NO_SUCH_FIELD:
    have_method = false;
    break;
  default:
    this->write_invalid_request_error_response(response_json);
    return;
  }

  ::simdjson::ondemand::object error;
  ::simdjson::error_code error_rc = message["error"].get(error);
  bool have_error = error_rc != ::simdjson::NO_SUCH_FIELD;
  std::int64_t error_code = 0;
  std::string_view error_message;
  switch (error_rc) {
  case ::simdjson::SUCCESS:
    if (error["code"].get(error_code) != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response(response_json);
      return;
    }
    if (error["message"].get(error_message) != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response(response_json);
      return;
    }
    break;
  case ::simdjson::NO_SUCH_FIELD:
    break;
  default:
    this->write_invalid_request_error_response(response_json);
    return;
  }

  ::simdjson::ondemand::value result;
  ::simdjson::error_code result_rc = message["result"].get(result);
  bool have_result = result_rc != ::simdjson::NO_SUCH_FIELD;

  if (have_id && have_method && !have_error && !have_result) {
    this->handler_->handle_request(message, method, id_json);
  } else if (have_id && !have_method && have_error && !have_result) {
    if (int_id_rc != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response(response_json);
      return;
    }
    this->handler_->handle_error_response(int_id, error_code, error_message);
  } else if (have_id && !have_method && !have_error && have_result) {
    if (int_id_rc != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response(response_json);
      return;
    }
    if (result_rc != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response(response_json);
      return;
    }
    this->handler_->handle_response(int_id, result);
  } else if (!have_id && have_method && !have_error && !have_result) {
    this->handler_->handle_notification(message, method);
  } else {
    this->write_invalid_request_error_response(response_json);
  }
}

void lsp_endpoint::write_json_parse_error_response(byte_buffer&) {
  byte_buffer& response_json = this->error_responses_.new_message();
  using namespace std::literals::string_view_literals;
  // clang-format off
  response_json.append_copy(u8R"({)"
    u8R"("jsonrpc":"2.0",)"
    u8R"("id":null,)"
    u8R"("error":{)"
      u8R"("code":-32700,)"
      u8R"("message":"Parse error")"
    u8R"(})"
  u8R"(})"_sv);
  // clang-format on
}

void lsp_endpoint::write_json_batch_messages_not_supported_error(byte_buffer&) {
  byte_buffer& response_json = this->error_responses_.new_message();
  // clang-format off
  response_json.append_copy(u8R"({)"
    u8R"("jsonrpc":"2.0",)"
    u8R"("id":null,)"
    u8R"("error":{)"
      u8R"("code":-32700,)"
      u8R"("message":"Parse error: batch messages not supported")"
    u8R"(})"
  u8R"(})"_sv);
}

void lsp_endpoint::write_invalid_request_error_response(
    byte_buffer& ) {
  byte_buffer& response_json = this->error_responses_.new_message();
  using namespace std::literals::string_view_literals;
  // clang-format off
  response_json.append_copy(u8R"({)"
    u8R"("jsonrpc":"2.0",)"
    u8R"("id":null,)"
    u8R"("error":{)"
      u8R"("code":-32600,)"
      u8R"("message":"Invalid Request")"
    u8R"(})"
  u8R"(})"_sv);
  // clang-format on
}

template void lsp_message_parser<lsp_endpoint>::append(string8_view);
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
