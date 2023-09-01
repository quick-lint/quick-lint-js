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
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
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
JSON_RPC_Message_Handler::~JSON_RPC_Message_Handler() = default;

LSP_JSON_RPC_Message_Parser::LSP_JSON_RPC_Message_Parser(
    JSON_RPC_Message_Handler* handler)
    : handler_(handler),
      json_parser_(std::make_unique<::simdjson::ondemand::parser>()) {}

LSP_JSON_RPC_Message_Parser::~LSP_JSON_RPC_Message_Parser() = default;

void LSP_JSON_RPC_Message_Parser::message_parsed(String8_View message) {
  Trace_Writer* tw =
      Trace_Flusher::instance()->trace_writer_for_current_thread();
  if (tw) {
    tw->write_event_lsp_client_to_server_message(
        Trace_Event_Header{.timestamp = 0},  // TODO(strager)
        Trace_Event_LSP_Client_To_Server_Message{
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
  if (parse_error != ::simdjson::SUCCESS) {
    this->write_json_parse_error_response();
    return;
  }

  ::simdjson::ondemand::array batched_messages;
  bool is_batch_message =
      message_document.get(batched_messages) == ::simdjson::SUCCESS;
  if (is_batch_message) {
    this->write_json_batch_messages_not_supported_error();
  } else {
    ::simdjson::ondemand::object message_object;
    if (message_document.get(message_object) == ::simdjson::SUCCESS) {
      this->handle_message(message_object);
    } else {
      this->write_json_parse_error_response();
    }
  }
}

void LSP_JSON_RPC_Message_Parser::flush_error_responses(
    LSP_Endpoint_Remote& remote) {
  this->error_responses_.send(remote);
}

void LSP_JSON_RPC_Message_Parser::handle_message(
    ::simdjson::ondemand::object& message) {
  using namespace std::literals::string_view_literals;

  bool have_id;
  String8_View id_json;
  JSON_RPC_Message_Handler::Request_ID_Type int_id = 0;
  ::simdjson::error_code int_id_rc = ::simdjson::NO_SUCH_FIELD;

  ::simdjson::ondemand::value id;
  switch (message["id"].get(id)) {
  case ::simdjson::error_code::SUCCESS: {
    ::simdjson::ondemand::json_type id_type;
    if (id.type().get(id_type) != ::simdjson::SUCCESS) {
      this->write_json_parse_error_response();
      return;
    }
    switch (id_type) {
    case ::simdjson::ondemand::json_type::null:
    case ::simdjson::ondemand::json_type::number:
    case ::simdjson::ondemand::json_type::string:
      break;

    default:
      this->write_invalid_request_error_response();
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
    this->write_json_parse_error_response();
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
    this->write_invalid_request_error_response();
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
      this->write_invalid_request_error_response();
      return;
    }
    if (error["message"].get(error_message) != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response();
      return;
    }
    break;
  case ::simdjson::NO_SUCH_FIELD:
    break;
  default:
    this->write_invalid_request_error_response();
    return;
  }

  ::simdjson::ondemand::value result;
  ::simdjson::error_code result_rc = message["result"].get(result);
  bool have_result = result_rc != ::simdjson::NO_SUCH_FIELD;

  if (have_id && have_method && !have_error && !have_result) {
    this->handler_->handle_request(message, method, id_json);
  } else if (have_id && !have_method && have_error && !have_result) {
    if (int_id_rc != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response();
      return;
    }
    this->handler_->handle_error_response(int_id, error_code, error_message);
  } else if (have_id && !have_method && !have_error && have_result) {
    if (int_id_rc != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response();
      return;
    }
    if (result_rc != ::simdjson::SUCCESS) {
      this->write_invalid_request_error_response();
      return;
    }
    this->handler_->handle_response(int_id, result);
  } else if (!have_id && have_method && !have_error && !have_result) {
    this->handler_->handle_notification(message, method);
  } else {
    this->write_invalid_request_error_response();
  }
}

void LSP_JSON_RPC_Message_Parser::write_json_parse_error_response() {
  Byte_Buffer& response_json = this->error_responses_.new_message();
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

void LSP_JSON_RPC_Message_Parser::
    write_json_batch_messages_not_supported_error() {
  Byte_Buffer& response_json = this->error_responses_.new_message();
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

void LSP_JSON_RPC_Message_Parser::write_invalid_request_error_response() {
  Byte_Buffer& response_json = this->error_responses_.new_message();
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

template void LSP_Message_Parser<LSP_JSON_RPC_Message_Parser>::append(
    String8_View);
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
