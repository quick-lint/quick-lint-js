// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/simdjson.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
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
    : remote_(remote), handler_(handler) {}

void lsp_endpoint::message_parsed(string8_view message) {
  using namespace std::literals::string_view_literals;

  // TODO(strager): Avoid copying the message.
  ::simdjson::padded_string padded_message(
      reinterpret_cast<const char*>(message.data()), message.size());
  ::simdjson::ondemand::document request_document;
  ::simdjson::error_code parse_error;
  this->json_parser_.iterate(padded_message).tie(request_document, parse_error);
  if (parse_error != ::simdjson::error_code::SUCCESS) {
    byte_buffer error_json;
    this->write_json_parse_error_response(error_json);
    this->remote_->send_message(std::move(error_json));
    return;
  }

  byte_buffer response_json;

  ::simdjson::ondemand::array batched_requests;
  bool is_batch_request =
      request_document.get(batched_requests) == ::simdjson::error_code::SUCCESS;
  if (is_batch_request) {
    response_json.append_copy(u8"["sv);
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
          response_json.append_copy(u8","sv);
        }
        this->write_json_parse_error_response(response_json);
      }
    }
    response_json.append_copy(u8"]"sv);
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
    this->remote_->send_message(std::move(response_json));
  }
}

void lsp_endpoint::handle_message(::simdjson::ondemand::object& request,
                                  byte_buffer& response_json,
                                  bool add_comma_before_response) {
  using namespace std::literals::string_view_literals;

  ::simdjson::ondemand::value id;
  switch (request["id"].get(id)) {
  case ::simdjson::error_code::SUCCESS: {
    if (add_comma_before_response) {
      response_json.append_copy(u8","sv);
    }
    std::string_view method;
    if (request["method"].get(method) != ::simdjson::error_code::SUCCESS) {
      this->write_invalid_request_error_response(response_json);
      return;
    }

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

    this->handler_->handle_request(request, method, get_raw_json(id),
                                   response_json);
    break;
  }

  case ::simdjson::error_code::NO_SUCH_FIELD: {
    std::string_view method;
    if (request["method"].get(method) != ::simdjson::error_code::SUCCESS) {
      this->write_invalid_request_error_response(response_json);
      break;
    }
    this->handler_->handle_notification(request, method);
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

void lsp_endpoint::write_json_parse_error_response(byte_buffer& response_json) {
  using namespace std::literals::string_view_literals;
  // clang-format off
  response_json.append_copy(u8R"({)"
    u8R"("jsonrpc":"2.0",)"
    u8R"("id":null,)"
    u8R"("error":{)"
      u8R"("code":-32700,)"
      u8R"("message":"Parse error")"
    u8R"(})"
  u8R"(})"sv);
  // clang-format on
}

void lsp_endpoint::write_invalid_request_error_response(
    byte_buffer& response_json) {
  using namespace std::literals::string_view_literals;
  // clang-format off
  response_json.append_copy(u8R"({)"
    u8R"("jsonrpc":"2.0",)"
    u8R"("id":null,)"
    u8R"("error":{)"
      u8R"("code":-32600,)"
      u8R"("message":"Invalid Request")"
    u8R"(})"
  u8R"(})"sv);
  // clang-format on
}
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
