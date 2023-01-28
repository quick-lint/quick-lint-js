// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <boost/json/serialize.hpp>
#include <boost/json/value.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-support.h>
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/spy-lsp-endpoint-remote.h>
#include <simdjson.h>

QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")

using ::testing::IsEmpty;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
// Fails the test if any not-overridden method is called.
struct test_json_rpc_message_handler : public json_rpc_message_handler {
  void handle_request(::simdjson::ondemand::object&, std::string_view,
                      string8_view) override {
    ADD_FAILURE() << "handle_request should not be called";
  }

  void handle_response(json_rpc_message_handler::request_id_type,
                       ::simdjson::ondemand::value&) override {
    ADD_FAILURE() << "handle_response should not be called";
  }

  void handle_error_response(json_rpc_message_handler::request_id_type,
                             std::int64_t, std::string_view) override {
    ADD_FAILURE() << "handle_error_response should not be called";
  }

  void handle_notification(::simdjson::ondemand::object&,
                           std::string_view) override {
    ADD_FAILURE() << "handle_notification should not be called";
  }
};

string8 make_message(string8_view content) {
  return string8(u8"Content-Length: ") +
         to_string8(std::to_string(content.size())) + u8"\r\n\r\n" +
         string8(content);
}

std::string json_get_string(
    ::simdjson::simdjson_result< ::simdjson::ondemand::value>&& value) {
  std::string_view s = "<not found>";
  EXPECT_EQ(value.get(s), ::simdjson::error_code::SUCCESS);
  return std::string(s);
}

void expect_batch_not_supported_error(::boost::json::value& message) {
  expect_error(message, -32700, "Parse error: batch messages not supported");
  EXPECT_EQ(look_up(message, "id"), ::boost::json::value());
}

TEST(test_lsp_json_rpc_message_parser, single_request) {
  struct mock_lsp_server_handler : public test_json_rpc_message_handler {
    void handle_request(::simdjson::ondemand::object& request,
                        std::string_view method,
                        string8_view id_json) override {
      EXPECT_EQ(json_get_string(request["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");
      EXPECT_EQ(id_json, u8"3");

      this->handle_request_called = true;
    }

    bool handle_request_called = false;
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "testmethod",
        "params": {}
      })"_sv));

  EXPECT_TRUE(handler.handle_request_called);
}

TEST(test_lsp_json_rpc_message_parser, batched_request_is_not_supported) {
  test_json_rpc_message_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"([
        {
          "jsonrpc": "2.0",
          "id": 3,
          "method": "testmethod A",
          "params": {}
        }, {
          "jsonrpc": "2.0",
          "id": 4,
          "method": "testmethod B",
          "params": {}
        }
      ])"_sv));
  server.flush_error_responses(remote);

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_TRUE(remote.messages[0].is_object())
      << "server should not respond with a batch response (array)";
  expect_batch_not_supported_error(remote.messages[0]);
}

TEST(test_lsp_json_rpc_message_parser, successful_response) {
  struct mock_lsp_server_handler : public test_json_rpc_message_handler {
    void handle_response(json_rpc_message_handler::request_id_type request_id,
                         ::simdjson::ondemand::value& result) override {
      this->response_handled = true;

      EXPECT_EQ(request_id, 3);

      std::uint64_t params_key;
      ASSERT_EQ(result["key"].get(params_key), ::simdjson::SUCCESS);
      EXPECT_EQ(params_key, 42);
    }

    bool response_handled = false;
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "result": {"key": 42}
      })"_sv));
  server.flush_error_responses(remote);

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_TRUE(handler.response_handled);
}

// This test is the same as successful_response, but it queries the given
// simdjson::ondemand::value-s in a different way. This test shakes out bugs
// caused by violating simdjson's strict usage pattern requirements.
TEST(test_lsp_json_rpc_message_parser, successful_response_v2) {
  struct mock_lsp_server_handler : public test_json_rpc_message_handler {
    void handle_response(json_rpc_message_handler::request_id_type request_id,
                         ::simdjson::ondemand::value& result) override {
      this->response_handled = true;

      EXPECT_EQ(request_id, 3);

      ::simdjson::ondemand::object params_object;
      ASSERT_EQ(result.get(params_object), ::simdjson::SUCCESS);
      std::uint64_t params_key;
      ASSERT_EQ(params_object["key"].get(params_key), ::simdjson::SUCCESS);
      EXPECT_EQ(params_key, 42);
    }

    bool response_handled = false;
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "result": {"key": 42}
      })"_sv));
  server.flush_error_responses(remote);

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_TRUE(handler.response_handled);
}

TEST(test_lsp_json_rpc_message_parser, error_response) {
  struct mock_lsp_server_handler : public test_json_rpc_message_handler {
    void handle_error_response(
        json_rpc_message_handler::request_id_type request_id, std::int64_t code,
        std::string_view message) override {
      this->error_response_handled = true;
      EXPECT_EQ(request_id, 3);
      EXPECT_EQ(code, 6969);
      EXPECT_EQ(message, "test error message");
    }

    bool error_response_handled = false;
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "error": {
          "code": 6969,
          "message": "test error message"
        }
      })"_sv));
  server.flush_error_responses(remote);

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_TRUE(handler.error_response_handled);
}

TEST(test_lsp_json_rpc_message_parser, batched_responses_are_not_supported) {
  test_json_rpc_message_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"([
        {
          "jsonrpc": "2.0",
          "id": 3,
          "result": "yay"
        }, {
          "jsonrpc": "2.0",
          "id": 4,
          "error": {
            "code": 6969,
            "message": "test error message"
          }
        }
      ])"_sv));
  server.flush_error_responses(remote);

  EXPECT_TRUE(remote.messages[0].is_object())
      << "server should not respond with a batch response (array)";
  expect_batch_not_supported_error(remote.messages[0]);
}

TEST(test_lsp_json_rpc_message_parser, single_notification_with_no_reply) {
  static int handle_notification_count;
  handle_notification_count = 0;

  struct mock_lsp_server_handler : public test_json_rpc_message_handler {
    void handle_notification(::simdjson::ondemand::object& notification,
                             std::string_view method) override {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");
      handle_notification_count += 1;
    }
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      })"_sv));
  server.flush_error_responses(remote);

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_EQ(handle_notification_count, 1);
}

TEST(test_lsp_json_rpc_message_parser, single_notification_with_reply) {
  struct mock_lsp_server_handler : public test_json_rpc_message_handler {
    void handle_notification(::simdjson::ondemand::object& notification,
                             std::string_view method) override {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");

      ::boost::json::value reply = {
          {"jsonrpc", "2.0"},
          {"method", "testreply"},
          {"params", "testparams"},
      };
      this->pending_notifications.push_back(reply);
    }

    std::vector< ::boost::json::value> pending_notifications;
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      })"_sv));
  server.flush_error_responses(remote);

  EXPECT_THAT(remote.messages, IsEmpty());
  ASSERT_EQ(handler.pending_notifications.size(), 1);
  EXPECT_EQ(look_up(handler.pending_notifications[0], "method"), "testreply");
  EXPECT_EQ(look_up(handler.pending_notifications[0], "params"), "testparams");
}

TEST(test_lsp_json_rpc_message_parser, batched_notification_is_not_supported) {
  test_json_rpc_message_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(
      make_message(u8R"([{
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      }])"_sv));
  server.flush_error_responses(remote);

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_TRUE(remote.messages[0].is_object())
      << "server should not respond with a batch response (array)";
  expect_batch_not_supported_error(remote.messages[0]);
}

// https://www.jsonrpc.org/specification#error_object
TEST(test_lsp_json_rpc_message_parser, malformed_json) {
  test_json_rpc_message_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_json_rpc_message_parser server(&handler);

  server.append(make_message(u8"{ malformed json! }"_sv));
  server.flush_error_responses(remote);

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_EQ(look_up(remote.messages[0], "id"), ::boost::json::value());
  expect_parse_error(remote.messages[0]);
}

TEST(test_lsp_json_rpc_message_parser, invalid_message) {
  for (
      string8_view message : {
          // request with missing method
          u8R"({ "jsonrpc": "2.0", "id": 10, "params": {} })"_sv,
          // request with method type mismatch
          u8R"({ "jsonrpc": "2.0", "method": 10, "id": 10 })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": 10, "id": 10, "params": {} })"_sv,
          // request with id type mismatch
          u8R"({ "jsonrpc": "2.0", "method": "mymethod", "id": true, "params": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "mymethod", "id": [], "params": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "mymethod", "id": {}, "params": {} })"_sv,
          // successful response with missing id
          u8R"({ "jsonrpc": "2.0", "result": {} })"_sv,
          // successful response with id type mismatch
          u8R"({ "jsonrpc": "2.0", "id": true, "result": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": [], "result": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": {}, "result": {} })"_sv,
          // error response with missing id
          u8R"({ "jsonrpc": "2.0", "error": {"code": 0, "message": ""} })"_sv,
          // error response with id type mismatch
          u8R"({ "jsonrpc": "2.0", "id": true, "error": {"code": 0, "message": ""} })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": [], "error": {"code": 0, "message": ""} })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": {}, "error": {"code": 0, "message": ""} })"_sv,
          // error response with error type mismatch
          u8R"({ "jsonrpc": "2.0", "id": 10, "error": 42 })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": 10, "error": "bad thing" })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": 10, "error": null })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": 10, "error": [] })"_sv,
          // ambiguous successful or error response
          u8R"({ "jsonrpc": "2.0", "id": 10, "result": {}, "error": {"code": 0, "message": ""} })"_sv,
          // ambiguous successful or error response with error type mismatch
          u8R"({ "jsonrpc": "2.0", "id": 10, "result": {}, "error": 42 })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": 10, "result": {}, "error": null })"_sv,
          // ambiguous response or notification
          u8R"({ "jsonrpc": "2.0", "method": "test", "result": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "test", "error": {"code": 0, "message": ""} })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": 10, "method": "test", "result": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "id": 10, "method": "test", "error": {"code": 0, "message": ""} })"_sv,
          // response with missing result or error
          u8R"({ "jsonrpc": "2.0", "id": 10 })"_sv,
      }) {
    SCOPED_TRACE(out_string8(message));

    test_json_rpc_message_handler handler;
    spy_lsp_endpoint_remote remote;
    lsp_json_rpc_message_parser server(&handler);

    server.append(make_message(message));
    server.flush_error_responses(remote);

    ASSERT_EQ(remote.messages.size(), 1);
    EXPECT_EQ(look_up(remote.messages[0], "jsonrpc"), "2.0");
    // TODO(strager): Check "id".
    EXPECT_FALSE(look_up(remote.messages[0]).as_object().contains("result"));
    ASSERT_TRUE(look_up(remote.messages[0]).as_object().contains("error"));
    EXPECT_EQ(look_up(remote.messages[0], "error", "code"), -32600);
    EXPECT_EQ(look_up(remote.messages[0], "error", "message"),
              "Invalid Request");
  }
}
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
