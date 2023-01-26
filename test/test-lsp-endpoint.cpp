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
#include <quick-lint-js/lsp/lsp-endpoint.h>
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
struct test_lsp_server_handler : public lsp_endpoint_handler {
  void handle_request(::simdjson::ondemand::object&, std::string_view,
                      string8_view, byte_buffer&) override {
    ADD_FAILURE() << "handle_request should not be called";
  }

  void handle_response(lsp_endpoint_handler::request_id_type,
                       ::simdjson::ondemand::value&) override {
    ADD_FAILURE() << "handle_response should not be called";
  }

  void handle_error_response(lsp_endpoint_handler::request_id_type,
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

string8 json_to_string(const ::boost::json::value& value) {
  return to_string8(::boost::json::serialize(value));
}

std::string json_get_string(
    ::simdjson::simdjson_result< ::simdjson::ondemand::value>&& value) {
  std::string_view s = "<not found>";
  EXPECT_EQ(value.get(s), ::simdjson::error_code::SUCCESS);
  return std::string(s);
}

TEST(test_lsp_endpoint, single_unbatched_request) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object& request,
                        std::string_view method, string8_view id_json,
                        byte_buffer& response_json) override {
      EXPECT_EQ(json_get_string(request["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");
      EXPECT_EQ(id_json, u8"3");

      ::boost::json::value response = {
          {"jsonrpc", "2.0"},
          {"id", simdjson_to_boost_json(request["id"])},
          {"params", "testresponse"},
      };
      response_json.append_copy(json_to_string(response));
    }
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "testmethod",
        "params": {}
      })"_sv));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_EQ(look_up(remote.messages[0], "id"), 3);
  EXPECT_EQ(look_up(remote.messages[0], "params"), "testresponse");
}

TEST(test_lsp_endpoint, batched_request) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object& request,
                        std::string_view method, string8_view id_json,
                        byte_buffer& response_json) override {
      EXPECT_THAT(json_get_string(request["method"]),
                  ::testing::AnyOf("testmethod A", "testmethod B"));
      EXPECT_THAT(method, ::testing::AnyOf("testmethod A", "testmethod B"));
      EXPECT_THAT(id_json, ::testing::AnyOf(u8"3", u8"4"));

      ::boost::json::value response = {
          {"jsonrpc", "2.0"},
          {"id", simdjson_to_boost_json(request["id"])},
          {"params", "testresponse"},
      };
      response_json.append_copy(json_to_string(response));
    }
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  remote.allow_batch_messages = true;
  lsp_endpoint server(&handler, &remote);

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

  ASSERT_EQ(remote.messages.size(), 1);
  ASSERT_EQ(remote.messages[0].as_array().size(), 2);
  EXPECT_EQ(look_up(remote.messages[0], 0, "id"), 3);
  EXPECT_EQ(look_up(remote.messages[0], 1, "id"), 4);
}

TEST(test_lsp_endpoint, successful_response) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_response(lsp_endpoint_handler::request_id_type request_id,
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
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "result": {"key": 42}
      })"_sv));

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_TRUE(handler.response_handled);
}

// This test is the same as successful_response, but it queries the given
// simdjson::ondemand::value-s in a different way. This test shakes out bugs
// caused by violating simdjson's strict usage pattern requirements.
TEST(test_lsp_endpoint, successful_response_v2) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_response(lsp_endpoint_handler::request_id_type request_id,
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
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "result": {"key": 42}
      })"_sv));

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_TRUE(handler.response_handled);
}

TEST(test_lsp_endpoint, error_response) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_error_response(lsp_endpoint_handler::request_id_type request_id,
                               std::int64_t code,
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
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "error": {
          "code": 6969,
          "message": "test error message"
        }
      })"_sv));

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_TRUE(handler.error_response_handled);
}

TEST(test_lsp_endpoint, batched_responses) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_response(lsp_endpoint_handler::request_id_type,
                         ::simdjson::ondemand::value&) override {
      this->handled_success_response = true;
    }

    void handle_error_response(lsp_endpoint_handler::request_id_type,
                               std::int64_t, std::string_view) override {
      this->handled_error_response = true;
    }

    bool handled_success_response = false;
    bool handled_error_response = false;
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  remote.allow_batch_messages = true;
  lsp_endpoint server(&handler, &remote);

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

  EXPECT_TRUE(handler.handled_success_response);
  EXPECT_TRUE(handler.handled_error_response);
}

TEST(test_lsp_endpoint, single_unbatched_notification_with_no_reply) {
  static int handle_notification_count;
  handle_notification_count = 0;

  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_notification(::simdjson::ondemand::object& notification,
                             std::string_view method) override {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");
      handle_notification_count += 1;
    }
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      })"_sv));

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_EQ(handle_notification_count, 1);
}

TEST(test_lsp_endpoint, single_unbatched_notification_with_reply) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
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
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      })"_sv));

  EXPECT_THAT(remote.messages, IsEmpty());
  ASSERT_EQ(handler.pending_notifications.size(), 1);
  EXPECT_EQ(look_up(handler.pending_notifications[0], "method"), "testreply");
  EXPECT_EQ(look_up(handler.pending_notifications[0], "params"), "testparams");
}

TEST(test_lsp_endpoint, batched_notification_with_no_reply) {
  static int handle_notification_count;
  handle_notification_count = 0;

  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_notification(::simdjson::ondemand::object& notification,
                             std::string_view method) override {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");
      handle_notification_count += 1;
    }
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  remote.allow_batch_messages = true;
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"([{
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      }])"_sv));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_THAT(remote.messages[0].as_array(), IsEmpty());
  EXPECT_EQ(handle_notification_count, 1);
}

TEST(test_lsp_endpoint, batched_notification_with_reply) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
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
  remote.allow_batch_messages = true;
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"([{
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      }])"_sv));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_THAT(remote.messages[0].as_array(), IsEmpty());

  ASSERT_EQ(handler.pending_notifications.size(), 1);
  EXPECT_EQ(look_up(handler.pending_notifications[0], "method"), "testreply");
  EXPECT_EQ(look_up(handler.pending_notifications[0], "params"), "testparams");
}

TEST(test_lsp_endpoint, big_batch) {
  struct mock_lsp_server_handler : public test_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object&, std::string_view method,
                        string8_view id_json,
                        byte_buffer& response_json) override {
      this->handled_request = true;
      EXPECT_EQ(method, "testrequest");
      response_json.append_copy(
          u8R"({"jsonrpc": "2.0", "params": "response", "id": )"_sv);
      response_json.append_copy(id_json);
      response_json.append_copy(u8"}"_sv);
    }

    void handle_response(lsp_endpoint_handler::request_id_type request_id,
                         ::simdjson::ondemand::value& result) override {
      this->handled_response = true;
      EXPECT_EQ(request_id, 2);
      std::string_view result_string;
      ASSERT_EQ(result.get(result_string), ::simdjson::SUCCESS);
      EXPECT_EQ(result_string, "testresult");
    }

    void handle_error_response(lsp_endpoint_handler::request_id_type request_id,
                               std::int64_t code,
                               std::string_view message) override {
      this->handled_error_response = true;
      EXPECT_EQ(request_id, 3);
      EXPECT_EQ(code, -1);
      EXPECT_EQ(message, "testerror");
    }

    void handle_notification(::simdjson::ondemand::object&,
                             std::string_view method) override {
      this->handled_notification = 1;
      EXPECT_EQ(method, "testnotification");
    }

    bool handled_request = false;
    bool handled_response = false;
    bool handled_error_response = false;
    bool handled_notification = false;
  };
  mock_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  remote.allow_batch_messages = true;
  lsp_endpoint server(&handler, &remote);

  server.append(
      make_message(u8R"([{
        "jsonrpc": "2.0",
        "method": "testnotification",
        "params": {}
      }, {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "testrequest",
        "params": {}
      }, {
        "jsonrpc": "2.0",
        "id": 2,
        "result": "testresult"
      }, {
        "jsonrpc": "2.0",
        "id": 3,
        "error": {
          "code": -1,
          "message": "testerror"
        }
      }])"_sv));

  ASSERT_EQ(remote.messages.size(), 1);
  ::boost::json::array batch = remote.messages[0].as_array();
  ASSERT_THAT(batch, ::testing::ElementsAre(::testing::_));
  EXPECT_EQ(look_up(batch, 0, "params"), "response");

  EXPECT_TRUE(handler.handled_request);
  EXPECT_TRUE(handler.handled_response);
  EXPECT_TRUE(handler.handled_error_response);
  EXPECT_TRUE(handler.handled_notification);
}

// https://www.jsonrpc.org/specification#error_object
TEST(test_lsp_endpoint, malformed_json) {
  test_lsp_server_handler handler;
  spy_lsp_endpoint_remote remote;
  lsp_endpoint server(&handler, &remote);

  server.append(make_message(u8"{ malformed json! }"_sv));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_EQ(look_up(remote.messages[0], "jsonrpc"), "2.0");
  EXPECT_EQ(look_up(remote.messages[0], "id"), ::boost::json::value());
  EXPECT_FALSE(look_up(remote.messages[0]).as_object().contains("result"));
  ASSERT_TRUE(look_up(remote.messages[0]).as_object().contains("error"));
  EXPECT_EQ(look_up(remote.messages[0], "error", "code"), -32700);
  EXPECT_EQ(look_up(remote.messages[0], "error", "message"), "Parse error");
}

TEST(test_lsp_endpoint, invalid_message) {
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

    test_lsp_server_handler handler;
    spy_lsp_endpoint_remote remote;
    lsp_endpoint server(&handler, &remote);

    server.append(make_message(message));

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
