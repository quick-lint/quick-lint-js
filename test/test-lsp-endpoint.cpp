// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <boost/json/serialize.hpp>
#include <boost/json/value.hpp>
#include <functional>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/spy-lsp-endpoint-remote.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>

QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")

using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
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
  struct mock_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object& request,
                        byte_buffer& response_json) {
      EXPECT_EQ(json_get_string(request["method"]), "testmethod");

      ::boost::json::value response = {
          {"jsonrpc", "2.0"},
          {"id", simdjson_to_boost_json(request["id"])},
          {"params", "testresponse"},
      };
      response_json.append_copy(json_to_string(response));
    }

    void handle_notification(::simdjson::ondemand::object&, std::string_view) {
      ADD_FAILURE() << "handle_notification should not be called";
    }

    void take_pending_notification_jsons(
        std::function<void(byte_buffer&&)>) noexcept {}
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "testmethod",
        "params": {}
      })"));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_EQ(look_up(remote.messages[0], "id"), 3);
  EXPECT_EQ(look_up(remote.messages[0], "params"), "testresponse");
}

TEST(test_lsp_endpoint, batched_request) {
  struct mock_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object& request,
                        byte_buffer& response_json) {
      EXPECT_THAT(json_get_string(request["method"]),
                  ::testing::AnyOf("testmethod A", "testmethod B"));

      ::boost::json::value response = {
          {"jsonrpc", "2.0"},
          {"id", simdjson_to_boost_json(request["id"])},
          {"params", "testresponse"},
      };
      response_json.append_copy(json_to_string(response));
    }

    void handle_notification(::simdjson::ondemand::object&, std::string_view) {
      ADD_FAILURE() << "handle_notification should not be called";
    }

    void take_pending_notification_jsons(
        std::function<void(byte_buffer&&)>) noexcept {}
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();
  remote.allow_batch_messages = true;

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
      ])"));

  ASSERT_EQ(remote.messages.size(), 1);
  ASSERT_EQ(remote.messages[0].as_array().size(), 2);
  EXPECT_EQ(look_up(remote.messages[0], 0, "id"), 3);
  EXPECT_EQ(look_up(remote.messages[0], 1, "id"), 4);
}

TEST(test_lsp_endpoint, single_unbatched_notification_with_no_reply) {
  static int handle_notification_count;
  handle_notification_count = 0;

  struct mock_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::ondemand::object& notification,
                             std::string_view method) {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");
      handle_notification_count += 1;
    }

    void take_pending_notification_jsons(
        std::function<void(byte_buffer&&)>) noexcept {}
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      })"));

  EXPECT_THAT(remote.messages, IsEmpty());
  EXPECT_EQ(handle_notification_count, 1);
}

TEST(test_lsp_endpoint, single_unbatched_notification_with_reply) {
  struct mock_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::ondemand::object& notification,
                             std::string_view method) {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");

      ::boost::json::value reply = {
          {"jsonrpc", "2.0"},
          {"method", "testreply"},
          {"params", "testparams"},
      };
      this->pending_notifications.push_back(reply);
    }

    void take_pending_notification_jsons(
        std::function<void(byte_buffer&&)> callback) noexcept {
      for (::boost::json::value& reply : this->pending_notifications) {
        byte_buffer notification_json;
        notification_json.append_copy(json_to_string(reply));
        callback(std::move(notification_json));
      }
      this->pending_notifications.clear();
    }

    std::vector< ::boost::json::value> pending_notifications;
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();

  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      })"));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_EQ(look_up(remote.messages[0], "method"), "testreply");
  EXPECT_EQ(look_up(remote.messages[0], "params"), "testparams");
}

TEST(test_lsp_endpoint, batched_notification_with_no_reply) {
  static int handle_notification_count;
  handle_notification_count = 0;

  struct mock_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::ondemand::object& notification,
                             std::string_view method) {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");
      handle_notification_count += 1;
    }

    void take_pending_notification_jsons(
        std::function<void(byte_buffer&&)>) noexcept {}
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();
  remote.allow_batch_messages = true;

  server.append(
      make_message(u8R"([{
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      }])"));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_THAT(remote.messages[0].as_array(), IsEmpty());
  EXPECT_EQ(handle_notification_count, 1);
}

TEST(test_lsp_endpoint, batched_notification_with_reply) {
  struct mock_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::ondemand::object& notification,
                             std::string_view method) {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      EXPECT_EQ(method, "testmethod");

      ::boost::json::value reply = {
          {"jsonrpc", "2.0"},
          {"method", "testreply"},
          {"params", "testparams"},
      };
      this->pending_notifications.push_back(reply);
    }

    void take_pending_notification_jsons(
        std::function<void(byte_buffer&&)> callback) noexcept {
      for (::boost::json::value& reply : this->pending_notifications) {
        byte_buffer notification_json;
        notification_json.append_copy(json_to_string(reply));
        callback(std::move(notification_json));
      }
      this->pending_notifications.clear();
    }

    std::vector< ::boost::json::value> pending_notifications;
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();
  remote.allow_batch_messages = true;

  server.append(
      make_message(u8R"([{
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      }])"));

  ASSERT_EQ(remote.messages.size(), 2);
  EXPECT_THAT(remote.messages[0].as_array(), IsEmpty());
  EXPECT_EQ(look_up(remote.messages[1], "method"), "testreply");
  EXPECT_EQ(look_up(remote.messages[1], "params"), "testparams");
}

// https://www.jsonrpc.org/specification#error_object
TEST(test_lsp_endpoint, malformed_json) {
  struct mock_lsp_server_handler {
    void handle_request(::simdjson::ondemand::object&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::ondemand::object&, std::string_view) {
      ADD_FAILURE() << "handle_notification should not be called";
    }

    void take_pending_notification_jsons(
        std::function<void(byte_buffer&&)>) noexcept {}
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();

  server.append(make_message(u8"{ malformed json! }"));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_EQ(look_up(remote.messages[0], "jsonrpc"), "2.0");
  EXPECT_EQ(look_up(remote.messages[0], "id"), ::boost::json::value());
  EXPECT_FALSE(look_up(remote.messages[0]).as_object().contains("result"));
  ASSERT_TRUE(look_up(remote.messages[0]).as_object().contains("error"));
  EXPECT_EQ(look_up(remote.messages[0], "error", "code"), -32700);
  EXPECT_EQ(look_up(remote.messages[0], "error", "message"), "Parse error");
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
