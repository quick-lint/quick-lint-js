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

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <json/value.h>
#include <json/writer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/spy-lsp-endpoint-remote.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>

using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
string8 make_message(string8_view content) {
  return string8(u8"Content-Length: ") +
         to_string8(std::to_string(content.size())) + u8"\r\n\r\n" +
         string8(content);
}

string8 json_to_string(::Json::Value& value) {
  return to_string8(::Json::writeString(::Json::StreamWriterBuilder(), value));
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
::Json::Value simdjson_to_jsoncpp(const ::simdjson::dom::element& value) {
  switch (value.type()) {
  case ::simdjson::dom::element_type::INT64: {
    std::int64_t data;
    ::simdjson::error_code error = value.get(data);
    QLJS_ASSERT(error == ::simdjson::error_code::SUCCESS);
    return ::Json::Value(data);
  }

  case ::simdjson::dom::element_type::UINT64: {
    std::uint64_t data;
    ::simdjson::error_code error = value.get(data);
    QLJS_ASSERT(error == ::simdjson::error_code::SUCCESS);
    return ::Json::Value(data);
  }

  case ::simdjson::dom::element_type::STRING: {
    std::string_view data;
    ::simdjson::error_code error = value.get(data);
    QLJS_ASSERT(error == ::simdjson::error_code::SUCCESS);
    return ::Json::Value(data.data(), data.data() + data.size());
  }

  case ::simdjson::dom::element_type::ARRAY:
  case ::simdjson::dom::element_type::BOOL:
  case ::simdjson::dom::element_type::DOUBLE:
  case ::simdjson::dom::element_type::NULL_VALUE:
  case ::simdjson::dom::element_type::OBJECT:
    QLJS_UNIMPLEMENTED();
    break;
  }

  QLJS_UNREACHABLE();
}
QLJS_WARNING_POP

::Json::Value simdjson_to_jsoncpp(
    ::simdjson::simdjson_result<::simdjson::dom::element>&& value) {
  ::simdjson::dom::element unwrapped_value;
  ::simdjson::error_code error = value.get(unwrapped_value);
  if (error != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  return simdjson_to_jsoncpp(unwrapped_value);
}

std::string json_get_string(
    ::simdjson::simdjson_result<::simdjson::dom::element>&& value) {
  std::string_view s = "<not found>";
  EXPECT_EQ(value.get(s), ::simdjson::error_code::SUCCESS);
  return std::string(s);
}

TEST(test_lsp_endpoint, single_unbatched_request) {
  struct mock_lsp_server_handler {
    void handle_request(::simdjson::dom::element& request,
                        byte_buffer& response_json) {
      EXPECT_EQ(json_get_string(request["method"]), "testmethod");

      ::Json::Value response;
      response["jsonrpc"] = "2.0";
      response["id"] = simdjson_to_jsoncpp(request["id"]);
      response["params"] = "testresponse";
      response_json.append_copy(json_to_string(response));
    }

    void handle_notification(::simdjson::dom::element&, byte_buffer&) {
      ADD_FAILURE() << "handle_notification should not be called";
    }
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
  EXPECT_EQ(remote.messages[0]["id"], 3);
  EXPECT_EQ(remote.messages[0]["params"], "testresponse");
}

TEST(test_lsp_endpoint, batched_request) {
  struct mock_lsp_server_handler {
    void handle_request(::simdjson::dom::element& request,
                        byte_buffer& response_json) {
      EXPECT_THAT(json_get_string(request["method"]),
                  ::testing::AnyOf("testmethod A", "testmethod B"));

      ::Json::Value response;
      response["jsonrpc"] = "2.0";
      response["id"] = simdjson_to_jsoncpp(request["id"]);
      response["params"] = "testresponse";
      response_json.append_copy(json_to_string(response));
    }

    void handle_notification(::simdjson::dom::element&, byte_buffer&) {
      ADD_FAILURE() << "handle_notification should not be called";
    }
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();

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
  ASSERT_EQ(remote.messages[0].size(), 2);
  EXPECT_EQ(remote.messages[0][0]["id"], 3);
  EXPECT_EQ(remote.messages[0][1]["id"], 4);
}

TEST(test_lsp_endpoint, single_unbatched_notification_with_no_reply) {
  static int handle_notification_count;
  handle_notification_count = 0;

  struct mock_lsp_server_handler {
    void handle_request(::simdjson::dom::element&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::dom::element& notification,
                             byte_buffer&) {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      handle_notification_count += 1;
    }
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
    void handle_request(::simdjson::dom::element&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::dom::element& notification,
                             byte_buffer& reply_json) {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");

      ::Json::Value reply;
      reply["jsonrpc"] = "2.0";
      reply["method"] = "testreply";
      reply["params"] = "testparams";
      reply_json.append_copy(json_to_string(reply));
    }
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
  EXPECT_EQ(remote.messages[0]["method"], "testreply");
  EXPECT_EQ(remote.messages[0]["params"], "testparams");
}

TEST(test_lsp_endpoint, batched_notification_with_no_reply) {
  static int handle_notification_count;
  handle_notification_count = 0;

  struct mock_lsp_server_handler {
    void handle_request(::simdjson::dom::element&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::dom::element& notification,
                             byte_buffer&) {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");
      handle_notification_count += 1;
    }
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();

  server.append(
      make_message(u8R"([{
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      }])"));

  ASSERT_EQ(remote.messages.size(), 1);
  EXPECT_THAT(remote.messages[0], IsEmpty());
  EXPECT_EQ(handle_notification_count, 1);
}

TEST(test_lsp_endpoint, batched_notification_with_reply) {
  struct mock_lsp_server_handler {
    void handle_request(::simdjson::dom::element&, byte_buffer&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(::simdjson::dom::element& notification,
                             byte_buffer& reply_json) {
      EXPECT_EQ(json_get_string(notification["method"]), "testmethod");

      ::Json::Value reply;
      reply["jsonrpc"] = "2.0";
      reply["method"] = "testreply";
      reply["params"] = "testparams";
      reply_json.append_copy(json_to_string(reply));
    }
  };
  lsp_endpoint<mock_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& remote = server.remote();

  server.append(
      make_message(u8R"([{
        "jsonrpc": "2.0",
        "method": "testmethod",
        "params": {}
      }])"));

  ASSERT_EQ(remote.messages.size(), 2);
  EXPECT_THAT(remote.messages[0], IsEmpty());
  EXPECT_EQ(remote.messages[1]["method"], "testreply");
  EXPECT_EQ(remote.messages[1]["params"], "testparams");
}
}
}
