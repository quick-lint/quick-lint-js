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

TEST(test_lsp_endpoint, single_unbatched_request) {
  struct mock_lsp_server_handler {
    void handle_request(const char8*, ::Json::Value& request,
                        string8& response_json) {
      EXPECT_EQ(request["method"], "testmethod");

      ::Json::Value response;
      response["jsonrpc"] = "2.0";
      response["id"] = request["id"];
      response["params"] = "testresponse";
      response_json.append(json_to_string(response));
    }

    void handle_notification(const char8*, ::Json::Value&, string8&) {
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
    void handle_request(const char8*, ::Json::Value& request,
                        string8& response_json) {
      EXPECT_THAT(request["method"],
                  ::testing::AnyOf("testmethod A", "testmethod B"));

      ::Json::Value response;
      response["jsonrpc"] = "2.0";
      response["id"] = request["id"];
      response["params"] = "testresponse";
      response_json.append(json_to_string(response));
    }

    void handle_notification(const char8*, ::Json::Value&, string8&) {
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
    void handle_request(const char8*, ::Json::Value&, string8&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(const char8*, ::Json::Value& notification,
                             string8&) {
      EXPECT_EQ(notification["method"], "testmethod");
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
    void handle_request(const char8*, ::Json::Value&, string8&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(const char8*, ::Json::Value& notification,
                             string8& reply_json) {
      EXPECT_EQ(notification["method"], "testmethod");

      ::Json::Value reply;
      reply["jsonrpc"] = "2.0";
      reply["method"] = "testreply";
      reply["params"] = "testparams";
      reply_json.append(json_to_string(reply));
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
    void handle_request(const char8*, ::Json::Value&, string8&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(const char8*, ::Json::Value& notification,
                             string8&) {
      EXPECT_EQ(notification["method"], "testmethod");
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
    void handle_request(const char8*, ::Json::Value&, string8&) {
      ADD_FAILURE() << "handle_request should not be called";
    }

    void handle_notification(const char8*, ::Json::Value& notification,
                             string8& reply_json) {
      EXPECT_EQ(notification["method"], "testmethod");

      ::Json::Value reply;
      reply["jsonrpc"] = "2.0";
      reply["method"] = "testreply";
      reply["params"] = "testparams";
      reply_json.append(json_to_string(reply));
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
