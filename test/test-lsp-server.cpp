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
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-server.h>
#include <quick-lint-js/spy-lsp-endpoint-remote.h>
#include <quick-lint-js/version.h>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
constexpr int lsp_error_severity = 1;

string8 make_message(string8_view content) {
  return string8(u8"Content-Length: ") +
         to_string8(std::to_string(content.size())) + u8"\r\n\r\n" +
         string8(content);
}

class test_linting_lsp_server : public ::testing::Test {
 public:
  lsp_endpoint<linting_lsp_server_handler, spy_lsp_endpoint_remote> server;
  spy_lsp_endpoint_remote& client = server.remote();
};

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
TEST_F(test_linting_lsp_server, initialize) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
          "processId": null,
          "rootUri": null,
          "capabilities": {}
        }
      })"));

  ASSERT_EQ(this->client.messages.size(), 1);
  ::Json::Value& response = this->client.messages[0];
  EXPECT_EQ(response["id"], 1);
  EXPECT_FALSE(response.isMember("error"));
  // LSP InitializeResult:
  EXPECT_THAT(response["result"]["capabilities"]["textDocumentSync"]["change"],
              ::testing::AnyOf(1, 2));
  EXPECT_EQ(response["result"]["capabilities"]["textDocumentSync"]["openClose"],
            true);
  EXPECT_EQ(response["result"]["serverInfo"]["name"], "quick-lint-js");
  EXPECT_EQ(response["result"]["serverInfo"]["version"],
            QUICK_LINT_JS_VERSION_STRING);
}

TEST_F(test_linting_lsp_server, server_ignores_initialized_notification) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
      })"));

  EXPECT_THAT(this->client.messages, IsEmpty());
}

TEST_F(test_linting_lsp_server, opening_document_lints) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "let x = x;"
          }
        }
      })"));

  ASSERT_EQ(this->client.messages.size(), 1);
  ::Json::Value& response = this->client.messages[0];
  EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
  EXPECT_FALSE(response.isMember("error"));
  // LSP PublishDiagnosticsParams:
  EXPECT_EQ(response["params"]["uri"], "file:///test.js");
  EXPECT_EQ(response["params"]["version"], 10);
  ::Json::Value& diagnostics = response["params"]["diagnostics"];
  EXPECT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["character"], 8);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["character"], 9);
  EXPECT_EQ(diagnostics[0]["severity"], lsp_error_severity);
  EXPECT_EQ(diagnostics[0]["message"], "variable used before declaration: x");
}

TEST_F(test_linting_lsp_server,
       opening_non_javasript_file_does_not_cause_diagnostics) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.html",
            "languageId": "html",
            "version": 10,
            "text": "<b>hi</b>"
          }
        }
      })"));

  EXPECT_THAT(this->client.messages, IsEmpty());
}

// TODO(strager): For batch requests containing multiple edits, lint and publish
// diagnostics only once.

// TODO(strager): Handle textDocument/didOpen.
}
}
