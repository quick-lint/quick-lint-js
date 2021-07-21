// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <json/value.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/fake-configuration-filesystem.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-server.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/spy-lsp-endpoint-remote.h>
#include <quick-lint-js/version.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>
#include <tuple>
#include <utility>

QLJS_WARNING_IGNORE_CLANG("-Wcovered-switch-default")

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using namespace std::literals::string_literals;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
constexpr int lsp_error_severity = 1;

constexpr int lsp_warning_message_type = 2;

#if QLJS_HAVE_WINDOWS_H
windows_file_io_error generic_file_io_error = {ERROR_READ_FAULT};
#endif
#if QLJS_HAVE_UNISTD_H
posix_file_io_error generic_file_io_error = {EIO};
#endif

string8 make_message(string8_view content) {
  return string8(u8"Content-Length: ") +
         to_string8(std::to_string(content.size())) + u8"\r\n\r\n" +
         string8(content);
}

using endpoint = lsp_endpoint<linting_lsp_server_handler<mock_lsp_linter>,
                              spy_lsp_endpoint_remote>;

template <class LinterCallback>
endpoint make_endpoint(configuration_filesystem* fs,
                       LinterCallback&& linter_callback) {
  return endpoint(
      /*handler_args=*/std::forward_as_tuple(
          fs, std::forward<LinterCallback>(linter_callback)),
      /*remote_args=*/std::forward_as_tuple());
}

endpoint make_endpoint(configuration_filesystem* fs) {
  return make_endpoint(fs, [](configuration&, padded_string_view, string8_view,
                              string8_view, byte_buffer&) {});
}

class test_linting_lsp_server : public ::testing::Test {
 public:
  std::function<void(configuration&, padded_string_view code,
                     string8_view uri_json, string8_view version,
                     byte_buffer& notification_json)>
      lint_callback;
  std::vector<string8> lint_calls;

  fake_configuration_filesystem fs;
  endpoint server = make_endpoint(
      &fs, [this](configuration& config, padded_string_view code,
                  string8_view uri_json, string8_view version_json,
                  byte_buffer& notification_json) {
        this->lint_calls.emplace_back(code.string_view());
        if (this->lint_callback) {
          this->lint_callback(config, code, uri_json, version_json,
                              notification_json);
        }
      });
  spy_lsp_endpoint_remote& client = server.remote();

  std::string config_file_load_error_message(const char* js_path,
                                             const char* error_path) {
    return "Failed to load configuration file for "s +
           this->fs.rooted(js_path).path() +
           ". "s
           "Using default configuration.\n"s
           "Error details: failed to read from "s +
           this->fs.rooted(error_path).path() + ": "s +
           generic_file_io_error.to_string();
  }
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
              2);
  EXPECT_EQ(response["result"]["capabilities"]["textDocumentSync"]["openClose"],
            true);
  EXPECT_EQ(response["result"]["serverInfo"]["name"], "quick-lint-js");
  EXPECT_EQ(response["result"]["serverInfo"]["version"],
            QUICK_LINT_JS_VERSION_STRING);
}

// For the "id" field of a request, JSON-RPC allows numbers, strings, and null.
TEST(test_linting_lsp_server_plain, initialize_with_different_request_ids) {
  struct test_case {
    string8_view id_json;
    ::Json::Value id;
  };

  // TODO(strager): Support numbers with fractional parts, such as 12.34.
  for (const test_case& test : {
           test_case{u8"null", ::Json::Value::nullSingleton()},
           test_case{u8"1", ::Json::Value(1)},
           test_case{u8"9007199254740991",
                     ::Json::Value(std::int64_t{9007199254740991LL})},
           test_case{u8"-12345", ::Json::Value(-12345)},
           test_case{u8R"("A")", ::Json::Value("A")},
           test_case{u8R"("id value goes \"here\"")",
                     ::Json::Value("id value goes \"here\"")},
           test_case{u8R"("id value goes \"here\"")",
                     ::Json::Value("id value goes \"here\"")},
       }) {
    fake_configuration_filesystem fs;
    auto server = make_endpoint(&fs);
    spy_lsp_endpoint_remote& client = server.remote();

    server.append(
        make_message(u8R"({
          "jsonrpc": "2.0",
          "id": )" + string8(test.id_json) +
                     u8R"(,
          "method": "initialize",
          "params": {
            "processId": null,
            "rootUri": null,
            "capabilities": {}
          }
        })"));

    ASSERT_EQ(client.messages.size(), 1);
    EXPECT_EQ(client.messages[0]["id"], test.id);
  }
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

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#shutdown
TEST_F(test_linting_lsp_server, shutdown) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "shutdown"
      })"));

  ASSERT_EQ(this->client.messages.size(), 1);
  ::Json::Value& response = this->client.messages[0];
  EXPECT_EQ(response["id"], 10);
  EXPECT_FALSE(response.isMember("error"));
  EXPECT_EQ(response["result"], ::Json::Value::nullSingleton());
}

#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST
// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#exit
TEST_F(test_linting_lsp_server,
       exit_without_shutdown_quits_program_with_exit_code_1) {
  auto send_exit = [this]() {
    this->server.append(
        make_message(u8R"({
          "jsonrpc": "2.0",
          "method": "exit"
        })"));
    std::exit(99);  // Shouldn't happen.
  };
  EXPECT_EXIT({ send_exit(); }, ::testing::ExitedWithCode(1), "");
}
#endif

#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST
// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#exit
TEST_F(test_linting_lsp_server,
       exit_with_shutdown_quits_program_with_exit_code_0) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "shutdown"
      })"));
  this->client.messages.clear();

  auto send_exit = [this]() {
    this->server.append(
        make_message(u8R"({
          "jsonrpc": "2.0",
          "method": "exit"
        })"));
    std::exit(99);  // Shouldn't happen.
  };
  EXPECT_EXIT({ send_exit(); }, ::testing::ExitedWithCode(0), "");
}
#endif

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#dollarRequests
TEST_F(test_linting_lsp_server, dollar_notifications_are_ignored) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "$/someNotification"
      })"));
  EXPECT_THAT(this->client.messages, IsEmpty());
}

TEST_F(test_linting_lsp_server, opening_document_lints) {
  this->lint_callback = [&](configuration&, padded_string_view code,
                            string8_view uri_json, string8_view version,
                            byte_buffer& notification_json) {
    EXPECT_EQ(code, u8"let x = x;");
    EXPECT_EQ(uri_json, u8"\"file:///test.js\"");
    EXPECT_EQ(version, u8"10"sv);

    notification_json.append_copy(
        u8R"--(
              {
                "method":"textDocument/publishDiagnostics",
                "params":{
                  "uri": "file:///test.js",
                  "version": 10,
                  "diagnostics": [
                    {
                      "range": {
                        "start": {"line": 0, "character": 8},
                        "end": {"line": 0, "character": 9}
                      },
                      "severity": 1,
                        "message": "variable used before declaration: x"
                      }
                  ]
                },
                "jsonrpc":"2.0"
              }
            )--");
  };

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

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"let x = x;"));
}

TEST_F(test_linting_lsp_server, opening_document_language_id_js_lints) {
  this->lint_callback = [&](configuration&, padded_string_view code,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer& notification_json) {
    EXPECT_EQ(code, u8"let x = x;");
    EXPECT_EQ(uri_json, u8"\"file:///test.js\"");
    EXPECT_EQ(version_json, u8"10");
    notification_json.append_copy(
        u8R"--(
              {
                "method":"textDocument/publishDiagnostics",
                "params":{
                  "uri": "file:///test.js",
                  "version": 10,
                  "diagnostics": [
                    {
                      "range": {
                        "start": {"line": 0, "character": 8},
                        "end": {"line": 0, "character": 9}
                      },
                      "severity": 1,
                        "message": "variable used before declaration: x"
                      }
                  ]
                },
                "jsonrpc":"2.0"
              }
            )--");
  };

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "languageId": "js",
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

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"let x = x;"));
}

TEST_F(test_linting_lsp_server, changing_document_with_full_text_lints) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "languageId": "javascript",
            "version": 10,
            "text": "FIRST"
          }
        }
      })"));
  this->lint_calls.clear();
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "version": 11
          },
          "contentChanges": [
            {
              "text": "SECOND"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"SECOND"));
}

TEST_F(test_linting_lsp_server,
       changing_document_with_single_incremental_edit_lints) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "languageId": "javascript",
            "version": 10,
            "text": "the quick brown fox"
          }
        }
      })"));
  this->lint_calls.clear();

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "version": 11
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 4},
                "end": {"line": 0, "character": 9}
              },
              "text": "slow"
            }
          ]
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "version": 11
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 9},
                "end": {"line": 0, "character": 14}
              },
              "text": "purple"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls,
              ElementsAre(u8"the slow brown fox", u8"the slow purple fox"));
}

TEST_F(test_linting_lsp_server,
       changing_document_with_multiple_incremental_edits_lints_only_once) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "languageId": "javascript",
            "version": 10,
            "text": "the quick brown fox"
          }
        }
      })"));
  this->lint_calls.clear();

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "version": 11
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 4},
                "end": {"line": 0, "character": 9}
              },
              "text": "slow"
            },
            {
              "range": {
                "start": {"line": 0, "character": 9},
                "end": {"line": 0, "character": 14}
              },
              "text": "purple"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"the slow purple fox"));
}

TEST_F(test_linting_lsp_server, linting_uses_config_from_file) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"testGlobalVariable": true}})");

  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view, string8_view, byte_buffer&) {
    EXPECT_TRUE(config.globals().find(u8"testGlobalVariable"sv));
  };

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8""));
}

TEST_F(test_linting_lsp_server,
       linting_uses_config_from_file_with_special_chars_in_document_uri) {
  this->fs.create_file(this->fs.rooted("a%b~/quick-lint-js.config"),
                       u8R"({"globals": {"testGlobalVariable": true}})");

  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view, string8_view, byte_buffer&) {
    EXPECT_TRUE(config.globals().find(u8"testGlobalVariable"sv));
  };

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(a%25b%7E/%E2%98%83.js",
            "languageId": "javascript",
            "version": 10,
            "text": "snowman"
          }
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"snowman"));
}

TEST_F(test_linting_lsp_server, linting_uses_already_opened_config_file) {
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view, string8_view, byte_buffer&) {
    EXPECT_TRUE(config.globals().find(u8"modified"sv));
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"modified": false}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"modified\": true}}"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8""));
}

TEST_F(test_linting_lsp_server,
       linting_uses_already_opened_shadowing_config_file) {
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view, string8_view, byte_buffer&) {
    EXPECT_TRUE(config.globals().find(u8"haveConfigWithoutDot"sv));
    EXPECT_FALSE(config.globals().find(u8"haveConfigWithDot"sv));
  };

  this->fs.create_file(this->fs.rooted(".quick-lint-js.config"),
                       u8R"({"globals": {"haveConfigWithDot": false}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"haveConfigWithoutDot\": true}}"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8""));
}

TEST_F(test_linting_lsp_server, editing_config_relints_open_js_file) {
  bool after_config_was_loaded = false;

  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer&) {
    if (config.globals().find(u8"after"sv)) {
      EXPECT_FALSE(config.globals().find(u8"before"sv));
      EXPECT_EQ(version_json, u8"10");
      EXPECT_EQ(uri_json,
                u8"\"" + this->fs.file_uri_prefix_8() + u8"test.js\"");
      after_config_was_loaded = true;
    }
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"before\": true}}"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"));
  // Change 'before' to 'after'.
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "version": 2
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 14},
                "end": {"line": 0, "character": 20}
              },
              "text": "after"
            }
          ]
        }
      })"));

  EXPECT_TRUE(after_config_was_loaded);
}

TEST_F(test_linting_lsp_server,
       editing_config_lints_latest_version_of_js_file) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"before\": true}}"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "version": 10,
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "text": ""
          }
        }
      })"));
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "version": 11,
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js"
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 0},
                "end": {"line": 0, "character": 0}
              },
              "text": "updated"
            }
          ]
        }
      })"));

  this->lint_callback = [&](configuration&, padded_string_view, string8_view,
                            string8_view version_json,
                            byte_buffer&) { EXPECT_EQ(version_json, u8"11"); };
  this->lint_calls.clear();

  // Change 'before' to 'after'.
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "version": 2
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 14},
                "end": {"line": 0, "character": 20}
              },
              "text": "after"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"updated"sv));
}

TEST_F(test_linting_lsp_server, editing_config_relints_many_open_js_files) {
  this->lint_callback = [&](configuration&, padded_string_view,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer& notification_json) {
    notification_json.append_copy(
        u8R"(
              {
                "method": "textDocument/publishDiagnostics",
                "params":{
                  "uri": )" +
        string8(uri_json) +
        u8R"(,
                  "version": )" +
        string8(version_json) +
        u8R"(,
                  "diagnostics": []
                },
                "jsonrpc":"2.0"
              }
            )");
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"before\": true}}"
          }
        }
      })"));

  for (const char8* js_file : {u8"a.js", u8"b.js", u8"c.js"}) {
    this->server.append(
        make_message(u8R"({
          "jsonrpc": "2.0",
          "method": "textDocument/didOpen",
          "params": {
            "textDocument": {
              "uri": ")" +
                     this->fs.file_uri_prefix_8() + js_file + u8R"(",
              "languageId": "javascript",
              "version": 10,
              "text": "/* )" +
                     js_file + u8R"( */"
            }
          }
        })"));
  }

  this->lint_calls.clear();
  this->client.messages.clear();
  // Change 'before' to 'after'.
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "version": 2
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 14},
                "end": {"line": 0, "character": 20}
              },
              "text": "after"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls,
              ::testing::UnorderedElementsAre(u8"/* a.js */", u8"/* b.js */",
                                              u8"/* c.js */"));

  std::vector<std::string> linted_uris;
  for (::Json::Value& notification : this->client.messages) {
    EXPECT_EQ(notification["method"], "textDocument/publishDiagnostics");
    std::string uri = notification["params"]["uri"].asString();
    if (uri ==
        to_string(this->fs.file_uri_prefix_8() + u8"quick-lint-js.config")) {
      // Ignore.
      continue;
    }
    linted_uris.emplace_back(uri);
  }
  EXPECT_THAT(linted_uris,
              ::testing::UnorderedElementsAre(
                  to_string(this->fs.file_uri_prefix_8() + u8"a.js"),
                  to_string(this->fs.file_uri_prefix_8() + u8"b.js"),
                  to_string(this->fs.file_uri_prefix_8() + u8"c.js")));
}

TEST_F(test_linting_lsp_server, editing_config_relints_only_affected_js_files) {
  this->lint_callback = [&](configuration&, padded_string_view,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer& notification_json) {
    notification_json.append_copy(
        u8R"(
              {
                "method": "textDocument/publishDiagnostics",
                "params":{
                  "uri": )" +
        string8(uri_json) +
        u8R"(,
                  "version": )" +
        string8(version_json) +
        u8R"(,
                  "diagnostics": []
                },
                "jsonrpc":"2.0"
              }
            )");
  };

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() +
                   u8R"(dir-a/quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"a\": true}}"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() +
                   u8R"(dir-b/quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"b\": true}}"
          }
        }
      })"));

  for (const char8* js_file : {u8"dir-a/test.js", u8"dir-b/test.js"}) {
    this->server.append(
        make_message(u8R"({
          "jsonrpc": "2.0",
          "method": "textDocument/didOpen",
          "params": {
            "textDocument": {
              "uri": ")" +
                     this->fs.file_uri_prefix_8() + js_file + u8R"(",
              "languageId": "javascript",
              "version": 10,
              "text": "/* )" +
                     js_file + u8R"( */"
            }
          }
        })"));
  }

  this->lint_calls.clear();
  this->client.messages.clear();
  // Change 'a' to 'A' in dir-a/quick-lint-js.config (but leave
  // dir-b/quick-lint-js.config as-is).
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() +
                   u8R"(dir-a/quick-lint-js.config",
            "version": 2
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 14},
                "end": {"line": 0, "character": 15}
              },
              "text": "A"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"/* dir-a/test.js */"));

  std::vector<std::string> linted_uris;
  for (::Json::Value& notification : this->client.messages) {
    EXPECT_EQ(notification["method"], "textDocument/publishDiagnostics");
    std::string uri = notification["params"]["uri"].asString();
    if (uri == to_string(this->fs.file_uri_prefix_8() +
                         u8"dir-a/quick-lint-js.config") ||
        uri == to_string(this->fs.file_uri_prefix_8() +
                         u8"dir-b/quick-lint-js.config")) {
      // Ignore.
      continue;
    }
    linted_uris.emplace_back(uri);
  }
  EXPECT_THAT(linted_uris, ElementsAre(to_string(this->fs.file_uri_prefix_8() +
                                                 u8"dir-a/test.js")));
}

TEST_F(test_linting_lsp_server,
       editing_js_file_after_shadowing_config_uses_latest_config) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(.quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"before\": true}}"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "original"
          }
        }
      })"));

  // After opening test.js, create quick-lint-js.config which shadows
  // .quick-lint-js.config.
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"after\": true}}"
          }
        }
      })"));

  this->lint_calls.clear();
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view, string8_view version_json,
                            byte_buffer&) {
    EXPECT_EQ(version_json, u8"11");
    EXPECT_FALSE(config.globals().find(u8"before"sv));
    EXPECT_TRUE(config.globals().find(u8"after"sv));
  };

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "version": 11,
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js"
          },
          "contentChanges": [
            {
              "text": "modified"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"modified"));
}

TEST_F(test_linting_lsp_server, opening_config_relints_open_js_files) {
  bool after_config_was_loaded = false;

  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer&) {
    if (config.globals().find(u8"after"sv)) {
      EXPECT_FALSE(config.globals().find(u8"before"sv));
      EXPECT_EQ(version_json, u8"10");
      EXPECT_EQ(uri_json,
                u8"\"" + this->fs.file_uri_prefix_8() + u8"test.js\"");
      after_config_was_loaded = true;
    }
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"after\": true}}"
          }
        }
      })"));

  EXPECT_TRUE(after_config_was_loaded);
}

TEST_F(test_linting_lsp_server,
       changing_config_on_filesystem_relints_open_js_files) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"));

  bool after_config_was_loaded = false;
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer& notification_json) {
    EXPECT_TRUE(config.globals().find(u8"after"sv));
    EXPECT_FALSE(config.globals().find(u8"before"sv));
    EXPECT_EQ(version_json, u8"10");
    EXPECT_EQ(uri_json, u8"\"" + this->fs.file_uri_prefix_8() + u8"test.js\"");
    after_config_was_loaded = true;

    notification_json.append_copy(
        u8R"({
      "method": "textDocument/publishDiagnostics",
      "params": {
        "uri": ")" +
        this->fs.file_uri_prefix_8() +
        u8R"(test.js",
        "version": 10,
        "diagnostics": []
      },
      "jsonrpc": "2.0"
    })");
  };
  this->client.messages.clear();

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"after": true}})");
  this->server.filesystem_changed();

  EXPECT_TRUE(after_config_was_loaded);

  ASSERT_THAT(this->client.messages, ElementsAre(::testing::_));
  ::Json::Value& notification = this->client.messages[0];
  EXPECT_EQ(notification["method"], "textDocument/publishDiagnostics");
}

TEST_F(
    test_linting_lsp_server,
    linting_uses_config_from_filesystem_if_config_is_opened_then_closed_before_opening_js_file) {
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view, string8_view, byte_buffer&) {
    EXPECT_TRUE(config.globals().find(u8"v1"sv));
    EXPECT_FALSE(config.globals().find(u8"v2"sv));
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"v1": true}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"v2\": true}}"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8""));
}

TEST_F(test_linting_lsp_server,
       closing_open_config_reloads_config_from_filesystem) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"configFromFilesystem": true}})");
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"configFromLSP\": true}}"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"));

  this->lint_calls.clear();
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view, string8_view,
                            byte_buffer& notification_json) {
    EXPECT_TRUE(config.globals().find(u8"configFromFilesystem"sv));
    EXPECT_FALSE(config.globals().find(u8"configFromLSP"sv));
    notification_json.append_copy(
        u8R"({
      "method": "textDocument/publishDiagnostics",
      "params": {
        "uri": ")" +
        this->fs.file_uri_prefix_8() +
        u8R"(test.js",
        "version": 10,
        "diagnostics": []
      },
      "jsonrpc": "2.0"
    })");
  };
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config"
          }
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8""));
}

TEST_F(test_linting_lsp_server, opening_js_file_with_unreadable_config_lints) {
  this->fs.create_file(
      this->fs.rooted("quick-lint-js.config"),
      [this]() -> fake_configuration_filesystem::read_file_result {
        return fake_configuration_filesystem::read_file_result::failure<
            read_file_io_error>(read_file_io_error{
            .path = this->fs.rooted("quick-lint-js.config").path(),
            .io_error = generic_file_io_error,
        });
      });
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer& notification_json) {
    EXPECT_TRUE(config.globals().find(u8"Array"sv))
        << "config should be default";
    EXPECT_FALSE(config.globals().find(u8"undeclaredVariable"sv))
        << "config should be default";
    notification_json.append_copy(
        u8R"({
          "method": "textDocument/publishDiagnostics",
          "params": {
            "uri": )" +
        string8(uri_json) +
        u8R"(,
            "version": )" +
        string8(version_json) +
        u8R"(,
            "diagnostics": []
          },
          "jsonrpc": "2.0"
        })");
  };

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "testjs"
          }
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"testjs"))
      << "should have linted despite config file being unloadable";

  ASSERT_EQ(this->client.messages.size(), 2);
  std::size_t showMessageIndex =
      this->client.messages[0]["method"] == "window/showMessage" ? 0 : 1;
  ::Json::Value& showMessageMessage = this->client.messages[showMessageIndex];
  EXPECT_EQ(showMessageMessage["method"], "window/showMessage");
  EXPECT_EQ(showMessageMessage["params"]["type"], lsp_warning_message_type);
  EXPECT_EQ(
      showMessageMessage["params"]["message"],
      this->config_file_load_error_message("test.js", "quick-lint-js.config"));
}

TEST_F(test_linting_lsp_server,
       opening_js_file_with_invalid_config_json_lints) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8"INVALID JSON"sv);
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer& notification_json) {
    EXPECT_TRUE(config.globals().find(u8"Array"sv))
        << "config should be default";
    EXPECT_FALSE(config.globals().find(u8"undeclaredVariable"sv))
        << "config should be default";
    notification_json.append_copy(
        u8R"({
          "method": "textDocument/publishDiagnostics",
          "params": {
            "uri": )" +
        string8(uri_json) +
        u8R"(,
            "version": )" +
        string8(version_json) +
        u8R"(,
            "diagnostics": []
          },
          "jsonrpc": "2.0"
        })");
  };

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "testjs"
          }
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"testjs"))
      << "should have linted despite config file being unloadable";

  ASSERT_EQ(this->client.messages.size(), 2);
  std::size_t showMessageIndex =
      this->client.messages[0]["method"] == "window/showMessage" ? 0 : 1;
  ::Json::Value& showMessageMessage = this->client.messages[showMessageIndex];
  EXPECT_EQ(showMessageMessage["method"], "window/showMessage");
  EXPECT_EQ(showMessageMessage["params"]["type"], lsp_warning_message_type);
  EXPECT_EQ(showMessageMessage["params"]["message"],
            "Problems found in the config file for "s +
                this->fs.rooted("test.js").c_str() + " (" +
                this->fs.rooted("quick-lint-js.config").c_str() + ").");
}

TEST_F(test_linting_lsp_server, making_config_file_unreadable_relints) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"configFromFilesystem": true}})");

  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "testjs"
          }
        }
      })"));

  this->fs.create_file(
      this->fs.rooted("quick-lint-js.config"),
      [this]() -> fake_configuration_filesystem::read_file_result {
        return fake_configuration_filesystem::read_file_result::failure<
            read_file_io_error>(read_file_io_error{
            .path = this->fs.rooted("quick-lint-js.config").path(),
            .io_error = generic_file_io_error,
        });
      });
  this->lint_callback = [&](configuration& config, padded_string_view,
                            string8_view uri_json, string8_view version_json,
                            byte_buffer& notification_json) {
    EXPECT_FALSE(config.globals().find(u8"configFromFilesystem"sv))
        << "config should be default";
    notification_json.append_copy(
        u8R"({
          "method": "textDocument/publishDiagnostics",
          "params": {
            "uri": )" +
        string8(uri_json) +
        u8R"(,
            "version": )" +
        string8(version_json) +
        u8R"(,
            "diagnostics": []
          },
          "jsonrpc": "2.0"
        })");
  };
  this->client.messages.clear();
  this->server.filesystem_changed();

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"testjs", u8"testjs"))
      << "should have linted twice: once on open, and once after config file "
         "changed";

  ASSERT_EQ(this->client.messages.size(), 2);
  std::size_t showMessageIndex =
      this->client.messages[0]["method"] == "window/showMessage" ? 0 : 1;
  ::Json::Value& showMessageMessage = this->client.messages[showMessageIndex];
  EXPECT_EQ(showMessageMessage["method"], "window/showMessage");
  EXPECT_EQ(showMessageMessage["params"]["type"], lsp_warning_message_type);
  EXPECT_EQ(
      showMessageMessage["params"]["message"],
      this->config_file_load_error_message("test.js", "quick-lint-js.config"));
}

TEST_F(test_linting_lsp_server, opening_broken_config_file_shows_diagnostics) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "json",
            "version": 1,
            "text": "THIS IS INVALID JSON"
          }
        }
      })"));

  ASSERT_EQ(this->client.messages.size(), 1);
  ::Json::Value& response = this->client.messages[0];
  EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
  EXPECT_FALSE(response.isMember("error"));
  // LSP PublishDiagnosticsParams:
  EXPECT_EQ(response["params"]["uri"],
            to_string(this->fs.file_uri_prefix_8() + u8"quick-lint-js.config"));
  EXPECT_EQ(response["params"]["version"], 1);
  ::Json::Value& diagnostics = response["params"]["diagnostics"];
  EXPECT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["character"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["severity"], lsp_error_severity);
  EXPECT_EQ(diagnostics[0]["message"], "JSON syntax error");
}

TEST_F(test_linting_lsp_server,
       introducing_config_file_error_shows_diagnostics) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "languageId": "json",
            "version": 1,
            "text": "{ \"globals\": {} }"
          }
        }
      })"));

  this->client.messages.clear();
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() + u8R"(quick-lint-js.config",
            "version": 2
          },
          "contentChanges": [
            {
              "text": "INVALID JSON"
            }
          ]
        }
      })"));

  ASSERT_EQ(this->client.messages.size(), 1);
  ::Json::Value& response = this->client.messages[0];
  EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
  EXPECT_FALSE(response.isMember("error"));
  // LSP PublishDiagnosticsParams:
  EXPECT_EQ(response["params"]["uri"],
            to_string(this->fs.file_uri_prefix_8() + u8"quick-lint-js.config"));
  EXPECT_EQ(response["params"]["version"], 2);
  ::Json::Value& diagnostics = response["params"]["diagnostics"];
  EXPECT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["character"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["severity"], lsp_error_severity);
  EXPECT_EQ(diagnostics[0]["message"], "JSON syntax error");
}

TEST_F(test_linting_lsp_server,
       changing_non_javascript_document_produces_no_lint) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test-fake.js",
            "languageId": "coffeescript",
            "version": 10,
            "text": ""
          }
        }
      })"));
  this->lint_calls.clear();
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///test-fake.js",
            "version": 11
          },
          "contentChanges": [
            {
              "text": "let x = x;"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls, IsEmpty());
}

TEST_F(test_linting_lsp_server, json_file_which_is_not_config_file_is_ignored) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")" +
                   this->fs.file_uri_prefix_8() +
                   u8R"(not-quick-lint-js.config",
            "languageId": "json",
            "version": 1,
            "text": "THIS IS INVALID JSON"
          }
        }
      })"));

  EXPECT_THAT(this->client.messages, IsEmpty());
  EXPECT_THAT(this->lint_calls, IsEmpty());
}

TEST_F(test_linting_lsp_server,
       opening_non_javascript_file_does_not_cause_diagnostics) {
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

  EXPECT_THAT(this->lint_calls, IsEmpty());
}

TEST_F(test_linting_lsp_server,
       closing_non_javascript_and_opening_javascript_lints) {
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "languageId": "html",
            "version": 10,
            "text": "let x = x;"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": "file:///test.js"
          }
        }
      })"));
  this->lint_calls.clear();
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "languageId": "javascript",
            "version": 11,
            "text": "let x = x;"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "version": 12
          },
          "contentChanges": [
            {
              "text": "let y = y;"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls, ElementsAre(u8"let x = x;", u8"let y = y;"));
}

TEST_F(test_linting_lsp_server,
       closing_javascript_and_opening_non_javascript_does_not_lint) {
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
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": "file:///test.js"
          }
        }
      })"));
  this->lint_calls.clear();
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "languageId": "html",
            "version": 11,
            "text": "let x = x;"
          }
        }
      })"));
  this->server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "version": 12
          },
          "contentChanges": [
            {
              "text": "let y = y;"
            }
          ]
        }
      })"));

  EXPECT_THAT(this->lint_calls, IsEmpty());
}

TEST(test_lsp_javascript_linter, linting_gives_diagnostics) {
  padded_string code(u8"let x = x;"_sv);
  byte_buffer notification_json_buffer;
  configuration config;

  lsp_javascript_linter linter;
  linter.lint_and_get_diagnostics_notification(
      config, &code, u8"\"file:///test.js\""sv, u8"10"sv,
      notification_json_buffer);

  std::string notification_json;
  notification_json.resize(notification_json_buffer.size());
  notification_json_buffer.copy_to(notification_json.data());

  ::Json::Value notification = parse_json(notification_json);
  EXPECT_EQ(notification["method"], "textDocument/publishDiagnostics");
  EXPECT_FALSE(notification.isMember("error"));
  // LSP PublishDiagnosticsParams:
  EXPECT_EQ(notification["params"]["uri"], "file:///test.js");
  EXPECT_EQ(notification["params"]["version"], 10);
  ::Json::Value& diagnostics = notification["params"]["diagnostics"];
  EXPECT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["start"]["character"], 8);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["line"], 0);
  EXPECT_EQ(diagnostics[0]["range"]["end"]["character"], 9);
  EXPECT_EQ(diagnostics[0]["severity"], lsp_error_severity);
  EXPECT_EQ(diagnostics[0]["message"], "variable used before declaration: x");
  EXPECT_EQ(diagnostics[0]["source"], "quick-lint-js");
}

TEST(test_lsp_javascript_linter, linting_does_not_desync) {
  // In the past, quick-lint-js' parser mutated the document. This caused
  // LSP-induced changes to edit the mutated document, desyncing the server from
  // the client. This test triggers what used to cause document mutation
  // (an identifier with an escape sequence), and makes sure desyncing doesn't
  // happen.

  fake_configuration_filesystem fs;
  lsp_endpoint<linting_lsp_server_handler<lsp_javascript_linter>,
               spy_lsp_endpoint_remote>
      server(std::forward_as_tuple(&fs), std::forward_as_tuple());
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "languageId": "javascript",
            "version": 10,
            "text": "let \\u{79}; x;"
          }
        }
      })"));

  {
    ASSERT_EQ(server.remote().messages.size(), 1);
    ::Json::Value& response = server.remote().messages[0];
    EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
    // LSP PublishDiagnosticsParams:
    ::Json::Value& diagnostics = response["params"]["diagnostics"];
    EXPECT_EQ(diagnostics.size(), 1) << "'x' should be undeclared";
  }

  server.remote().messages.clear();

  // Change "\u{79}" ("y") to "\u{78}" ("x").
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///test2.js",
            "version": 11
          },
          "contentChanges": [
            {
              "range": {
                "start": {"line": 0, "character": 8},
                "end": {"line": 0, "character": 9}
              },
              "text": "8"
            }
          ]
        }
      })"));

  {
    ASSERT_EQ(server.remote().messages.size(), 1);
    ::Json::Value& response = server.remote().messages[0];
    EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
    // LSP PublishDiagnosticsParams:
    ::Json::Value& diagnostics = response["params"]["diagnostics"];
    EXPECT_EQ(diagnostics.size(), 0) << "'x' should be declared";
  }
}

// TODO(strager): For batch requests containing multiple edits, lint and publish
// diagnostics only once.
}
}

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
