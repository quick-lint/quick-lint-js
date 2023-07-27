// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <boost/json.hpp>
#include <boost/json/value.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/boost-json.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/heap-function.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fake-configuration-filesystem.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/lsp-support.h>
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/lsp/outgoing-json-rpc-message-queue.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/spy-lsp-endpoint-remote.h>
#include <quick-lint-js/version.h>
#include <tuple>
#include <utility>

QLJS_WARNING_IGNORE_CLANG("-Wcovered-switch-default")
QLJS_WARNING_IGNORE_CLANG("-Wunused-member-function")

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using namespace std::literals::string_literals;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
constexpr int lsp_error_severity = 1;

constexpr int lsp_warning_message_type = 2;

#if QLJS_HAVE_WINDOWS_H
Windows_File_IO_Error generic_file_io_error = {ERROR_READ_FAULT};
#endif
#if QLJS_HAVE_UNISTD_H
POSIX_File_IO_Error generic_file_io_error = {EIO};
#endif

class Mock_LSP_Linter final : public LSP_Linter {
 public:
  using Lint_And_Get_Diagnostics_Notification_Type =
      void(Configuration&, Linter_Options, Padded_String_View code,
           String8_View uri_json, String8_View version_json,
           Outgoing_JSON_RPC_Message_Queue& outgoing_messages);

  explicit Mock_LSP_Linter() = default;

  explicit Mock_LSP_Linter(
      Heap_Function<Lint_And_Get_Diagnostics_Notification_Type> callback)
      : callback_(std::move(callback)) {}

  Mock_LSP_Linter(Mock_LSP_Linter&&) = default;
  Mock_LSP_Linter& operator=(Mock_LSP_Linter&&) = default;

  ~Mock_LSP_Linter() override = default;

  void lint(Configuration& config, Linter_Options lint_options,
            Padded_String_View code, String8_View uri_json,
            String8_View version_json,
            Outgoing_JSON_RPC_Message_Queue& outgoing_messages) override {
    this->callback_(config, lint_options, code, uri_json, version_json,
                    outgoing_messages);
  }

 private:
  Heap_Function<Lint_And_Get_Diagnostics_Notification_Type> callback_;
};

class Test_Linting_LSP_Server : public ::testing::Test, public Filesystem_Test {
 public:
  explicit Test_Linting_LSP_Server() { this->reset(); }

  void reset() {
    this->lint_callback = {};
    this->lint_calls.clear();
    this->fs.clear();
    this->linter = Mock_LSP_Linter(
        [this](Configuration& config, Linter_Options lint_options,
               Padded_String_View code, String8_View uri_json,
               String8_View version_json,
               Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
          this->lint_calls.emplace_back(code.string_view());
          if (this->lint_callback) {
            this->lint_callback(config, lint_options, code, uri_json,
                                version_json, outgoing_messages);
          }
        });
    this->handler =
        std::make_unique<Linting_LSP_Server_Handler>(&this->fs, &this->linter);
    this->client = std::make_unique<Spy_LSP_Endpoint_Remote>();
    this->server =
        std::make_unique<LSP_JSON_RPC_Message_Parser>(this->handler.get());
  }

  Heap_Function<void(Configuration&, Linter_Options, Padded_String_View code,
                     String8_View uri_json, String8_View version,
                     Outgoing_JSON_RPC_Message_Queue& outgoing_messages)>
      lint_callback;
  std::vector<String8> lint_calls;

  Fake_Configuration_Filesystem fs;

  Mock_LSP_Linter linter;
  std::unique_ptr<Linting_LSP_Server_Handler> handler;
  std::unique_ptr<Spy_LSP_Endpoint_Remote> client;
  std::unique_ptr<LSP_JSON_RPC_Message_Parser> server;

  std::string config_file_load_error_message(const char* js_path,
                                             const char* error_path) {
    return concat("Failed to load configuration file for "sv,
                  this->fs.rooted(js_path).path(),
                  ". "
                  "Using default configuration.\n"
                  "Error details: failed to read from "sv,
                  this->fs.rooted(error_path).path(), ": "sv,
                  generic_file_io_error.to_string());
  }
};

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
TEST_F(Test_Linting_LSP_Server, initialize) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
          "processId": null,
          "rootUri": null,
          "capabilities": {}
        }
      })"_sv));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> responses = this->client->responses();
  ASSERT_EQ(responses.size(), 1);
  ::boost::json::object response = responses[0];
  EXPECT_EQ(response["id"], 1);
  EXPECT_FALSE(response.contains("error"));
  // LSP InitializeResult:
  EXPECT_THAT(
      look_up(response, "result", "capabilities", "textDocumentSync", "change"),
      2);
  EXPECT_EQ(look_up(response, "result", "capabilities", "textDocumentSync",
                    "openClose"),
            true);
  EXPECT_EQ(look_up(response, "result", "serverInfo", "name"), "quick-lint-js");
  EXPECT_EQ(look_up(response, "result", "serverInfo", "version"),
            QUICK_LINT_JS_VERSION_STRING);

  EXPECT_THAT(this->client->requests(), IsEmpty())
      << "VS Code and other clients do not support server-to-client requests "
         "before the initialized notification";
  EXPECT_THAT(this->client->notifications(), IsEmpty())
      << "VS Code and other clients do not support server-to-client "
         "notifications before the initialized notification";
}

// For the "id" field of a request, JSON-RPC allows numbers, strings, and null.
TEST_F(Test_Linting_LSP_Server, initialize_with_different_request_ids) {
  struct Test_Case {
    String8_View id_json;
    ::boost::json::value id;
  };

  // TODO(strager): Support numbers with fractional parts, such as 12.34.
  for (const Test_Case& test : {
           Test_Case{u8"null"_sv, ::boost::json::value()},
           Test_Case{u8"1"_sv, ::boost::json::value(1)},
           Test_Case{u8"9007199254740991"_sv,
                     ::boost::json::value(std::int64_t{9007199254740991LL})},
           Test_Case{u8"-12345"_sv, ::boost::json::value(-12345)},
           Test_Case{u8R"("A")"_sv, ::boost::json::value("A")},
           Test_Case{u8R"("id value goes \"here\"")"_sv,
                     ::boost::json::value("id value goes \"here\"")},
           Test_Case{u8R"("id value goes \"here\"")"_sv,
                     ::boost::json::value("id value goes \"here\"")},
       }) {
    SCOPED_TRACE(out_string8(test.id_json));

    this->reset();

    this->server->append(
        make_message(concat(u8R"({
          "jsonrpc": "2.0",
          "id": )"_sv,
                            test.id_json,
                            u8R"(,
          "method": "initialize",
          "params": {
            "processId": null,
            "rootUri": null,
            "capabilities": {}
          }
        })"_sv)));
    this->server->flush_error_responses(*this->client);
    this->handler->flush_pending_notifications(*this->client);

    std::vector<::boost::json::object> responses = this->client->responses();
    ASSERT_EQ(responses.size(), 1);
    EXPECT_EQ(responses[0]["id"], test.id);
  }
}

TEST_F(Test_Linting_LSP_Server, server_ignores_initialized_notification) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
      })"_sv));
  this->server->flush_error_responses(*this->client);

  EXPECT_THAT(this->client->messages, IsEmpty());
}

TEST_F(Test_Linting_LSP_Server, loads_config_after_client_initialization) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
      })"_sv));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> requests = this->client->requests();
  ASSERT_EQ(requests.size(), 1);
  ::boost::json::object request = requests[0];
  EXPECT_EQ(look_up(request, "method"), "workspace/configuration");

  std::vector<std::string> requested_sections;
  ::boost::json::array items = look_up(request, "params", "items").as_array();
  for (const ::boost::json::value& item : items) {
    std::string section(to_string_view(look_up(item, "section").get_string()));
    requested_sections.push_back(std::move(section));
  }
  EXPECT_THAT(requested_sections,
              ::testing::Contains("quick-lint-js.tracing-directory"));
}

TEST_F(Test_Linting_LSP_Server, stores_config_values_after_config_response) {
  // Trigger a config request.
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
      })"_sv));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> requests = this->client->requests();
  ASSERT_EQ(requests.size(), 1);
  ::boost::json::object config_request = requests[0];
  ::boost::json::value config_request_id = look_up(config_request, "id");
  EXPECT_EQ(look_up(config_request, "method"), "workspace/configuration");

  ::boost::json::array config_response_params;
  ::boost::json::array items =
      look_up(config_request, "params", "items").as_array();
  for (const ::boost::json::value& item : items) {
    std::string section(to_string_view(look_up(item, "section").get_string()));
    if (section == "quick-lint-js.tracing-directory") {
      config_response_params.push_back("/test/tracing/dir");
    } else {
      config_response_params.push_back(nullptr);
    }
  }

  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "id": )"_sv,
                          json_to_string8(config_request_id),
                          u8R"(,
        "result": )"_sv,
                          json_to_string8(config_response_params),
                          u8R"(
      })"_sv)));

  Linting_LSP_Server_Config& config = this->handler->server_config();
  EXPECT_EQ(config.tracing_directory, "/test/tracing/dir");
}

TEST_F(Test_Linting_LSP_Server, config_can_be_set_with_initialize_request) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 100,
        "method": "initialize",
        "params": {
          "processId": null,
          "rootUri": null,
          "capabilities": {},
          "initializationOptions": {
            "configuration": {
              "quick-lint-js.tracing-directory": "/initial-tracing-directory"
            }
          }
        }
      })"_sv));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> responses = this->client->responses();
  ASSERT_EQ(responses.size(), 1);
  EXPECT_EQ(responses[0]["id"], 100);

  Linting_LSP_Server_Config& config = this->handler->server_config();
  EXPECT_EQ(config.tracing_directory, "/initial-tracing-directory");
}

TEST_F(Test_Linting_LSP_Server, did_change_configuration_notification) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "workspace/didChangeConfiguration",
        "params": {
          "settings": {
            "quick-lint-js.tracing-directory": "/test/tracing/dir"
          }
        }
      })"_sv));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  EXPECT_THAT(this->client->messages, IsEmpty());
  Linting_LSP_Server_Config& config = this->handler->server_config();
  EXPECT_EQ(config.tracing_directory, "/test/tracing/dir");
}

TEST_F(Test_Linting_LSP_Server,
       changing_config_to_same_tracing_dir_does_not_reset_tracing) {
  std::string temp_dir = this->make_temporary_directory();
  Fake_Configuration_Filesystem fs;
  Mock_LSP_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  LSP_JSON_RPC_Message_Parser server(&handler);

  server.append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "workspace/didChangeConfiguration",
        "params": {
          "settings": {
            "quick-lint-js.tracing-directory": )"_sv,
                          json_to_string8(::boost::json::string(temp_dir)),
                          u8R"(
          }
        }
      })"_sv)));

  std::vector<std::string> original_children =
      list_files_in_directory(temp_dir);
  EXPECT_THAT(original_children, ElementsAreArray({::testing::_}))
      << "enabling tracing in " << temp_dir
      << " should create a trace subdirectory";

  server.append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "workspace/didChangeConfiguration",
        "params": {
          "settings": {
            "quick-lint-js.tracing-directory": )"_sv,
                          json_to_string8(::boost::json::string(temp_dir)),
                          u8R"(
          }
        }
      })"_sv)));

  std::vector<std::string> new_children = list_files_in_directory(temp_dir);
  EXPECT_THAT(new_children, original_children)
      << "enabling tracing in " << temp_dir
      << " should not create another trace subdirectory";
}

TEST_F(Test_Linting_LSP_Server,
       changing_config_to_different_tracing_dir_resets_tracing) {
  Fake_Configuration_Filesystem fs;
  Mock_LSP_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  LSP_JSON_RPC_Message_Parser server(&handler);

  std::string original_tracing_dir = this->make_temporary_directory();
  std::string new_tracing_dir = this->make_temporary_directory();

  server.append(make_message(
      concat(u8R"({
        "jsonrpc": "2.0",
        "method": "workspace/didChangeConfiguration",
        "params": {
          "settings": {
            "quick-lint-js.tracing-directory": )"_sv,
             json_to_string8(::boost::json::string(original_tracing_dir)),
             u8R"(
          }
        }
      })"_sv)));
  server.append(make_message(
      concat(u8R"({
        "jsonrpc": "2.0",
        "method": "workspace/didChangeConfiguration",
        "params": {
          "settings": {
            "quick-lint-js.tracing-directory": )"_sv,
             json_to_string8(::boost::json::string(new_tracing_dir)),
             u8R"(
          }
        }
      })"_sv)));

  std::vector<std::string> new_children =
      list_files_in_directory(new_tracing_dir);
  EXPECT_THAT(new_children, ElementsAreArray({::testing::_}))
      << "changing tracing dir from " << original_tracing_dir << " to "
      << new_tracing_dir << " should create a new trace subdirectory";
}

TEST_F(Test_Linting_LSP_Server,
       changing_tracing_dir_config_to_empty_disables_tracing) {
  Trace_Flusher& tracer = *Trace_Flusher::instance();
  ASSERT_FALSE(tracer.is_enabled());

  std::string temp_dir = this->make_temporary_directory();
  Fake_Configuration_Filesystem fs;
  Mock_LSP_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  LSP_JSON_RPC_Message_Parser server(&handler);

  server.append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "workspace/didChangeConfiguration",
        "params": {
          "settings": {
            "quick-lint-js.tracing-directory": )"_sv,
                          json_to_string8(::boost::json::string(temp_dir)),
                          u8R"(
          }
        }
      })"_sv)));
  EXPECT_TRUE(tracer.is_enabled());
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "workspace/didChangeConfiguration",
        "params": {
          "settings": {
            "quick-lint-js.tracing-directory": ""
          }
        }
      })"_sv));
  EXPECT_FALSE(tracer.is_enabled());
}

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#shutdown
TEST_F(Test_Linting_LSP_Server, shutdown) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "shutdown"
      })"_sv));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> responses = this->client->responses();
  ASSERT_EQ(responses.size(), 1);
  ::boost::json::object response = responses[0];
  EXPECT_EQ(response["id"], 10);
  EXPECT_FALSE(response.contains("error"));
  EXPECT_EQ(response["result"], ::boost::json::value());
}

#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST
// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#exit
TEST_F(Test_Linting_LSP_Server,
       exit_without_shutdown_quits_program_with_exit_code_1_SLOW) {
  auto send_exit = [this]() {
    this->server->append(
        make_message(u8R"({
          "jsonrpc": "2.0",
          "method": "exit"
        })"_sv));
    std::exit(99);  // Shouldn't happen.
  };
  EXPECT_EXIT({ send_exit(); }, ::testing::ExitedWithCode(1), "");
}
#endif

#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST
// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#exit
TEST_F(Test_Linting_LSP_Server,
       exit_with_shutdown_quits_program_with_exit_code_0_SLOW) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "shutdown"
      })"_sv));
  this->client->messages.clear();

  auto send_exit = [this]() {
    this->server->append(
        make_message(u8R"({
          "jsonrpc": "2.0",
          "method": "exit"
        })"_sv));
    std::exit(99);  // Shouldn't happen.
  };
  EXPECT_EXIT({ send_exit(); }, ::testing::ExitedWithCode(0), "");
}
#endif

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#dollarRequests
TEST_F(Test_Linting_LSP_Server, dollar_notifications_are_ignored) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "$/someNotification"
      })"_sv));
  this->server->flush_error_responses(*this->client);
  EXPECT_THAT(this->client->messages, IsEmpty());
}

TEST_F(Test_Linting_LSP_Server, opening_document_lints) {
  this->lint_callback =
      [&](Configuration&, Linter_Options, Padded_String_View code,
          String8_View uri_json, String8_View version,
          Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
        EXPECT_EQ(code, u8"let x = x;"_sv);
        EXPECT_EQ(uri_json, u8"\"file:///test.js\""_sv);
        EXPECT_EQ(version, u8"10"_sv);

        Byte_Buffer& notification_json = outgoing_messages.new_message();
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
              )--"_sv);
      };

  this->server->append(
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
        })"_sv));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_EQ(notifications.size(), 1);
  ::boost::json::object response = notifications[0];
  EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
  EXPECT_FALSE(response.contains("error"));
  // LSP PublishDiagnosticsParams:
  EXPECT_EQ(look_up(response, "params", "uri"), "file:///test.js");
  EXPECT_EQ(look_up(response, "params", "version"), 10);
  ::boost::json::array diagnostics =
      look_up(response, "params", "diagnostics").as_array();
  EXPECT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "character"), 8);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "character"), 9);
  EXPECT_EQ(look_up(diagnostics, 0, "severity"), lsp_error_severity);
  EXPECT_EQ(look_up(diagnostics, 0, "message"),
            "variable used before declaration: x");

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"let x = x;"}));
}

TEST_F(Test_Linting_LSP_Server, javascript_language_ids_enable_jsx) {
  for (String8_View language_id :
       {u8"javascript"_sv, u8"js"_sv, u8"javascriptreact"_sv, u8"js-jsx"_sv}) {
    SCOPED_TRACE(out_string8(language_id));
    this->reset();

    this->lint_callback = [&](Configuration&, Linter_Options lint_options,
                              Padded_String_View, String8_View, String8_View,
                              Outgoing_JSON_RPC_Message_Queue&) {
      EXPECT_TRUE(lint_options.jsx);
      EXPECT_FALSE(lint_options.typescript);
    };

    this->server->append(
        make_message(concat(u8R"({
          "jsonrpc": "2.0",
          "method": "textDocument/didOpen",
          "params": {
            "textDocument": {
              "uri": "file:///test.js",
              "languageId": ")"_sv,
                            language_id,
                            u8R"(",
              "version": 10,
              "text": "code goes here"
            }
          }
        })"_sv)));
    EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"code goes here"}));
  }
}

TEST_F(Test_Linting_LSP_Server, typescript_language_ids_enable_typescript) {
  for (String8_View language_id : {u8"typescript"_sv}) {
    SCOPED_TRACE(out_string8(language_id));
    this->reset();

    this->lint_callback = [&](Configuration&, Linter_Options lint_options,
                              Padded_String_View, String8_View, String8_View,
                              Outgoing_JSON_RPC_Message_Queue&) {
      EXPECT_TRUE(lint_options.typescript);
      EXPECT_FALSE(lint_options.jsx);
    };

    this->server->append(
        make_message(concat(u8R"({
          "jsonrpc": "2.0",
          "method": "textDocument/didOpen",
          "params": {
            "textDocument": {
              "uri": "file:///test.ts",
              "languageId": ")"_sv,
                            language_id,
                            u8R"(",
              "version": 10,
              "text": "code goes here"
            }
          }
        })"_sv)));
    EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"code goes here"}));
  }
}

TEST_F(Test_Linting_LSP_Server, tsx_language_ids_enable_typescript_jsx) {
  for (String8_View language_id : {u8"typescriptreact"_sv, u8"tsx"_sv}) {
    SCOPED_TRACE(out_string8(language_id));
    this->reset();

    this->lint_callback = [&](Configuration&, Linter_Options lint_options,
                              Padded_String_View, String8_View, String8_View,
                              Outgoing_JSON_RPC_Message_Queue&) {
      EXPECT_TRUE(lint_options.typescript);
      EXPECT_TRUE(lint_options.jsx);
    };

    this->server->append(
        make_message(concat(u8R"({
          "jsonrpc": "2.0",
          "method": "textDocument/didOpen",
          "params": {
            "textDocument": {
              "uri": "file:///test.tsx",
              "languageId": ")"_sv,
                            language_id,
                            u8R"(",
              "version": 10,
              "text": "code goes here"
            }
          }
        })"_sv)));
    EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"code goes here"}));
  }
}

TEST_F(Test_Linting_LSP_Server, changing_document_with_full_text_lints) {
  this->server->append(
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
      })"_sv));
  this->lint_calls.clear();
  this->server->append(
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
      })"_sv));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"SECOND"}));
}

TEST_F(Test_Linting_LSP_Server,
       changing_document_with_single_incremental_edit_lints) {
  this->server->append(
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
      })"_sv));
  this->lint_calls.clear();

  this->server->append(
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
      })"_sv));
  this->server->append(
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
      })"_sv));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"the slow brown fox",
                                                  u8"the slow purple fox"}));
}

TEST_F(Test_Linting_LSP_Server,
       changing_document_with_multiple_incremental_edits_lints_only_once) {
  this->server->append(
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
      })"_sv));
  this->lint_calls.clear();

  this->server->append(
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
      })"_sv));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"the slow purple fox"}));
}

TEST_F(Test_Linting_LSP_Server, linting_uses_config_from_file) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"testGlobalVariable": true}})"_sv);

  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View, String8_View,
                            Outgoing_JSON_RPC_Message_Queue&) {
    EXPECT_TRUE(config.globals().find(u8"testGlobalVariable"_sv));
  };

  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8""}));
}

TEST_F(
    Test_Linting_LSP_Server,
    open_then_close_then_open_js_file_then_modify_config_file_lints_js_file) {
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "json",
            "version": 1,
            "text": "{\"globals\": {\"testGlobalVariableBefore\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));

  this->lint_calls.clear();
  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View, String8_View,
                            Outgoing_JSON_RPC_Message_Queue&) {
    EXPECT_FALSE(config.globals().find(u8"testGlobalVariableBefore"_sv));
    EXPECT_TRUE(config.globals().find(u8"testGlobalVariableAfter"_sv));
  };
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "version": 2
          },
          "contentChanges": [
            {
              "text": "{\"globals\": {\"testGlobalVariableAfter\": true}}"
            }
          ]
        }
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8""}));
}

TEST_F(Test_Linting_LSP_Server,
       linting_uses_config_from_file_with_special_chars_in_document_uri) {
  this->fs.create_file(this->fs.rooted("a%b~/quick-lint-js.config"),
                       u8R"({"globals": {"testGlobalVariable": true}})"_sv);

  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View, String8_View,
                            Outgoing_JSON_RPC_Message_Queue&) {
    EXPECT_TRUE(config.globals().find(u8"testGlobalVariable"_sv));
  };

  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(a%25b%7E/%E2%98%83.js",
            "languageId": "javascript",
            "version": 10,
            "text": "snowman"
          }
        }
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"snowman"}));
}

TEST_F(Test_Linting_LSP_Server, linting_uses_already_opened_config_file) {
  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View, String8_View,
                            Outgoing_JSON_RPC_Message_Queue&) {
    EXPECT_TRUE(config.globals().find(u8"modified"_sv));
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"modified": false}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"modified\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8""}));
}

TEST_F(Test_Linting_LSP_Server,
       linting_uses_already_opened_shadowing_config_file) {
  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View, String8_View,
                            Outgoing_JSON_RPC_Message_Queue&) {
    EXPECT_TRUE(config.globals().find(u8"haveInnerConfig"_sv));
    EXPECT_FALSE(config.globals().find(u8"haveOuterConfig"_sv));
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"haveOuterConfig": false}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(inner/quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"haveInnerConfig\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(inner/test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8""}));
}

TEST_F(Test_Linting_LSP_Server, editing_config_relints_open_js_file) {
  bool after_config_was_loaded = false;

  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View uri_json,
                            String8_View version_json,
                            Outgoing_JSON_RPC_Message_Queue&) {
    if (config.globals().find(u8"after"_sv)) {
      EXPECT_FALSE(config.globals().find(u8"before"_sv));
      EXPECT_EQ(version_json, u8"10"_sv);
      EXPECT_EQ(uri_json, concat(u8"\""_sv, this->fs.file_uri_prefix_8(),
                                 u8"test.js\""_sv));
      after_config_was_loaded = true;
    }
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"before\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));
  // Change 'before' to 'after'.
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
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
      })"_sv)));

  EXPECT_TRUE(after_config_was_loaded);
}

TEST_F(Test_Linting_LSP_Server,
       editing_config_lints_latest_version_of_js_file) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"before\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "version": 10,
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "text": ""
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "version": 11,
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js"
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
      })"_sv)));

  this->lint_callback = [&](Configuration&, Linter_Options, Padded_String_View,
                            String8_View, String8_View version_json,
                            Outgoing_JSON_RPC_Message_Queue&) {
    EXPECT_EQ(version_json, u8"11"_sv);
  };
  this->lint_calls.clear();

  // Change 'before' to 'after'.
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
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
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"updated"_sv}));
}

TEST_F(Test_Linting_LSP_Server, editing_config_relints_many_open_js_files) {
  this->lint_callback =
      [&](Configuration&, Linter_Options, Padded_String_View,
          String8_View uri_json, String8_View version_json,
          Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
        Byte_Buffer& notification_json = outgoing_messages.new_message();
        notification_json.append_copy(
            concat(u8R"(
              {
                "method": "textDocument/publishDiagnostics",
                "params":{
                  "uri": )"_sv,
                   uri_json,
                   u8R"(,
                  "version": )"_sv,
                   version_json,
                   u8R"(,
                  "diagnostics": []
                },
                "jsonrpc":"2.0"
              }
            )"_sv));
      };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"before\": true}}"
          }
        }
      })"_sv)));

  for (String8_View js_file : {u8"a.js"_sv, u8"b.js"_sv, u8"c.js"_sv}) {
    this->server->append(
        make_message(concat(u8R"({
          "jsonrpc": "2.0",
          "method": "textDocument/didOpen",
          "params": {
            "textDocument": {
              "uri": ")"_sv,
                            this->fs.file_uri_prefix_8(), js_file,
                            u8R"(",
              "languageId": "javascript",
              "version": 10,
              "text": "/* )"_sv,
                            js_file,
                            u8R"( */"
            }
          }
        })"_sv)));
  }

  this->handler->flush_pending_notifications(*this->client);
  this->lint_calls.clear();
  this->client->messages.clear();
  // Change 'before' to 'after'.
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
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
      })"_sv)));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  EXPECT_THAT(this->lint_calls,
              ::testing::UnorderedElementsAreArray(
                  {u8"/* a.js */", u8"/* b.js */", u8"/* c.js */"}));

  std::vector<String8> linted_uris;
  for (const ::boost::json::object& notification :
       this->client->notifications()) {
    EXPECT_EQ(look_up(notification, "method"),
              "textDocument/publishDiagnostics");
    String8 uri(to_string8_view(
        to_string_view(look_up(notification, "params", "uri").get_string())));
    if (uri == this->fs.file_uri_prefix_8() + u8"quick-lint-js.config") {
      // Ignore.
      continue;
    }
    linted_uris.emplace_back(uri);
  }
  EXPECT_THAT(linted_uris, ::testing::UnorderedElementsAreArray({
                               this->fs.file_uri_prefix_8() + u8"a.js",
                               this->fs.file_uri_prefix_8() + u8"b.js",
                               this->fs.file_uri_prefix_8() + u8"c.js",
                           }));
}

TEST_F(Test_Linting_LSP_Server, editing_config_relints_only_affected_js_files) {
  this->lint_callback =
      [&](Configuration&, Linter_Options, Padded_String_View,
          String8_View uri_json, String8_View version_json,
          Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
        Byte_Buffer& notification_json = outgoing_messages.new_message();
        notification_json.append_copy(
            concat(u8R"(
              {
                "method": "textDocument/publishDiagnostics",
                "params":{
                  "uri": )"_sv,
                   uri_json,
                   u8R"(,
                  "version": )"_sv,
                   version_json,
                   u8R"(,
                  "diagnostics": []
                },
                "jsonrpc":"2.0"
              }
            )"_sv));
      };

  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(dir-a/quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"a\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(dir-b/quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"b\": true}}"
          }
        }
      })"_sv)));

  for (String8_View js_file : {u8"dir-a/test.js"_sv, u8"dir-b/test.js"_sv}) {
    this->server->append(
        make_message(concat(u8R"({
          "jsonrpc": "2.0",
          "method": "textDocument/didOpen",
          "params": {
            "textDocument": {
              "uri": ")"_sv,
                            this->fs.file_uri_prefix_8(), js_file,
                            u8R"(",
              "languageId": "javascript",
              "version": 10,
              "text": "/* )"_sv,
                            js_file,
                            u8R"( */"
            }
          }
        })"_sv)));
  }

  this->handler->flush_pending_notifications(*this->client);
  this->lint_calls.clear();
  this->client->messages.clear();
  // Change 'a' to 'A' in dir-a/quick-lint-js.config (but leave
  // dir-b/quick-lint-js.config as-is).
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
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
      })"_sv)));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"/* dir-a/test.js */"}));

  std::vector<std::string> linted_uris;
  for (const ::boost::json::object& notification :
       this->client->notifications()) {
    EXPECT_EQ(look_up(notification, "method"),
              "textDocument/publishDiagnostics");
    std::string uri(
        to_string_view(look_up(notification, "params", "uri").get_string()));
    if (uri == to_string(this->fs.file_uri_prefix_8() +
                         u8"dir-a/quick-lint-js.config") ||
        uri == to_string(this->fs.file_uri_prefix_8() +
                         u8"dir-b/quick-lint-js.config")) {
      // Ignore.
      continue;
    }
    linted_uris.emplace_back(uri);
  }
  EXPECT_THAT(linted_uris,
              ElementsAreArray({
                  to_string(this->fs.file_uri_prefix_8() + u8"dir-a/test.js"),
              }));
}

TEST_F(Test_Linting_LSP_Server,
       editing_js_file_after_shadowing_config_uses_latest_config) {
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"before\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(inner/test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "original"
          }
        }
      })"_sv)));

  // After opening test.js, create /inner/quick-lint-js.config which shadows
  // /quick-lint-js.config.
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(inner/quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"after\": true}}"
          }
        }
      })"_sv)));

  this->lint_calls.clear();
  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View,
                            String8_View version_json,
                            Outgoing_JSON_RPC_Message_Queue&) {
    EXPECT_EQ(version_json, u8"11"_sv);
    EXPECT_FALSE(config.globals().find(u8"before"_sv));
    EXPECT_TRUE(config.globals().find(u8"after"_sv));
  };

  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "version": 11,
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(inner/test.js"
          },
          "contentChanges": [
            {
              "text": "modified"
            }
          ]
        }
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"modified"}));
}

TEST_F(Test_Linting_LSP_Server, opening_config_relints_open_js_files) {
  bool after_config_was_loaded = false;

  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View uri_json,
                            String8_View version_json,
                            Outgoing_JSON_RPC_Message_Queue&) {
    if (config.globals().find(u8"after"_sv)) {
      EXPECT_FALSE(config.globals().find(u8"before"_sv));
      EXPECT_EQ(version_json, u8"10"_sv);
      EXPECT_EQ(uri_json, concat(u8"\""_sv, this->fs.file_uri_prefix_8(),
                                 u8"test.js\""_sv));
      after_config_was_loaded = true;
    }
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"after\": true}}"
          }
        }
      })"_sv)));

  EXPECT_TRUE(after_config_was_loaded);
}

TEST_F(Test_Linting_LSP_Server,
       changing_config_on_filesystem_relints_open_js_files) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"before": true}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));

  bool after_config_was_loaded = false;
  this->lint_callback =
      [&](Configuration& config, Linter_Options, Padded_String_View,
          String8_View uri_json, String8_View version_json,
          Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
        EXPECT_TRUE(config.globals().find(u8"after"_sv));
        EXPECT_FALSE(config.globals().find(u8"before"_sv));
        EXPECT_EQ(version_json, u8"10"_sv);
        EXPECT_EQ(uri_json, concat(u8"\""_sv, this->fs.file_uri_prefix_8(),
                                   u8"test.js\""_sv));
        after_config_was_loaded = true;

        Byte_Buffer& notification_json = outgoing_messages.new_message();
        notification_json.append_copy(
            concat(u8R"({
          "method": "textDocument/publishDiagnostics",
          "params": {
            "uri": ")"_sv,
                   this->fs.file_uri_prefix_8(),
                   u8R"(test.js",
            "version": 10,
            "diagnostics": []
          },
          "jsonrpc": "2.0"
        })"_sv));
      };
  this->client->messages.clear();

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"after": true}})"_sv);
  this->handler->filesystem_changed();
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  EXPECT_TRUE(after_config_was_loaded);

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_THAT(notifications, ElementsAreArray({::testing::_}));
  ::boost::json::object notification = notifications[0];
  EXPECT_EQ(notification["method"], "textDocument/publishDiagnostics");
}

TEST_F(
    Test_Linting_LSP_Server,
    linting_uses_config_from_filesystem_if_config_is_opened_then_closed_before_opening_js_file) {
  this->lint_callback = [&](Configuration& config, Linter_Options,
                            Padded_String_View, String8_View, String8_View,
                            Outgoing_JSON_RPC_Message_Queue&) {
    EXPECT_TRUE(config.globals().find(u8"v1"_sv));
    EXPECT_FALSE(config.globals().find(u8"v2"_sv));
  };

  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"v1": true}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"v2\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8""}));
}

TEST_F(Test_Linting_LSP_Server,
       closing_open_config_reloads_config_from_filesystem) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"configFromFilesystem": true}})"_sv);
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "plaintext",
            "version": 1,
            "text": "{\"globals\": {\"configFromLSP\": true}}"
          }
        }
      })"_sv)));
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": ""
          }
        }
      })"_sv)));

  this->lint_calls.clear();
  this->lint_callback =
      [&](Configuration& config, Linter_Options, Padded_String_View,
          String8_View, String8_View,
          Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
        EXPECT_TRUE(config.globals().find(u8"configFromFilesystem"_sv));
        EXPECT_FALSE(config.globals().find(u8"configFromLSP"_sv));
        Byte_Buffer& notification_json = outgoing_messages.new_message();
        notification_json.append_copy(
            concat(u8R"({
          "method": "textDocument/publishDiagnostics",
          "params": {
            "uri": ")"_sv,
                   this->fs.file_uri_prefix_8(),
                   u8R"(test.js",
            "version": 10,
            "diagnostics": []
          },
          "jsonrpc": "2.0"
        })"_sv));
      };
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config"
          }
        }
      })"_sv)));

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8""}));
}

TEST_F(Test_Linting_LSP_Server, opening_js_file_with_unreadable_config_lints) {
  this->fs.create_file(
      this->fs.rooted("quick-lint-js.config"),
      [this]() -> Fake_Configuration_Filesystem::Read_File_Result {
        return failed_result(Read_File_IO_Error{
            .path = this->fs.rooted("quick-lint-js.config").path(),
            .io_error = generic_file_io_error,
        });
      });
  this->lint_callback =
      [&](Configuration& config, Linter_Options, Padded_String_View,
          String8_View uri_json, String8_View version_json,
          Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
        EXPECT_TRUE(config.globals().find(u8"Array"_sv))
            << "config should be default";
        EXPECT_FALSE(config.globals().find(u8"undeclaredVariable"_sv))
            << "config should be default";
        Byte_Buffer& notification_json = outgoing_messages.new_message();
        notification_json.append_copy(
            concat(u8R"({
          "method": "textDocument/publishDiagnostics",
          "params": {
            "uri": )"_sv,
                   uri_json,
                   u8R"(,
            "version": )"_sv,
                   version_json,
                   u8R"(,
            "diagnostics": []
          },
          "jsonrpc": "2.0"
        })"_sv));
      };

  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "testjs"
          }
        }
      })"_sv)));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"testjs"}))
      << "should have linted despite config file being unloadable";

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_EQ(notifications.size(), 2);
  std::size_t showMessageIndex =
      look_up(notifications[0], "method") == "window/showMessage" ? 0 : 1;
  ::boost::json::object showMessageMessage = notifications[showMessageIndex];
  EXPECT_EQ(look_up(showMessageMessage, "method"), "window/showMessage");
  EXPECT_EQ(look_up(showMessageMessage, "params", "type"),
            lsp_warning_message_type);
  EXPECT_EQ(look_up(showMessageMessage, "params", "message"),
            to_boost_string_view(this->config_file_load_error_message(
                "test.js", "quick-lint-js.config")));
}

TEST_F(Test_Linting_LSP_Server,
       opening_js_file_with_invalid_config_json_lints) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8"INVALID JSON"_sv);
  this->lint_callback =
      [&](Configuration& config, Linter_Options, Padded_String_View,
          String8_View uri_json, String8_View version_json,
          Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
        EXPECT_TRUE(config.globals().find(u8"Array"_sv))
            << "config should be default";
        EXPECT_FALSE(config.globals().find(u8"undeclaredVariable"_sv))
            << "config should be default";
        Byte_Buffer& notification_json = outgoing_messages.new_message();
        notification_json.append_copy(
            concat(u8R"({
          "method": "textDocument/publishDiagnostics",
          "params": {
            "uri": )"_sv,
                   uri_json,
                   u8R"(,
            "version": )"_sv,
                   version_json,
                   u8R"(,
            "diagnostics": []
          },
          "jsonrpc": "2.0"
        })"_sv));
      };

  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "testjs"
          }
        }
      })"_sv)));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"testjs"}))
      << "should have linted despite config file being unloadable";

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_EQ(notifications.size(), 2);
  std::size_t showMessageIndex =
      look_up(notifications[0], "method") == "window/showMessage" ? 0 : 1;
  ::boost::json::object showMessageMessage = notifications[showMessageIndex];
  EXPECT_EQ(look_up(showMessageMessage, "method"), "window/showMessage");
  EXPECT_EQ(look_up(showMessageMessage, "params", "type"),
            lsp_warning_message_type);
  EXPECT_EQ(look_up(showMessageMessage, "params", "message"),
            to_boost_string_view(concat(
                "Problems found in the config file for "sv,
                this->fs.rooted("test.js").path(), " ("sv,
                this->fs.rooted("quick-lint-js.config").path(), ")."sv)));
}

TEST_F(Test_Linting_LSP_Server, making_config_file_unreadable_relints) {
  this->fs.create_file(this->fs.rooted("quick-lint-js.config"),
                       u8R"({"globals": {"configFromFilesystem": true}})"_sv);

  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "testjs"
          }
        }
      })"_sv)));

  this->fs.create_file(
      this->fs.rooted("quick-lint-js.config"),
      [this]() -> Fake_Configuration_Filesystem::Read_File_Result {
        return failed_result(Read_File_IO_Error{
            .path = this->fs.rooted("quick-lint-js.config").path(),
            .io_error = generic_file_io_error,
        });
      });
  this->lint_callback =
      [&](Configuration& config, Linter_Options, Padded_String_View,
          String8_View uri_json, String8_View version_json,
          Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
        EXPECT_FALSE(config.globals().find(u8"configFromFilesystem"_sv))
            << "config should be default";
        Byte_Buffer& notification_json = outgoing_messages.new_message();
        notification_json.append_copy(
            concat(u8R"({
          "method": "textDocument/publishDiagnostics",
          "params": {
            "uri": )"_sv,
                   uri_json,
                   u8R"(,
            "version": )"_sv,
                   version_json,
                   u8R"(,
            "diagnostics": []
          },
          "jsonrpc": "2.0"
        })"_sv));
      };
  this->client->messages.clear();
  this->handler->filesystem_changed();
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  EXPECT_THAT(this->lint_calls, ElementsAreArray({u8"testjs", u8"testjs"}))
      << "should have linted twice: once on open, and once after config file "
         "changed";

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_EQ(notifications.size(), 2);
  std::size_t showMessageIndex =
      look_up(notifications[0], "method") == "window/showMessage" ? 0 : 1;
  ::boost::json::object showMessageMessage = notifications[showMessageIndex];
  EXPECT_EQ(look_up(showMessageMessage, "method"), "window/showMessage");
  EXPECT_EQ(look_up(showMessageMessage, "params", "type"),
            lsp_warning_message_type);
  EXPECT_EQ(look_up(showMessageMessage, "params", "message"),
            to_boost_string_view(this->config_file_load_error_message(
                "test.js", "quick-lint-js.config")));
}

TEST_F(Test_Linting_LSP_Server, opening_broken_config_file_shows_diagnostics) {
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "json",
            "version": 1,
            "text": "THIS IS INVALID JSON"
          }
        }
      })"_sv)));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_EQ(notifications.size(), 1);
  ::boost::json::object response = notifications[0];
  EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
  EXPECT_FALSE(response.contains("error"));
  // LSP PublishDiagnosticsParams:
  EXPECT_EQ(look_up(response, "params", "uri"),
            to_boost_string_view(this->fs.file_uri_prefix_8() +
                                 u8"quick-lint-js.config"));
  EXPECT_EQ(look_up(response, "params", "version"), 1);
  ::boost::json::array diagnostics =
      look_up(response, "params", "diagnostics").as_array();
  EXPECT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "character"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "severity"), lsp_error_severity);
  EXPECT_EQ(look_up(diagnostics, 0, "message"), "JSON syntax error");
  EXPECT_EQ(look_up(diagnostics, 0, "code"), "E0164");
  EXPECT_EQ(look_up(diagnostics, 0, "codeDescription", "href"),
            "https://quick-lint-js.com/errors/E0164/");
}

TEST_F(Test_Linting_LSP_Server,
       introducing_config_file_error_shows_diagnostics) {
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "languageId": "json",
            "version": 1,
            "text": "{ \"globals\": {} }"
          }
        }
      })"_sv)));

  this->handler->flush_pending_notifications(*this->client);
  this->client->messages.clear();
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(quick-lint-js.config",
            "version": 2
          },
          "contentChanges": [
            {
              "text": "INVALID JSON"
            }
          ]
        }
      })"_sv)));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_EQ(notifications.size(), 1);
  ::boost::json::object response = notifications[0];
  EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
  EXPECT_FALSE(response.contains("error"));
  // LSP PublishDiagnosticsParams:
  EXPECT_EQ(look_up(response, "params", "uri"),
            to_boost_string_view(this->fs.file_uri_prefix_8() +
                                 u8"quick-lint-js.config"));
  EXPECT_EQ(look_up(response, "params", "version"), 2);
  ::boost::json::array diagnostics =
      look_up(response, "params", "diagnostics").as_array();
  EXPECT_EQ(diagnostics.size(), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "character"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "severity"), lsp_error_severity);
  EXPECT_EQ(look_up(diagnostics, 0, "message"), "JSON syntax error");
  EXPECT_EQ(look_up(diagnostics, 0, "code"), "E0164");
  EXPECT_EQ(look_up(diagnostics, 0, "codeDescription", "href"),
            "https://quick-lint-js.com/errors/E0164/");
}

TEST_F(Test_Linting_LSP_Server,
       changing_non_javascript_document_produces_no_lint) {
  this->server->append(
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
      })"_sv));
  this->lint_calls.clear();
  this->server->append(
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
      })"_sv));

  EXPECT_THAT(this->lint_calls, IsEmpty());
}

TEST_F(Test_Linting_LSP_Server, json_file_which_is_not_config_file_is_ignored) {
  this->server->append(
      make_message(concat(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": ")"_sv,
                          this->fs.file_uri_prefix_8(),
                          u8R"(not-quick-lint-js.config",
            "languageId": "json",
            "version": 1,
            "text": "THIS IS INVALID JSON"
          }
        }
      })"_sv)));
  this->server->flush_error_responses(*this->client);

  EXPECT_THAT(this->client->messages, IsEmpty());
  EXPECT_THAT(this->lint_calls, IsEmpty());
}

TEST_F(Test_Linting_LSP_Server,
       opening_non_javascript_file_does_not_cause_diagnostics) {
  this->server->append(
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
      })"_sv));

  EXPECT_THAT(this->lint_calls, IsEmpty());
}

TEST_F(Test_Linting_LSP_Server,
       closing_non_javascript_and_opening_javascript_lints) {
  this->server->append(
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
      })"_sv));
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": "file:///test.js"
          }
        }
      })"_sv));
  this->lint_calls.clear();
  this->server->append(
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
      })"_sv));
  this->server->append(
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
      })"_sv));

  EXPECT_THAT(this->lint_calls,
              ElementsAreArray({u8"let x = x;", u8"let y = y;"}));
}

TEST_F(Test_Linting_LSP_Server,
       closing_javascript_and_opening_non_javascript_does_not_lint) {
  this->server->append(
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
      })"_sv));
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didClose",
        "params": {
          "textDocument": {
            "uri": "file:///test.js"
          }
        }
      })"_sv));
  this->lint_calls.clear();
  this->server->append(
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
      })"_sv));
  this->server->append(
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
      })"_sv));

  EXPECT_THAT(this->lint_calls, IsEmpty());
}

TEST_F(Test_Linting_LSP_Server, showing_io_errors_shows_only_first) {
  this->handler->add_watch_io_errors(std::vector<Watch_IO_Error>{
      Watch_IO_Error{
          .path = "/banana",
          .io_error = generic_file_io_error,
      },
      Watch_IO_Error{
          .path = "/orange",
          .io_error = generic_file_io_error,
      },
  });
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_EQ(notifications.size(), 1);
  ::boost::json::object show_message_message = notifications[0];
  EXPECT_EQ(look_up(show_message_message, "method"), "window/showMessage");
  EXPECT_EQ(look_up(show_message_message, "params", "type"),
            lsp_warning_message_type);
  std::string message(to_string_view(
      look_up(show_message_message, "params", "message").as_string()));
  EXPECT_THAT(message, ::testing::HasSubstr("/banana"));
  EXPECT_THAT(message, ::testing::Not(::testing::HasSubstr("orange")));
}

TEST_F(Test_Linting_LSP_Server, showing_io_errors_shows_only_first_ever) {
  this->handler->add_watch_io_errors(std::vector<Watch_IO_Error>{
      Watch_IO_Error{
          .path = "/banana",
          .io_error = generic_file_io_error,
      },
  });
  this->handler->flush_pending_notifications(*this->client);
  // Separate call to add_watch_io_errors:
  this->handler->add_watch_io_errors(std::vector<Watch_IO_Error>{
      Watch_IO_Error{
          .path = "/orange",
          .io_error = generic_file_io_error,
      },
  });
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> notifications =
      this->client->notifications();
  ASSERT_EQ(notifications.size(), 1);
  ::boost::json::object show_message_message = notifications[0];
  std::string message(to_string_view(
      look_up(show_message_message, "params", "message").as_string()));
  EXPECT_THAT(message, ::testing::HasSubstr("/banana"));
  EXPECT_THAT(message, ::testing::Not(::testing::HasSubstr("orange")));
}

TEST_F(Test_Linting_LSP_Server, invalid_json_in_request) {
  for (
      String8_View message : {
          u8"{\"i\"0d,:\"result\":{\"capabilities\":{\"textDocumen|Sync\":{\"change\":2,\"openClose#:true}},\"serverInfo\":{\"name\":\"quick-lint"_sv,
          u8"[falsex]"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "mymethod", "id": xxx, "params": {} })"_sv,
      }) {
    SCOPED_TRACE(out_string8(message));
    this->reset();

    this->server->append(make_message(message));
    this->server->flush_error_responses(*this->client);

    ASSERT_EQ(this->client->messages.size(), 1);
    ::boost::json::value response = this->client->messages[0];
    expect_parse_error(response);
  }
}

TEST_F(Test_Linting_LSP_Server,
       unimplemented_method_in_notification_is_ignored) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/shinyNewMethod",
        "params": {}
      })"_sv));
  this->server->flush_error_responses(*this->client);
  EXPECT_THAT(this->client->messages, IsEmpty());
}

TEST_F(Test_Linting_LSP_Server, unimplemented_method_in_request_returns_error) {
  this->server->append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/shinyNewMethod",
        "id": 10,
        "params": {}
      })"_sv));
  this->server->flush_error_responses(*this->client);
  this->handler->flush_pending_notifications(*this->client);

  std::vector<::boost::json::object> responses = this->client->responses();
  ASSERT_EQ(responses.size(), 1);
  ::boost::json::object response = responses[0];
  EXPECT_EQ(look_up(response, "id"), 10);
  expect_error(response, -32601, "Method not found");
}

TEST_F(Test_Linting_LSP_Server, invalid_request_returns_error) {
  for (
      String8_View message : {
          u8R"({ "jsonrpc": "2.0", "method": null, "id": 10, "params": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": null, "params": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "mymethod", "id": true, "params": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "mymethod", "id": [], "params": {} })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "mymethod", "id": {}, "params": {} })"_sv,
      }) {
    SCOPED_TRACE(out_string8(message));
    this->reset();

    this->server->append(make_message(message));
    this->server->flush_error_responses(*this->client);

    ASSERT_EQ(this->client->messages.size(), 1);
    ::boost::json::value response = this->client->messages[0];
    expect_error(response, -32600, "Invalid Request");
    EXPECT_EQ(look_up(response, "id"), ::boost::json::value());
  }
}

TEST_F(Test_Linting_LSP_Server, invalid_notification_is_ignored) {
  for (
      String8_View message : {
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didOpen" })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didOpen", "params": { "textDocument": null } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didOpen", "params": { "textDocument": {} } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didOpen", "params": { "textDocument": { "languageId": "javascript" } } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didOpen", "params": { "textDocument": { "languageId": "javascript", "uri": null } } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didOpen", "params": { "textDocument": { "languageId": "javascript", "uri": "file:///new.js" } } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didOpen", "params": { "textDocument": { "languageId": "javascript", "uri": "file:///new.js", "version": 1 } } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didClose" })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange" })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange", "params": { "textDocument": {} } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange", "params": { "textDocument": { "uri": "file:///test.js" } } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange", "params": { "textDocument": { "uri": "file:///test.js", "version": 2 } } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange", "params": { "textDocument": { "uri": "file:///test.js", "version": 2 }, "contentChanges": [ {} ] } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange", "params": { "textDocument": { "uri": "file:///test.js", "version": 2 }, "contentChanges": [ { "text": "", "range": { "start": { "line": null, "character": 0 }, "end": { "line": 0, "character": 0 } } } ] } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange", "params": { "textDocument": { "uri": "file:///test.js", "version": 2 }, "contentChanges": [ { "text": "", "range": { "start": { "line": 0, "character": null }, "end": { "line": 0, "character": 0 } } } ] } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange", "params": { "textDocument": { "uri": "file:///test.js", "version": 2 }, "contentChanges": [ { "text": "", "range": { "start": { "line": 0, "character": 0 }, "end": { "line": null, "character": 0 } } } ] } })"_sv,
          u8R"({ "jsonrpc": "2.0", "method": "textDocument/didChange", "params": { "textDocument": { "uri": "file:///test.js", "version": 2 }, "contentChanges": [ { "text": "", "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": null } } } ] } })"_sv,
      }) {
    SCOPED_TRACE(out_string8(message));
    this->reset();

    // Open a file so we can test textDocument/didChange (which behaves
    // differently if the file wasn't previously opened).
    this->server->append(
        make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "languageId": "javascript",
            "version": 1,
            "text": ""
          }
        }
      })"_sv));

    this->server->append(make_message(message));
    this->server->flush_error_responses(*this->client);

    // TODO(strager): Have the LSP server respond with a notification instead?
    EXPECT_THAT(this->client->messages, IsEmpty());
  }
}

// TODO(strager): Per the LSP specification, lsp_server should not send messages
// for a Watch_IO_Error before LSP initialization completes.

TEST(Test_LSP_JavaScript_Linter, linting_gives_diagnostics) {
  Padded_String code(u8"let x = x;"_sv);
  Outgoing_JSON_RPC_Message_Queue notifications;
  Configuration config;

  LSP_JavaScript_Linter linter;
  linter.lint(config, Linter_Options(), &code, u8"\"file:///test.js\""_sv,
              u8"10"_sv, notifications);

  Spy_LSP_Endpoint_Remote endpoint;
  notifications.send(endpoint);
  ::boost::json::object notification = endpoint.notifications().at(0);

  EXPECT_EQ(look_up(notification, "method"), "textDocument/publishDiagnostics");
  EXPECT_FALSE(notification.contains("error"));
  // LSP PublishDiagnosticsParams:
  EXPECT_EQ(look_up(notification, "params", "uri"), "file:///test.js");
  EXPECT_EQ(look_up(notification, "params", "version"), 10);
  ::boost::json::value diagnostics =
      look_up(notification, "params", "diagnostics");
  EXPECT_EQ(diagnostics.as_array().size(), 1);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "start", "character"), 8);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "line"), 0);
  EXPECT_EQ(look_up(diagnostics, 0, "range", "end", "character"), 9);
  EXPECT_EQ(look_up(diagnostics, 0, "severity"), lsp_error_severity);
  EXPECT_EQ(look_up(diagnostics, 0, "message"),
            "variable used before declaration: x");
  EXPECT_EQ(look_up(diagnostics, 0, "source"), "quick-lint-js");
  EXPECT_EQ(look_up(diagnostics, 0, "code"), "E0058");
  EXPECT_EQ(look_up(diagnostics, 0, "codeDescription", "href"),
            "https://quick-lint-js.com/errors/E0058/");
}

TEST(Test_LSP_JavaScript_Linter, linting_does_not_desync) {
  // In the past, quick-lint-js' parser mutated the document. This caused
  // LSP-induced changes to edit the mutated document, desyncing the server from
  // the client. This test triggers what used to cause document mutation
  // (an identifier with an escape sequence), and makes sure desyncing doesn't
  // happen.

  Fake_Configuration_Filesystem fs;
  LSP_JavaScript_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  Spy_LSP_Endpoint_Remote remote;
  LSP_JSON_RPC_Message_Parser server(&handler);
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
      })"_sv));
  server.flush_error_responses(remote);
  handler.flush_pending_notifications(remote);

  {
    ASSERT_EQ(remote.messages.size(), 1);
    ::boost::json::object response = remote.messages[0].as_object();
    EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
    // LSP PublishDiagnosticsParams:
    ::boost::json::array diagnostics =
        look_up(response, "params", "diagnostics").as_array();
    EXPECT_EQ(diagnostics.size(), 1) << "'x' should be undeclared";
  }

  handler.flush_pending_notifications(remote);
  remote.messages.clear();

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
      })"_sv));
  server.flush_error_responses(remote);
  handler.flush_pending_notifications(remote);

  {
    ASSERT_EQ(remote.messages.size(), 1);
    ::boost::json::object response = remote.messages[0].as_object();
    EXPECT_EQ(response["method"], "textDocument/publishDiagnostics");
    // LSP PublishDiagnosticsParams:
    ::boost::json::array diagnostics =
        look_up(response, "params", "diagnostics").as_array();
    EXPECT_EQ(diagnostics.size(), 0) << "'x' should be declared";
  }
}

TEST(Test_LSP_JavaScript_Linter,
     linted_javascript_file_complains_about_typescript) {
  Fake_Configuration_Filesystem fs;
  LSP_JavaScript_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  Spy_LSP_Endpoint_Remote client;
  LSP_JSON_RPC_Message_Parser server(&handler);
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.js",
            "languageId": "javascript",
            "version": 10,
            "text": "interface I { }"
          }
        }
      })"_sv));
  server.flush_error_responses(client);
  handler.flush_pending_notifications(client);

  std::vector<::boost::json::object> notifications = client.notifications();
  ASSERT_EQ(notifications.size(), 1);
  ::boost::json::object notification = notifications[0];
  EXPECT_EQ(look_up(notification, "method"), "textDocument/publishDiagnostics");
  EXPECT_FALSE(notification.contains("error"));
  EXPECT_EQ(look_up(notification, "params", "diagnostics", 0, "code"), "E0213")
      << "should report diagnostic: TypeScript's 'interface' feature is not "
         "allowed in JavaScript code";
}

TEST(Test_LSP_JavaScript_Linter,
     linted_typescript_file_accepts_typescript_syntax) {
  Fake_Configuration_Filesystem fs;
  LSP_JavaScript_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  Spy_LSP_Endpoint_Remote client;
  LSP_JSON_RPC_Message_Parser server(&handler);
  server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///test.ts",
            "languageId": "typescript",
            "version": 10,
            "text": "interface I { }"
          }
        }
      })"_sv));
  server.flush_error_responses(client);
  handler.flush_pending_notifications(client);

  std::vector<::boost::json::object> notifications = client.notifications();
  ASSERT_EQ(notifications.size(), 1);
  ::boost::json::object notification = notifications[0];
  EXPECT_EQ(look_up(notification, "method"), "textDocument/publishDiagnostics");
  EXPECT_FALSE(notification.contains("error"));
  EXPECT_THAT(look_up(notification, "params", "diagnostics").as_array(),
              IsEmpty())
      << "should report no diagnostics";
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
