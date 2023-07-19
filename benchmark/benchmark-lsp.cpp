// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <array>
#include <benchmark/benchmark.h>
#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/basic-configuration-filesystem.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/port/char8.h>
#include <string>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
class Byte_Buffer;

namespace {
class Null_LSP_Writer final : public LSP_Endpoint_Remote {
 public:
  void send_message(Byte_Buffer&& message) override {
    ::benchmark::ClobberMemory();
    ::benchmark::DoNotOptimize(message);
  }
};

String8 make_message(String8_View content) {
  return String8(u8"Content-Length: ") +
         to_string8(std::to_string(content.size())) + u8"\r\n\r\n" +
         String8(content);
}

void benchmark_lsp_full_text_change_on_tiny_document(
    ::benchmark::State& state) {
  // TODO(strager): This performs undesired filesystem accesses! Make a
  // null_configuration_filesystem.
  Basic_Configuration_Filesystem fs;
  LSP_JavaScript_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  Null_LSP_Writer remote;
  LSP_JSON_RPC_Message_Parser lsp_server(&handler);
  lsp_server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///benchmark.js",
            "languageId": "javascript",
            "version": 1000000000,
            "text": "let x = x;"
          }
        }
      })"));

  std::array<String8, 2> change_messages = {
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///benchmark.js",
            "version": 1000000000
          },
          "contentChanges": [
            {
              "text": "let y = y;"
            }
          ]
        }
      })"),
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///benchmark.js",
            "version": 1000000001
          },
          "contentChanges": [
            {
              "text": "let z = z;"
            }
          ]
        }
      })"),
  };

  for (auto _ : state) {
    lsp_server.append(change_messages[0]);
    lsp_server.append(change_messages[1]);
  }
  double iteration_count = static_cast<double>(state.iterations());
  double change_count = iteration_count / 2.0;
  state.counters["changes"] =
      ::benchmark::Counter(change_count, ::benchmark::Counter::kIsRate);
}
BENCHMARK(benchmark_lsp_full_text_change_on_tiny_document);

void benchmark_lsp_full_text_change_on_large_document(
    ::benchmark::State& state) {
  String8_View code_line = u8"console.log('HELLO');\n";
  int line_count = 1000;

  String8 document_text;
  for (int line = 0; line < line_count; ++line) {
    document_text += code_line;
  }
  String8 document_text_json =
      to_json_escaped_string_with_quotes(document_text);

  // TODO(strager): This performs undesired filesystem accesses! Make a
  // null_configuration_filesystem.
  Basic_Configuration_Filesystem fs;
  LSP_JavaScript_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  Null_LSP_Writer remote;
  LSP_JSON_RPC_Message_Parser lsp_server(&handler);

  lsp_server.append(
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
          "textDocument": {
            "uri": "file:///benchmark.js",
            "languageId": "javascript",
            "version": 1000000000,
            "text": )" +
                   document_text_json + u8R"(
          }
        }
      })"));

  std::array<String8, 2> change_messages = {
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///benchmark.js",
            "version": 1000000000
          },
          "contentChanges": [
            {
              "text": )" +
                   document_text_json + u8R"(
            }
          ]
        }
      })"),
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///benchmark.js",
            "version": 1000000001
          },
          "contentChanges": [
            {
              "text": )" +
                   document_text_json + u8R"(
            }
          ]
        }
      })"),
  };

  for (auto _ : state) {
    lsp_server.append(change_messages[0]);
    lsp_server.append(change_messages[1]);
  }
  double iteration_count = static_cast<double>(state.iterations());
  double change_count = iteration_count / 2.0;
  state.counters["changes"] =
      ::benchmark::Counter(change_count, ::benchmark::Counter::kIsRate);
}
BENCHMARK(benchmark_lsp_full_text_change_on_large_document);

void benchmark_lsp_tiny_change_on_large_document(::benchmark::State& state) {
  String8_View code_line = u8"console.log('HELLO');\n";
  int line_count = 1000;

  Memory_Output_Stream did_open_message_json;
  did_open_message_json.append_copy(
      u8R"(
    {
      "jsonrpc": "2.0",
      "method": "textDocument/didOpen",
      "params": {
        "textDocument": {
          "uri": "file:///benchmark.js",
          "languageId": "javascript",
          "version": 1000000000,
          "text": ")"sv);
  for (int line = 0; line < line_count; ++line) {
    write_json_escaped_string(did_open_message_json, code_line);
  }
  did_open_message_json.append_copy(
      u8R"("
        }
      }
    })"sv);
  did_open_message_json.flush();

  std::array<String8, 2> change_messages = {
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///benchmark.js",
            "version": 1000000000
          },
          "contentChanges": [
            {
              "text": "!",
              "range": {
                "start": {"line": 500, "character": 18},
                "end": {"line": 500, "character": 18}
              }
            }
          ]
        }
      })"),
      make_message(u8R"({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
          "textDocument": {
            "uri": "file:///benchmark.js",
            "version": 1000000001
          },
          "contentChanges": [
            {
              "text": "",
              "range": {
                "start": {"line": 500, "character": 18},
                "end": {"line": 500, "character": 19}
              }
            }
          ]
        }
      })"),
  };

  // TODO(strager): This performs undesired filesystem accesses! Make a
  // null_configuration_filesystem.
  Basic_Configuration_Filesystem fs;
  LSP_JavaScript_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  Null_LSP_Writer remote;
  LSP_JSON_RPC_Message_Parser lsp_server(&handler);
  lsp_server.append(make_message(did_open_message_json.get_flushed_string8()));

  for (auto _ : state) {
    lsp_server.append(change_messages[0]);
    lsp_server.append(change_messages[1]);
  }
  double iteration_count = static_cast<double>(state.iterations());
  double change_count = iteration_count / 2.0;
  state.counters["changes"] =
      ::benchmark::Counter(change_count, ::benchmark::Counter::kIsRate);
}
BENCHMARK(benchmark_lsp_tiny_change_on_large_document);
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
