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

#include <array>
#include <benchmark/benchmark.h>
#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-server.h>
#include <sstream>
#include <string>

namespace quick_lint_js {
class byte_buffer;

namespace {
class null_lsp_writer {
 public:
  void send_message(const byte_buffer& message) {
    ::benchmark::ClobberMemory();
    ::benchmark::DoNotOptimize(message);
  }
};

string8 make_message(string8_view content) {
  return string8(u8"Content-Length: ") +
         to_string8(std::to_string(content.size())) + u8"\r\n\r\n" +
         string8(content);
}

void benchmark_lsp_full_text_change_on_tiny_document(
    ::benchmark::State& state) {
  lsp_endpoint<linting_lsp_server_handler<lsp_javascript_linter>,
               null_lsp_writer>
      lsp_server;
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

  std::array<string8, 2> change_messages = {
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

void benchmark_lsp_tiny_change_on_large_document(::benchmark::State& state) {
  string8_view code_line = u8"console.log('HELLO');\n";
  int line_count = 1000;

  std::ostringstream did_open_message_json;
  did_open_message_json << R"(
    {
      "jsonrpc": "2.0",
      "method": "textDocument/didOpen",
      "params": {
        "textDocument": {
          "uri": "file:///benchmark.js",
          "languageId": "javascript",
          "version": 1000000000,
          "text": ")";
  for (int line = 0; line < line_count; ++line) {
    write_json_escaped_string(did_open_message_json, code_line);
  }
  did_open_message_json << R"("
        }
      }
    })";

  std::array<string8, 2> change_messages = {
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

  lsp_endpoint<linting_lsp_server_handler<lsp_javascript_linter>,
               null_lsp_writer>
      lsp_server;
  lsp_server.append(make_message(to_string8(did_open_message_json.str())));

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
