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

#include <cstddef>
#include <cstring>
#include <json/value.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-error-reporter.h>
#include <quick-lint-js/lsp-server.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/version.h>
#include <sstream>
#include <string>

namespace quick_lint_js {
void linting_lsp_server_handler::handle_request(const char8* message_begin,
                                                ::Json::Value& request,
                                                string8& response_json) {
  ::Json::Value& method = request["method"];
  if (method == "initialize") {
    this->handle_initialize_request(message_begin, request, response_json);
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

void linting_lsp_server_handler::handle_notification(
    const char8* message_begin, ::Json::Value& request,
    string8& notification_json) {
  ::Json::Value& method = request["method"];
  if (method == "textDocument/didOpen") {
    this->handle_text_document_did_open_notification(message_begin, request,
                                                     notification_json);
  } else if (method == "initialized") {
    // Do nothing.
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

void linting_lsp_server_handler::handle_initialize_request(
    const char8* message_begin, ::Json::Value& request,
    string8& response_json) {
  response_json.append(u8R"--({"id":)--");
  response_json.append(this->raw_json(request["id"], message_begin));
  // clang-format off
  response_json.append(
    u8R"--(,)--"
    u8R"--("result":{)--"
      u8R"--("capabilities":{)--"
        u8R"--("textDocumentSync":{"change":1,"openClose":true})--"
      u8R"--(},)--"
      u8R"--("serverInfo":{)--"
        u8R"--("name":"quick-lint-js",)--"
        u8R"--("version":")--" QUICK_LINT_JS_VERSION_STRING_U8
      u8R"--("})--"
    u8R"--(},)--"
    u8R"--("jsonrpc":"2.0"})--");
  // clang-format on
}

void linting_lsp_server_handler::handle_text_document_did_open_notification(
    const char8* message_begin, ::Json::Value& request,
    string8& notification_json) {
  if (request["params"]["textDocument"]["languageId"] != "javascript") {
    return;
  }

  // clang-format off
  notification_json.append(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--");
  // clang-format on
  notification_json.append(
      this->raw_json(request["params"]["textDocument"]["uri"], message_begin));

  notification_json.append(u8R"--(,"version":)--");
  notification_json.append(this->raw_json(
      request["params"]["textDocument"]["version"], message_begin));

  notification_json.append(u8R"--(,"diagnostics":)--");
  padded_string code =
      this->make_padded_string(request["params"]["textDocument"]["text"]);
  this->lint_and_get_diagnostics(&code, notification_json);

  notification_json.append(u8R"--(},"jsonrpc":"2.0"})--");
}

void linting_lsp_server_handler::lint_and_get_diagnostics(
    padded_string_view code, string8& diagnostics_json) {
  std::ostringstream diagnostics_stream;
  lsp_error_reporter error_reporter(diagnostics_stream, code);

  parser p(code, &error_reporter);
  linter l(&error_reporter);
  p.parse_and_visit_module(l);

  error_reporter.finish();
  std::string diagnostics = diagnostics_stream.str();
  diagnostics_json.append(reinterpret_cast<const char8*>(diagnostics.data()),
                          diagnostics.size());
}

padded_string linting_lsp_server_handler::make_padded_string(
    ::Json::Value& string) {
  const char* begin;
  const char* end;
  if (!string.getString(&begin, &end)) {
    QLJS_UNIMPLEMENTED();
  }
  int size = narrow_cast<int>(end - begin);

  padded_string result;
  result.resize(size);
  std::memcpy(result.data(), begin, narrow_cast<std::size_t>(size));
  return result;
}

string8_view linting_lsp_server_handler::raw_json(::Json::Value& value,
                                                  const char8* json) {
  return string8_view(json + value.getOffsetStart(),
                      narrow_cast<std::size_t>(value.getOffsetLimit() -
                                               value.getOffsetStart()));
}
}
