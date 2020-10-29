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

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <json/value.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-error-reporter.h>
#include <quick-lint-js/lsp-server.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/version.h>
#include <quick-lint-js/warning.h>
#include <sstream>
#include <string>

namespace quick_lint_js {
namespace {
string8_view make_string_view(::Json::Value& string);
}

void linting_lsp_server_handler::handle_request(const char8* message_begin,
                                                ::Json::Value& request,
                                                byte_buffer& response_json) {
  ::Json::Value& method = request["method"];
  if (method == "initialize") {
    this->handle_initialize_request(message_begin, request, response_json);
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

void linting_lsp_server_handler::handle_notification(
    const char8* message_begin, ::Json::Value& request,
    byte_buffer& notification_json) {
  ::Json::Value& method = request["method"];
  if (method == "textDocument/didChange") {
    this->handle_text_document_did_change_notification(message_begin, request,
                                                       notification_json);
  } else if (method == "textDocument/didOpen") {
    this->handle_text_document_did_open_notification(message_begin, request,
                                                     notification_json);
  } else if (method == "textDocument/didClose") {
    this->handle_text_document_did_close_notification(request);
  } else if (method == "initialized") {
    // Do nothing.
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

void linting_lsp_server_handler::handle_initialize_request(
    const char8* message_begin, ::Json::Value& request,
    byte_buffer& response_json) {
  response_json.append_copy(u8R"--({"id":)--");
  response_json.append_copy(this->raw_json(request["id"], message_begin));
  // clang-format off
  response_json.append_copy(
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

void linting_lsp_server_handler::handle_text_document_did_change_notification(
    const char8* message_begin, ::Json::Value& request,
    byte_buffer& notification_json) {
  ::Json::Value& text_document = request["params"]["textDocument"];
  bool url_is_lintable =
      std::find(this->lintable_uris_.begin(), this->lintable_uris_.end(),
                make_string_view(text_document["uri"])) !=
      this->lintable_uris_.end();
  if (!url_is_lintable) {
    return;
  }

  // TODO(strager): What if contentChanges is empty or contains more than one
  // entry?
  padded_string code =
      make_padded_string(request["params"]["contentChanges"][0]["text"]);
  this->lint_and_get_diagnostics_notification(&code, text_document,
                                              message_begin, notification_json);
}

void linting_lsp_server_handler::handle_text_document_did_close_notification(
    ::Json::Value& request) {
  ::Json::Value& uri = request["params"]["textDocument"]["uri"];
  auto lintable_uri_it =
      std::find(this->lintable_uris_.begin(), this->lintable_uris_.end(),
                make_string_view(uri));
  if (lintable_uri_it != this->lintable_uris_.end()) {
    this->lintable_uris_.erase(lintable_uri_it);
  }
}

void linting_lsp_server_handler::handle_text_document_did_open_notification(
    const char8* message_begin, ::Json::Value& request,
    byte_buffer& notification_json) {
  if (request["params"]["textDocument"]["languageId"] != "javascript") {
    return;
  }

  ::Json::Value& text_document = request["params"]["textDocument"];
  this->lintable_uris_.emplace_back(make_string_view(text_document["uri"]));

  padded_string code = make_padded_string(text_document["text"]);
  this->lint_and_get_diagnostics_notification(&code, text_document,
                                              message_begin, notification_json);
}

void linting_lsp_server_handler::lint_and_get_diagnostics_notification(
    padded_string_view code, ::Json::Value& text_document,
    const char8* message_begin, byte_buffer& notification_json) {
  // clang-format off
  notification_json.append_copy(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--");
  // clang-format on
  notification_json.append_copy(
      this->raw_json(text_document["uri"], message_begin));

  notification_json.append_copy(u8R"--(,"version":)--");
  notification_json.append_copy(
      this->raw_json(text_document["version"], message_begin));

  notification_json.append_copy(u8R"--(,"diagnostics":)--");
  this->lint_and_get_diagnostics(code, notification_json);

  notification_json.append_copy(u8R"--(},"jsonrpc":"2.0"})--");
}

void linting_lsp_server_handler::lint_and_get_diagnostics(
    padded_string_view code, byte_buffer& diagnostics_json) {
  lsp_error_reporter error_reporter(diagnostics_json, code);

  parser p(code, &error_reporter);
  linter l(&error_reporter);
  p.parse_and_visit_module(l);

  error_reporter.finish();
}

string8_view linting_lsp_server_handler::raw_json(::Json::Value& value,
                                                  const char8* json) {
  return string8_view(json + value.getOffsetStart(),
                      narrow_cast<std::size_t>(value.getOffsetLimit() -
                                               value.getOffsetStart()));
}

padded_string linting_lsp_server_handler::make_padded_string(
    ::Json::Value& string) {
  string8_view string_view = make_string_view(string);
  padded_string result;
  result.resize(narrow_cast<int>(string_view.size()));
  std::memcpy(result.data(), string_view.data(), string_view.size());
  return result;
}

namespace {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
string8_view make_string_view(::Json::Value& string) {
  const char* begin;
  const char* end;
  if (!string.getString(&begin, &end)) {
    QLJS_UNIMPLEMENTED();
  }
  return string8_view(reinterpret_cast<const char8*>(begin),
                      narrow_cast<std::size_t>(end - begin));
}
QLJS_WARNING_POP
}
}
