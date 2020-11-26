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

#include <quick-lint-js/warning.h>

// HACK(strager): GCC 9.3.0 reports possibly-uninitialized reads in
// append_raw_json. For some reason, we can't suppress it after one of our
// #include-s; the suppression gets ignored. Therefore, we suppress the warning
// as soon as possible.
QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <functional>
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
#include <simdjson.h>
#include <sstream>
#include <string>
#include <utility>

namespace quick_lint_js {
namespace {
padded_string make_padded_string(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& string);

void append_raw_json(::simdjson::dom::element& value, byte_buffer& out);
void append_raw_json(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& value,
    byte_buffer& out);

string8_view make_string_view(const ::simdjson::dom::element& string);
string8_view make_string_view(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& string);
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::handle_request(
    ::simdjson::dom::element& request, byte_buffer& response_json) {
  std::string_view method;
  if (request["method"].get(method) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  if (method == "initialize") {
    this->handle_initialize_request(request, response_json);
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::handle_notification(
    ::simdjson::dom::element& request, byte_buffer& notification_json) {
  std::string_view method;
  if (request["method"].get(method) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  if (method == "textDocument/didChange") {
    this->handle_text_document_did_change_notification(request,
                                                       notification_json);
  } else if (method == "textDocument/didOpen") {
    this->handle_text_document_did_open_notification(request,
                                                     notification_json);
  } else if (method == "textDocument/didClose") {
    this->handle_text_document_did_close_notification(request);
  } else if (method == "initialized") {
    // Do nothing.
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::handle_initialize_request(
    ::simdjson::dom::element& request, byte_buffer& response_json) {
  response_json.append_copy(u8R"--({"id":)--");
  append_raw_json(request["id"], response_json);
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

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_change_notification(
        ::simdjson::dom::element& request, byte_buffer& notification_json) {
  ::simdjson::dom::element text_document;
  if (request["params"]["textDocument"].get(text_document) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
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
      make_padded_string(request["params"]["contentChanges"].at(0)["text"]);
  this->linter_.lint_and_get_diagnostics_notification(&code, text_document,
                                                      notification_json);
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_close_notification(
        ::simdjson::dom::element& request) {
  auto lintable_uri_it =
      std::find(this->lintable_uris_.begin(), this->lintable_uris_.end(),
                make_string_view(request["params"]["textDocument"]["uri"]));
  if (lintable_uri_it != this->lintable_uris_.end()) {
    this->lintable_uris_.erase(lintable_uri_it);
  }
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_open_notification(
        ::simdjson::dom::element& request, byte_buffer& notification_json) {
  std::string_view language_id;
  if (request["params"]["textDocument"]["languageId"].get(language_id) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  if (language_id != "javascript") {
    return;
  }

  ::simdjson::dom::element text_document;
  if (request["params"]["textDocument"].get(text_document) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  this->lintable_uris_.emplace_back(make_string_view(text_document["uri"]));

  padded_string code = make_padded_string(text_document["text"]);
  this->linter_.lint_and_get_diagnostics_notification(&code, text_document,
                                                      notification_json);
}

void lsp_javascript_linter::lint_and_get_diagnostics_notification(
    padded_string_view code, ::simdjson::dom::element& text_document,
    byte_buffer& notification_json) {
  // clang-format off
  notification_json.append_copy(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--");
  // clang-format on
  append_raw_json(text_document["uri"], notification_json);

  notification_json.append_copy(u8R"--(,"version":)--");
  append_raw_json(text_document["version"], notification_json);

  notification_json.append_copy(u8R"--(,"diagnostics":)--");
  this->lint_and_get_diagnostics(code, notification_json);

  notification_json.append_copy(u8R"--(},"jsonrpc":"2.0"})--");
}

void lsp_javascript_linter::lint_and_get_diagnostics(
    padded_string_view code, byte_buffer& diagnostics_json) {
  lsp_error_reporter error_reporter(diagnostics_json, code);

  parser p(code, &error_reporter);
  linter l(&error_reporter);
  p.parse_and_visit_module(l);

  error_reporter.finish();
}

mock_lsp_linter::mock_lsp_linter(
    std::function<lint_and_get_diagnostics_notification_type> callback)
    : callback_(std::move(callback)) {}

void mock_lsp_linter::lint_and_get_diagnostics_notification(
    padded_string_view code, ::simdjson::dom::element& text_document,
    byte_buffer& notification_json) {
  this->callback_(code, text_document, notification_json);
}

template class linting_lsp_server_handler<lsp_javascript_linter>;
template class linting_lsp_server_handler<mock_lsp_linter>;

namespace {
padded_string make_padded_string(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& string) {
  string8_view s = make_string_view(string);
  padded_string result;
  result.resize(narrow_cast<int>(s.size()));
  std::memcpy(result.data(), s.data(), s.size());
  return result;
}

void append_raw_json(::simdjson::dom::element& value, byte_buffer& out) {
  switch (value.type()) {
  case ::simdjson::dom::element_type::INT64: {
    std::int64_t data;
    ::simdjson::error_code error = value.get(data);
    QLJS_ASSERT(error == ::simdjson::error_code::SUCCESS);
    out.append_decimal_integer(data);
    break;
  }

  case ::simdjson::dom::element_type::UINT64: {
    std::uint64_t data;
    ::simdjson::error_code error = value.get(data);
    QLJS_ASSERT(error == ::simdjson::error_code::SUCCESS);
    out.append_decimal_integer(data);
    break;
  }

  case ::simdjson::dom::element_type::STRING:
    out.append_copy(u8"\"");
    write_json_escaped_string(out, make_string_view(value));
    out.append_copy(u8"\"");
    break;

  case ::simdjson::dom::element_type::NULL_VALUE:
    out.append_copy(u8"null");
    break;

  case ::simdjson::dom::element_type::DOUBLE:
    QLJS_UNIMPLEMENTED();
    break;

  case ::simdjson::dom::element_type::ARRAY:
  case ::simdjson::dom::element_type::BOOL:
  case ::simdjson::dom::element_type::OBJECT:
    QLJS_UNIMPLEMENTED();
    break;
  }
}

void append_raw_json(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& value,
    byte_buffer& out) {
  ::simdjson::dom::element real_value;
  if (value.get(real_value) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  append_raw_json(real_value, out);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
string8_view make_string_view(const ::simdjson::dom::element& string) {
  std::string_view s;
  if (string.get(s) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  return string8_view(reinterpret_cast<const char8*>(s.data()), s.size());
}
QLJS_WARNING_POP

string8_view make_string_view(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& string) {
  ::simdjson::dom::element s;
  if (string.get(s) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  return make_string_view(s);
}
}
}
