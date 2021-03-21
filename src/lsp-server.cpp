// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/warning.h>

// HACK(strager): GCC 9.3.0 reports possibly-uninitialized reads in
// append_raw_json. For some reason, we can't suppress it after one of our
// #include-s; the suppression gets ignored. Therefore, we suppress the warning
// as soon as possible.
QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")

#include <algorithm>
#include <cstddef>
#include <cstdlib>
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
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/version.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>
#include <sstream>
#include <string>
#include <utility>

namespace quick_lint_js {
namespace {
void append_raw_json(::simdjson::dom::element& value, byte_buffer& out);
void append_raw_json(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& value,
    byte_buffer& out);

string8_view make_string_view(const ::simdjson::dom::element& string);
string8_view make_string_view(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& string);

int get_int(const ::simdjson::simdjson_result<::simdjson::dom::element>&);
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
  } else if (method == "shutdown") {
    this->handle_shutdown_request(request, response_json);
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
  } else if (method == "exit") {
    std::exit(this->shutdown_requested_ ? 0 : 1);
  } else if (starts_with(method, "$/")) {
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
        u8R"--("textDocumentSync":{"change":2,"openClose":true})--"
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
void linting_lsp_server_handler<Linter>::handle_shutdown_request(
    ::simdjson::dom::element& request, byte_buffer& response_json) {
  this->shutdown_requested_ = true;
  response_json.append_copy(u8R"--({"jsonrpc":"2.0","id":)--");
  append_raw_json(request["id"], response_json);
  response_json.append_copy(u8R"--(,"result":null})--");
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
  auto document_it =
      this->documents_.find(string8(make_string_view(text_document["uri"])));
  bool url_is_lintable = document_it != this->documents_.end();
  if (!url_is_lintable) {
    return;
  }
  lsp_document& document = document_it->second;

  ::simdjson::dom::array changes;
  if (request["params"]["contentChanges"].get(changes) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  this->apply_document_changes(document, changes);
  this->linter_.lint_and_get_diagnostics_notification(
      document.string(), text_document, notification_json);
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_close_notification(
        ::simdjson::dom::element& request) {
  this->documents_.erase(
      string8(make_string_view(request["params"]["textDocument"]["uri"])));
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
  lsp_document& document =
      this->documents_[string8(make_string_view(text_document["uri"]))];

  document.set_text(make_string_view(text_document["text"]));
  this->linter_.lint_and_get_diagnostics_notification(
      document.string(), text_document, notification_json);
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::apply_document_changes(
    lsp_document& document, ::simdjson::dom::array& changes) {
  for (::simdjson::dom::element change : changes) {
    string8_view change_text = make_string_view(change["text"]);
    ::simdjson::dom::object raw_range;
    bool is_incremental =
        change["range"].get(raw_range) == ::simdjson::error_code::SUCCESS;
    if (is_incremental) {
      lsp_range range = {
          .start =
              {
                  .line = get_int(raw_range["start"]["line"]),
                  .character = get_int(raw_range["start"]["character"]),
              },
          .end =
              {
                  .line = get_int(raw_range["end"]["line"]),
                  .character = get_int(raw_range["end"]["character"]),
              },
      };
      document.replace_text(range, change_text);
    } else {
      document.set_text(change_text);
    }
  }
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

int get_int(
    const ::simdjson::simdjson_result<::simdjson::dom::element>& element) {
  std::int64_t int64;
  if (element.get(int64) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  if (!in_range<int>(int64)) {
    QLJS_UNIMPLEMENTED();
  }
  return static_cast<int>(int64);
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
