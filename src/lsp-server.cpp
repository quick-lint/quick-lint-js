// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-error-reporter.h>
#include <quick-lint-js/lsp-location.h>
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
void append_raw_json(::simdjson::ondemand::value& value, byte_buffer& out);
void append_raw_json(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& value,
    byte_buffer& out);

string8_view make_string_view(::simdjson::ondemand::value& string);
string8_view make_string_view(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& string);

int get_int(::simdjson::simdjson_result<::simdjson::ondemand::value>&&);
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::handle_request(
    ::simdjson::ondemand::object& request, byte_buffer& response_json) {
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
    ::simdjson::ondemand::object& request, byte_buffer& notification_json) {
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
    ::simdjson::ondemand::object& request, byte_buffer& response_json) {
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
    ::simdjson::ondemand::object& request, byte_buffer& response_json) {
  this->shutdown_requested_ = true;
  response_json.append_copy(u8R"--({"jsonrpc":"2.0","id":)--");
  append_raw_json(request["id"], response_json);
  response_json.append_copy(u8R"--(,"result":null})--");
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_change_notification(
        ::simdjson::ondemand::object& request, byte_buffer& notification_json) {
  ::simdjson::ondemand::object text_document;
  if (request["params"]["textDocument"].get(text_document) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  ::simdjson::ondemand::value uri;
  if (text_document["uri"].get(uri) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  ::simdjson::ondemand::value version;
  if (text_document["version"].get(version) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }

  auto document_it = this->documents_.find(string8(make_string_view(uri)));
  bool url_is_lintable = document_it != this->documents_.end();
  if (!url_is_lintable) {
    return;
  }
  document<lsp_locator>& doc = document_it->second;

  ::simdjson::ondemand::array changes;
  if (request["params"]["contentChanges"].get(changes) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  this->apply_document_changes(doc, changes);
  this->linter_.lint_and_get_diagnostics_notification(
      doc.string(), uri, version, notification_json);
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_close_notification(
        ::simdjson::ondemand::object& request) {
  this->documents_.erase(
      string8(make_string_view(request["params"]["textDocument"]["uri"])));
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_open_notification(
        ::simdjson::ondemand::object& request, byte_buffer& notification_json) {
  ::simdjson::ondemand::object text_document;
  if (request["params"]["textDocument"].get(text_document) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  std::string_view language_id;
  if (text_document["languageId"].get(language_id) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  if (language_id != "javascript") {
    return;
  }
  ::simdjson::ondemand::value uri;
  if (text_document["uri"].get(uri) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  ::simdjson::ondemand::value version;
  if (text_document["version"].get(version) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }

  document<lsp_locator>& doc = this->documents_[string8(make_string_view(uri))];

  doc.set_text(make_string_view(text_document["text"]));
  this->linter_.lint_and_get_diagnostics_notification(
      doc.string(), uri, version, notification_json);
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::apply_document_changes(
    document<lsp_locator>& doc, ::simdjson::ondemand::array& changes) {
  for (::simdjson::simdjson_result<::simdjson::ondemand::value> change :
       changes) {
    string8_view change_text = make_string_view(change["text"]);
    ::simdjson::ondemand::object raw_range;
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
      doc.replace_text(range, change_text);
    } else {
      doc.set_text(change_text);
    }
  }
}

void lsp_javascript_linter::lint_and_get_diagnostics_notification(
    padded_string_view code, ::simdjson::ondemand::value& uri,
    ::simdjson::ondemand::value& version, byte_buffer& notification_json) {
  // clang-format off
  notification_json.append_copy(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--");
  // clang-format on
  append_raw_json(uri, notification_json);

  notification_json.append_copy(u8R"--(,"version":)--");
  append_raw_json(version, notification_json);

  notification_json.append_copy(u8R"--(,"diagnostics":)--");
  this->lint_and_get_diagnostics(code, notification_json);

  notification_json.append_copy(u8R"--(},"jsonrpc":"2.0"})--");
}

void lsp_javascript_linter::lint_and_get_diagnostics(
    padded_string_view code, byte_buffer& diagnostics_json) {
  lsp_error_reporter error_reporter(diagnostics_json, code);

  configuration config;
  parser p(code, &error_reporter);
  linter l(&error_reporter, &config.globals());
#if QLJS_HAVE_SETJMP
  bool ok = p.parse_and_visit_module_catching_unimplemented(l);
  if (!ok) {
    // TODO(strager): Send a window/logMessage to the client reporting that the
    // parser crashed.
  }
#else
  p.parse_and_visit_module(l);
#endif

  error_reporter.finish();
}

mock_lsp_linter::mock_lsp_linter(
    std::function<lint_and_get_diagnostics_notification_type> callback)
    : callback_(std::move(callback)) {}

void mock_lsp_linter::lint_and_get_diagnostics_notification(
    padded_string_view code, ::simdjson::ondemand::value& uri,
    ::simdjson::ondemand::value& version, byte_buffer& notification_json) {
  this->callback_(code, uri, version, notification_json);
}

template class linting_lsp_server_handler<lsp_javascript_linter>;
template class linting_lsp_server_handler<mock_lsp_linter>;

namespace {
void append_raw_json(::simdjson::ondemand::value& value, byte_buffer& out) {
  ::simdjson::ondemand::json_type type;
  if (value.type().get(type) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  switch (type) {
  case ::simdjson::ondemand::json_type::boolean:
  case ::simdjson::ondemand::json_type::null:
  case ::simdjson::ondemand::json_type::number:
  case ::simdjson::ondemand::json_type::string:
    out.append_copy(to_string8_view(value.raw_json_token()));
    break;

  case ::simdjson::ondemand::json_type::array:
  case ::simdjson::ondemand::json_type::object:
    QLJS_UNIMPLEMENTED();
    break;
  }
}

void append_raw_json(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& value,
    byte_buffer& out) {
  ::simdjson::ondemand::value real_value;
  if (value.get(real_value) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  append_raw_json(real_value, out);
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
string8_view make_string_view(::simdjson::ondemand::value& string) {
  std::string_view s;
  if (string.get(s) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  return string8_view(reinterpret_cast<const char8*>(s.data()), s.size());
}
QLJS_WARNING_POP

string8_view make_string_view(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& string) {
  ::simdjson::ondemand::value s;
  if (string.get(s) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  return make_string_view(s);
}

int get_int(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& element) {
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
