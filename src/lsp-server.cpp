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
#include <quick-lint-js/have.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-error-reporter.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/lsp-server.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/uri.h>
#include <quick-lint-js/version.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>
#include <sstream>
#include <string>
#include <utility>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
void append_raw_json(::simdjson::ondemand::value& value, byte_buffer& out);
void append_raw_json(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& value,
    byte_buffer& out);
string8_view get_raw_json(::simdjson::ondemand::value& value);

string8_view make_string_view(::simdjson::ondemand::value& string);
string8_view make_string_view(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& string);

int get_int(::simdjson::simdjson_result<::simdjson::ondemand::value>&&);
}

lsp_overlay_configuration_filesystem::lsp_overlay_configuration_filesystem(
    configuration_filesystem* underlying_fs)
    : underlying_fs_(underlying_fs) {}

result<canonical_path_result, canonicalize_path_io_error>
lsp_overlay_configuration_filesystem::canonicalize_path(
    const std::string& path) {
  return this->underlying_fs_->canonicalize_path(path);
}

result<padded_string, read_file_io_error, watch_io_error>
lsp_overlay_configuration_filesystem::read_file(const canonical_path& path) {
#if QLJS_HAVE_STD_TRANSPARENT_KEYS
  std::string_view key = path.path();
#else
  std::string key(path.path());
#endif
  auto doc_it = this->overlaid_documents_.find(key);
  if (doc_it == this->overlaid_documents_.end()) {
    return this->underlying_fs_->read_file(path);
  }
  return padded_string(doc_it->second->string().string_view());
}

void lsp_overlay_configuration_filesystem::open_document(
    const std::string& path, document<lsp_locator>* doc) {
  auto [_it, inserted] = this->overlaid_documents_.emplace(path, doc);
  QLJS_ASSERT(inserted);
}

void lsp_overlay_configuration_filesystem::close_document(
    const std::string& path) {
  bool erased = this->overlaid_documents_.erase(path);
  QLJS_ASSERT(erased);
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
    ::simdjson::ondemand::object& request,
    std::vector<byte_buffer>& notification_jsons) {
  std::string_view method;
  if (request["method"].get(method) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  if (method == "textDocument/didChange") {
    this->handle_text_document_did_change_notification(request,
                                                       notification_jsons);
  } else if (method == "textDocument/didOpen") {
    this->handle_text_document_did_open_notification(request,
                                                     notification_jsons);
  } else if (method == "textDocument/didClose") {
    this->handle_text_document_did_close_notification(request,
                                                      notification_jsons);
  } else if (method == "initialized") {
    // Do nothing.
  } else if (method == "exit") {
    std::exit(this->shutdown_requested_ ? 0 : 1);
  } else if (starts_with(method, "$/"sv)) {
    // Do nothing.
  } else if (method == "workspace/didChangeConfiguration") {
    // Do nothing.
  } else if (method == "textDocument/didSave") {
    // Do nothing.
  } else if (method == "textDocument/willSave") {
    // Do nothing.
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::filesystem_changed(
    std::vector<byte_buffer>& notification_jsons) {
  std::vector<configuration_change> config_changes =
      this->config_loader_.refresh();
  this->handle_config_file_changes(config_changes, notification_jsons);
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
        ::simdjson::ondemand::object& request,
        std::vector<byte_buffer>& notification_jsons) {
  ::simdjson::ondemand::object text_document;
  if (request["params"]["textDocument"].get(text_document) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  ::simdjson::ondemand::value uri;
  if (text_document["uri"].get(uri) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  string8_view uri_string = make_string_view(uri);
  ::simdjson::ondemand::value version;
  if (text_document["version"].get(version) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }

  auto document_it = this->documents_.find(string8(make_string_view(uri)));
  bool url_is_tracked = document_it != this->documents_.end();
  if (!url_is_tracked) {
    return;
  }
  document& doc = document_it->second;

  std::string document_path = parse_file_from_lsp_uri(uri_string);
  if (document_path.empty()) {
    // TODO(strager): Report a warning and use a default configuration.
    QLJS_UNIMPLEMENTED();
  }

  ::simdjson::ondemand::array changes;
  if (request["params"]["contentChanges"].get(changes) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  this->apply_document_changes(doc.doc, changes);
  doc.version_json = get_raw_json(version);

  switch (doc.type) {
  case document_type::lintable: {
    byte_buffer& notification_json = notification_jsons.emplace_back();
    this->linter_.lint_and_get_diagnostics_notification(
        *doc.config, doc.doc.string(), get_raw_json(uri), doc.version_json,
        notification_json);
    break;
  }

  case document_type::config: {
    std::vector<configuration_change> config_changes =
        this->config_loader_.refresh();
    this->handle_config_file_changes(config_changes, notification_jsons);
    break;
  }

  case document_type::unknown:
    // Ignore.
    break;
  }
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_close_notification(
        ::simdjson::ondemand::object& request,
        std::vector<byte_buffer>& notification_jsons) {
  string8_view uri = make_string_view(request["params"]["textDocument"]["uri"]);
  std::string path = parse_file_from_lsp_uri(uri);
  if (path.empty()) {
    // TODO(strager): Report a warning.
    QLJS_UNIMPLEMENTED();
  }

  this->config_fs_.close_document(path);
  this->documents_.erase(string8(uri));
  // TODO(strager): Signal to configuration_loader and
  // change_detecting_filesystem_* that we no longer need to track changes to
  // this .js document's config file.

  this->filesystem_changed(notification_jsons);
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    handle_text_document_did_open_notification(
        ::simdjson::ondemand::object& request,
        std::vector<byte_buffer>& notification_jsons) {
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
  ::simdjson::ondemand::value uri;
  if (text_document["uri"].get(uri) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  string8_view uri_string = make_string_view(uri);
  ::simdjson::ondemand::value version;
  if (text_document["version"].get(version) !=
      ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }

  document& doc = this->documents_[string8(uri_string)];

  std::string document_path = parse_file_from_lsp_uri(uri_string);
  if (document_path.empty()) {
    // TODO(strager): Report a warning and use a default configuration.
    QLJS_UNIMPLEMENTED();
  }
  this->config_fs_.open_document(document_path, &doc.doc);

  doc.doc.set_text(make_string_view(text_document["text"]));
  doc.version_json = get_raw_json(version);

  if (language_id == "javascript" || language_id == "js") {
    doc.type = document_type::lintable;
    auto config = this->config_loader_.watch_and_load_for_file(document_path,
                                                               /*token=*/&doc);
    if (config.ok()) {
      doc.config = *config;
    } else {
      doc.config = this->config_loader_.get_default_config();
      byte_buffer& message_json = notification_jsons.emplace_back();
      this->write_configuration_loader_error_notification(
          document_path, config.error_to_string(), message_json);
    }
    byte_buffer& notification_json = notification_jsons.emplace_back();
    this->linter_.lint_and_get_diagnostics_notification(
        *doc.config, doc.doc.string(), get_raw_json(uri), doc.version_json,
        notification_json);
  } else {
    doc.type = document_type::config;
    std::vector<configuration_change> config_changes =
        this->config_loader_.refresh();
    this->handle_config_file_changes(config_changes, notification_jsons);
  }
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::handle_config_file_changes(
    const std::vector<configuration_change>& config_changes,
    std::vector<byte_buffer>& notification_jsons) {
  for (auto& entry : this->documents_) {
    const string8& document_uri = entry.first;
    document& doc = entry.second;
    if (doc.type == document_type::lintable) {
      auto change_it =
          std::find_if(config_changes.begin(), config_changes.end(),
                       [&](const configuration_change& change) {
                         return change.token == &doc;
                       });
      if (change_it == config_changes.end()) {
        continue;
      }

      std::string document_path = parse_file_from_lsp_uri(document_uri);
      if (document_path.empty()) {
        // TODO(strager): Report a warning and use a default configuration.
        QLJS_UNIMPLEMENTED();
      }
      if (change_it->error) {
        byte_buffer& message_json = notification_jsons.emplace_back();
        this->write_configuration_loader_error_notification(
            document_path,
            std::visit([](const auto& error) { return error.to_string(); },
                       *change_it->error),
            message_json);
      }
      configuration* config = change_it->config;
      doc.config = config;
      byte_buffer& notification_json = notification_jsons.emplace_back();
      // TODO(strager): Don't copy document_uri if it contains only non-special
      // characters.
      // TODO(strager): Cache the result of to_json_escaped_string?
      this->linter_.lint_and_get_diagnostics_notification(
          *config, doc.doc.string(),
          to_json_escaped_string_with_quotes(document_uri), doc.version_json,
          notification_json);
    }
  }
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::
    write_configuration_loader_error_notification(
        std::string_view document_path, std::string_view error_details,
        byte_buffer& out_json) {
  // clang-format off
  out_json.append_copy(u8R"--({)--"
    u8R"--("jsonrpc":"2.0",)--"
    u8R"--("method":"window/showMessage",)--"
    u8R"--("params":{)--"
      u8R"--("type":2,)--"
      u8R"--("message":"Failed to load configuration file for )--");
  // clang-format on
  write_json_escaped_string(out_json, to_string8_view(document_path));
  out_json.append_copy(u8". Using default configuration.\\nError details: ");
  write_json_escaped_string(out_json, to_string8_view(error_details));
  out_json.append_copy(u8"\"}}");
}

template <QLJS_LSP_LINTER Linter>
void linting_lsp_server_handler<Linter>::apply_document_changes(
    quick_lint_js::document<lsp_locator>& doc,
    ::simdjson::ondemand::array& changes) {
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
    configuration& config, padded_string_view code, string8_view uri_json,
    string8_view version_json, byte_buffer& notification_json) {
  // clang-format off
  notification_json.append_copy(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--");
  // clang-format on
  notification_json.append_copy(uri_json);

  notification_json.append_copy(u8R"--(,"version":)--");
  notification_json.append_copy(version_json);

  notification_json.append_copy(u8R"--(,"diagnostics":)--");
  this->lint_and_get_diagnostics(config, code, notification_json);

  notification_json.append_copy(u8R"--(},"jsonrpc":"2.0"})--");
}

void lsp_javascript_linter::lint_and_get_diagnostics(
    configuration& config, padded_string_view code,
    byte_buffer& diagnostics_json) {
  lsp_error_reporter error_reporter(diagnostics_json, code);

  parser p(code, &error_reporter);
  linter l(&error_reporter, &config.globals());
#if QLJS_HAVE_SETJMP
  bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(l);
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
    configuration& config, padded_string_view code, string8_view uri_json,
    string8_view version_json, byte_buffer& notification_json) {
  this->callback_(config, code, uri_json, version_json, notification_json);
}

template class linting_lsp_server_handler<lsp_javascript_linter>;
template class linting_lsp_server_handler<mock_lsp_linter>;

namespace {
void append_raw_json(::simdjson::ondemand::value& value, byte_buffer& out) {
  out.append_copy(get_raw_json(value));
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

string8_view get_raw_json(::simdjson::ondemand::value& value) {
  ::simdjson::ondemand::json_type type;
  if (value.type().get(type) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  switch (type) {
  case ::simdjson::ondemand::json_type::boolean:
  case ::simdjson::ondemand::json_type::null:
  case ::simdjson::ondemand::json_type::number:
  case ::simdjson::ondemand::json_type::string:
    return to_string8_view(value.raw_json_token());

  case ::simdjson::ondemand::json_type::array:
  case ::simdjson::ondemand::json_type::object:
    QLJS_UNIMPLEMENTED();
  }
  QLJS_UNREACHABLE();
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
