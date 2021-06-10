// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_SERVER_H
#define QUICK_LINT_JS_LSP_SERVER_H

#include <cstddef>
#include <functional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <simdjson.h>
#include <unordered_map>

#if QLJS_HAVE_CXX_CONCEPTS
#define QLJS_LSP_LINTER ::quick_lint_js::lsp_linter
#else
#define QLJS_LSP_LINTER class
#endif

namespace quick_lint_js {
class byte_buffer;
class configuration;
class configuration_filesystem;

#if QLJS_HAVE_CXX_CONCEPTS
template <class Linter>
concept lsp_linter = requires(Linter l, configuration config,
                              padded_string_view code,
                              ::simdjson::ondemand::value uri,
                              ::simdjson::ondemand::value version,
                              byte_buffer notification_json) {
  {l.lint_and_get_diagnostics_notification(config, code, uri, version,
                                           notification_json)};
};
#endif

// A linting_lsp_server_handler listens for JavaScript code changes and notifies
// the client of diagnostics.
template <QLJS_LSP_LINTER Linter>
class linting_lsp_server_handler {
 public:
  template <class... LinterArgs>
  explicit linting_lsp_server_handler(configuration_filesystem* fs,
                                      LinterArgs&&... linter_args)
      : config_loader_(fs), linter_(std::forward<LinterArgs>(linter_args)...) {}

  void handle_request(::simdjson::ondemand::object& request,
                      byte_buffer& response_json);
  void handle_notification(::simdjson::ondemand::object& request,
                           byte_buffer& notification_json);

 private:
  void handle_initialize_request(::simdjson::ondemand::object& request,
                                 byte_buffer& response_json);
  void handle_shutdown_request(::simdjson::ondemand::object& request,
                               byte_buffer& response_json);

  void handle_text_document_did_change_notification(
      ::simdjson::ondemand::object& request, byte_buffer& notification_json);
  void handle_text_document_did_close_notification(
      ::simdjson::ondemand::object& request);
  void handle_text_document_did_open_notification(
      ::simdjson::ondemand::object& request, byte_buffer& notification_json);

  configuration* get_config(::simdjson::ondemand::value& document_uri);

  static void apply_document_changes(document<lsp_locator>& doc,
                                     ::simdjson::ondemand::array& changes);

  configuration_loader config_loader_;
  Linter linter_;
  std::unordered_map<string8, document<lsp_locator>> documents_;
  bool shutdown_requested_ = false;
};

class lsp_javascript_linter {
 public:
  void lint_and_get_diagnostics_notification(
      configuration&, padded_string_view code, ::simdjson::ondemand::value& uri,
      ::simdjson::ondemand::value& version, byte_buffer& notification_json);

 private:
  void lint_and_get_diagnostics(configuration&, padded_string_view code,
                                byte_buffer& diagnostics_json);
};

class mock_lsp_linter {
 public:
  using lint_and_get_diagnostics_notification_type = void(
      configuration&, padded_string_view code, ::simdjson::ondemand::value& uri,
      ::simdjson::ondemand::value& version, byte_buffer& notification_json);

  /*implicit*/ mock_lsp_linter(
      std::function<lint_and_get_diagnostics_notification_type> callback);

  void lint_and_get_diagnostics_notification(
      configuration&, padded_string_view code, ::simdjson::ondemand::value& uri,
      ::simdjson::ondemand::value& version, byte_buffer& notification_json);

 private:
  std::function<lint_and_get_diagnostics_notification_type> callback_;
};

extern template class linting_lsp_server_handler<lsp_javascript_linter>;
extern template class linting_lsp_server_handler<mock_lsp_linter>;
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
