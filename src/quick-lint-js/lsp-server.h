// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_SERVER_H
#define QUICK_LINT_JS_LSP_SERVER_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <functional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <simdjson.h>
#include <unordered_map>
#include <vector>

#if QLJS_HAVE_CXX_CONCEPTS
#define QLJS_LSP_LINTER ::quick_lint_js::lsp_linter
#else
#define QLJS_LSP_LINTER class
#endif

namespace quick_lint_js {
class byte_buffer;
class configuration;
class configuration_filesystem;
struct watch_io_error;

#if QLJS_HAVE_CXX_CONCEPTS
template <class Linter>
concept lsp_linter = requires(Linter l, configuration config,
                              padded_string_view code, string8_view uri_json,
                              string8_view version_json,
                              byte_buffer notification_json) {
  {l.lint_and_get_diagnostics_notification(config, code, uri_json, version_json,
                                           notification_json)};
};
#endif

// A configuration_filesystem which allows unsaved LSP documents (from the
// client) to appear as real files.
class lsp_overlay_configuration_filesystem : public configuration_filesystem {
 public:
  explicit lsp_overlay_configuration_filesystem(
      configuration_filesystem* underlying_fs);

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string&) override;
  result<padded_string, read_file_io_error> read_file(
      const canonical_path&) override;

  void open_document(const std::string&, document<lsp_locator>*);
  void close_document(const std::string&);

 private:
  configuration_filesystem* underlying_fs_;
  std::unordered_map<std::string, document<lsp_locator>*> overlaid_documents_;
};

// A linting_lsp_server_handler listens for JavaScript code changes and notifies
// the client of diagnostics.
template <QLJS_LSP_LINTER Linter>
class linting_lsp_server_handler {
 public:
  template <class... LinterArgs>
  explicit linting_lsp_server_handler(configuration_filesystem* fs,
                                      LinterArgs&&... linter_args)
      : config_fs_(fs),
        config_loader_(&this->config_fs_),
        linter_(std::forward<LinterArgs>(linter_args)...) {}

  void handle_request(::simdjson::ondemand::object& request,
                      std::string_view method, byte_buffer& response_json);
  void handle_notification(::simdjson::ondemand::object& request,
                           std::string_view method);

  void filesystem_changed();

  template <class Func>
  void take_pending_notification_jsons(Func&& callback) noexcept {
    for (byte_buffer& notification_json : this->pending_notification_jsons_) {
      callback(std::move(notification_json));
    }
    this->pending_notification_jsons_.clear();
  }

  void add_watch_io_errors(const std::vector<watch_io_error>&);

 private:
  enum class document_type {
    config,    // quick-lint-js.config
    lintable,  // .js file
    unknown,
  };

  struct document {
    quick_lint_js::document<lsp_locator> doc;
    document_type type = document_type::unknown;
    string8 version_json;

    // Used only if type == document_type::lintable.
    configuration* config;
  };

  void handle_initialize_request(::simdjson::ondemand::object& request,
                                 byte_buffer& response_json);
  void handle_shutdown_request(::simdjson::ondemand::object& request,
                               byte_buffer& response_json);

  void handle_text_document_did_change_notification(
      ::simdjson::ondemand::object& request);
  void handle_text_document_did_close_notification(
      ::simdjson::ondemand::object& request);
  void handle_text_document_did_open_notification(
      ::simdjson::ondemand::object& request);

  void handle_config_file_changes(
      const std::vector<configuration_change>& config_changes);

  void get_config_file_diagnostics_notification(loaded_config_file*,
                                                string8_view uri_json,
                                                string8_view version_json,
                                                byte_buffer& notification_json);

  void write_configuration_loader_error_notification(
      std::string_view document_path, std::string_view error_details,
      byte_buffer& out_json);
  void write_configuration_errors_notification(std::string_view document_path,
                                               loaded_config_file*,
                                               byte_buffer& out_json);

  static void apply_document_changes(quick_lint_js::document<lsp_locator>& doc,
                                     ::simdjson::ondemand::array& changes);

  static void write_method_not_found_error_response(byte_buffer&);
  static void write_invalid_request_error_response(byte_buffer&);

  lsp_overlay_configuration_filesystem config_fs_;
  configuration_loader config_loader_;
  configuration default_config_;
  Linter linter_;
  std::unordered_map<string8, document> documents_;
  std::vector<byte_buffer> pending_notification_jsons_;
  bool did_report_watch_io_error_ = false;
  bool shutdown_requested_ = false;
};

class lsp_javascript_linter {
 public:
  void lint_and_get_diagnostics_notification(configuration&,
                                             padded_string_view code,
                                             string8_view uri_json,
                                             string8_view version_json,
                                             byte_buffer& notification_json);

 private:
  void lint_and_get_diagnostics(configuration&, padded_string_view code,
                                byte_buffer& diagnostics_json);
};

class mock_lsp_linter {
 public:
  using lint_and_get_diagnostics_notification_type =
      void(configuration&, padded_string_view code, string8_view uri_json,
           string8_view version_json, byte_buffer& notification_json);

  /*implicit*/ mock_lsp_linter(
      std::function<lint_and_get_diagnostics_notification_type> callback);

  void lint_and_get_diagnostics_notification(configuration&,
                                             padded_string_view code,
                                             string8_view uri_json,
                                             string8_view version_json,
                                             byte_buffer& notification_json);

 private:
  std::function<lint_and_get_diagnostics_notification_type> callback_;
};

extern template class linting_lsp_server_handler<lsp_javascript_linter>;
extern template class linting_lsp_server_handler<mock_lsp_linter>;
}

#endif

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
