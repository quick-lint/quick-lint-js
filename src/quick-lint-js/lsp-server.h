// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_SERVER_H
#define QUICK_LINT_JS_LSP_SERVER_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/lsp-workspace-configuration.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <simdjson.h>
#include <string>
#include <unordered_map>
#include <vector>

namespace quick_lint_js {
class byte_buffer;
class trace_flusher;
struct watch_io_error;

class lsp_linter {
 public:
  lsp_linter() = default;

  lsp_linter(const lsp_linter&) = default;
  lsp_linter(lsp_linter&&) = default;
  lsp_linter& operator=(const lsp_linter&) = default;
  lsp_linter& operator=(lsp_linter&&) = default;

  virtual ~lsp_linter();

  virtual void lint_and_get_diagnostics_notification(
      configuration& config, padded_string_view code, string8_view uri_json,
      string8_view version_json, byte_buffer& notification_json) = 0;
};

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

struct linting_lsp_server_config {
  std::string tracing_directory;
};

// A linting_lsp_server_handler listens for JavaScript code changes and notifies
// the client of diagnostics.
class linting_lsp_server_handler final : public lsp_endpoint_handler {
 public:
  explicit linting_lsp_server_handler(configuration_filesystem* fs,
                                      lsp_linter* linter);
  explicit linting_lsp_server_handler(configuration_filesystem* fs,
                                      lsp_linter* linter,
                                      trace_flusher* tracer);

  linting_lsp_server_config& server_config() noexcept {
    return this->server_config_;
  }

  void handle_request(::simdjson::ondemand::object& request,
                      std::string_view method, string8_view id_json,
                      byte_buffer& response_json) override;
  void handle_response(lsp_endpoint_handler::request_id_type request_id,
                       ::simdjson::ondemand::value& result) override;
  void handle_error_response(lsp_endpoint_handler::request_id_type request_id,
                             std::int64_t code,
                             std::string_view message) override;
  void handle_notification(::simdjson::ondemand::object& request,
                           std::string_view method) override;

  void filesystem_changed();

  // Sends notifications and requests to the client.
  // TODO(strager): Rename.
  void flush_pending_notifications(lsp_endpoint_remote& remote) {
    for (byte_buffer& notification_json : this->pending_notification_jsons_) {
      if (notification_json.empty()) {
        // TODO(strager): Fix our tests so they don't make empty
        // byte_buffer-s.
        continue;
      }
      remote.send_message(std::move(notification_json));
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
                                 string8_view id_json,
                                 byte_buffer& response_json);
  void handle_shutdown_request(::simdjson::ondemand::object& request,
                               string8_view id_json,
                               byte_buffer& response_json);

  void handle_workspace_configuration_response(
      ::simdjson::ondemand::value& result);

  void handle_initialized_notification();
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

  static void write_method_not_found_error_response(
      string8_view request_id_json, byte_buffer&);
  static void write_invalid_request_error_response(byte_buffer&);

  lsp_overlay_configuration_filesystem config_fs_;
  configuration_loader config_loader_;
  configuration default_config_;
  lsp_linter& linter_;
  std::unordered_map<string8, document> documents_;
  // Stores notifications and requests destined for the client.
  // TODO(strager): Rename.
  std::vector<byte_buffer> pending_notification_jsons_;
  linting_lsp_server_config server_config_;
  lsp_workspace_configuration workspace_configuration_;
  trace_flusher* tracer_;
  bool did_report_watch_io_error_ = false;
  bool shutdown_requested_ = false;
};

class lsp_javascript_linter final : public lsp_linter {
 public:
  ~lsp_javascript_linter() override = default;

  void lint_and_get_diagnostics_notification(
      configuration&, padded_string_view code, string8_view uri_json,
      string8_view version_json, byte_buffer& notification_json) override;

 private:
  void lint_and_get_diagnostics(configuration&, padded_string_view code,
                                byte_buffer& diagnostics_json);
};
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
