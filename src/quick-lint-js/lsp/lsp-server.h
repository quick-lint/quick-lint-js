// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_LSP_SERVER_H
#define QUICK_LINT_JS_LSP_LSP_SERVER_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lsp/lsp-endpoint.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/lsp/lsp-message-parser.h>
#include <quick-lint-js/lsp/lsp-workspace-configuration.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/simdjson-fwd.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class byte_buffer;
class lsp_linter;
class trace_flusher_directory_backend;
struct watch_io_error;

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
  hash_map<std::string, document<lsp_locator>*> overlaid_documents_;
};

struct linting_lsp_server_config {
  std::string tracing_directory;
};

// List of asynchronous LSP messages (requests and notifications) to send to the
// client.
class outgoing_lsp_message_queue {
 public:
  byte_buffer& new_message();

  void send(lsp_endpoint_remote&);

 private:
  std::vector<byte_buffer> messages_;
};

// A linting_lsp_server_handler listens for JavaScript code changes and notifies
// the client of diagnostics.
class linting_lsp_server_handler final : public lsp_endpoint_handler {
 public:
  explicit linting_lsp_server_handler(configuration_filesystem* fs,
                                      lsp_linter* linter);
  ~linting_lsp_server_handler() override;

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
    this->outgoing_messages_.send(remote);
  }

  void add_watch_io_errors(const std::vector<watch_io_error>&);

 private:
  struct document_base {
    virtual ~document_base() = default;

    virtual void on_text_changed(linting_lsp_server_handler&,
                                 string8_view document_uri_json) = 0;
    virtual void on_config_file_changed(linting_lsp_server_handler&,
                                        string8_view document_uri,
                                        const configuration_change&) = 0;

    quick_lint_js::document<lsp_locator> doc;
    string8 version_json;
  };

  // quick-lint-js.config
  struct config_document final : document_base {
    void on_text_changed(linting_lsp_server_handler&,
                         string8_view document_uri_json) override;
    void on_config_file_changed(linting_lsp_server_handler&,
                                string8_view document_uri,
                                const configuration_change&) override;
  };

  // .js file
  struct lintable_document final : document_base {
    void on_text_changed(linting_lsp_server_handler&,
                         string8_view document_uri_json) override;
    void on_config_file_changed(linting_lsp_server_handler&,
                                string8_view document_uri,
                                const configuration_change&) override;

    configuration* config;
    linter_options lint_options;
  };

  struct unknown_document final : document_base {
    void on_text_changed(linting_lsp_server_handler&,
                         string8_view document_uri_json) override;
    void on_config_file_changed(linting_lsp_server_handler&,
                                string8_view document_uri,
                                const configuration_change&) override;
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
  void handle_workspace_did_change_configuration_notification(
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
  hash_map<string8, std::unique_ptr<document_base> > documents_;
  outgoing_lsp_message_queue outgoing_messages_;
  linting_lsp_server_config server_config_;
  lsp_workspace_configuration workspace_configuration_;
  std::unique_ptr<trace_flusher_directory_backend> tracer_backend_;
  bool did_report_watch_io_error_ = false;
  bool shutdown_requested_ = false;

  friend class lsp_linter;
};

class lsp_linter {
 public:
  lsp_linter() = default;

  lsp_linter(const lsp_linter&) = default;
  lsp_linter(lsp_linter&&) = default;
  lsp_linter& operator=(const lsp_linter&) = default;
  lsp_linter& operator=(lsp_linter&&) = default;

  virtual ~lsp_linter();

  virtual void lint(configuration& config, linter_options lint_options,
                    padded_string_view code, string8_view uri_json,
                    string8_view version_json, outgoing_lsp_message_queue&) = 0;

  void lint(linting_lsp_server_handler::lintable_document&,
            string8_view uri_json, outgoing_lsp_message_queue&);
};

class lsp_javascript_linter final : public lsp_linter {
 public:
  ~lsp_javascript_linter() override = default;

  void lint(configuration&, linter_options, padded_string_view code,
            string8_view uri_json, string8_view version_json,
            outgoing_lsp_message_queue&) override;

 private:
  void lint_and_get_diagnostics(configuration&, linter_options,
                                padded_string_view code,
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
