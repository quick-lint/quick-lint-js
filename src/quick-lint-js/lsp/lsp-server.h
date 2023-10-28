// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/lsp/lsp-message-parser.h>
#include <quick-lint-js/lsp/lsp-workspace-configuration.h>
#include <quick-lint-js/lsp/outgoing-json-rpc-message-queue.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/simdjson-fwd.h>
#include <quick-lint-js/simdjson.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/synchronized.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class Byte_Buffer;
class Linting_LSP_Server_Handler;
class LSP_Linter;
class Trace_Flusher_Directory_Backend;
struct Watch_IO_Error;

// A Configuration_Filesystem which allows unsaved LSP documents (from the
// client) to appear as real files.
class LSP_Overlay_Configuration_Filesystem : public Configuration_Filesystem {
 public:
  explicit LSP_Overlay_Configuration_Filesystem(
      Configuration_Filesystem* underlying_fs);

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
      const std::string&) override;
  Result<Padded_String, Read_File_IO_Error> read_file(
      const Canonical_Path&) override;

  void open_document(const std::string&, LSP_Document_Text*);
  void close_document(const std::string&);

 private:
  Configuration_Filesystem* underlying_fs_;

  // See NOTE[lsp_documents thread safety].
  Hash_Map<std::string, LSP_Document_Text*> overlaid_documents_;
};

struct Linting_LSP_Server_Config {
  std::string tracing_directory;
};

struct LSP_Documents {
  enum class Document_Type {
    config,    // Config_Document
    lintable,  // Lintable_Document
    unknown,   // Unknown_Document
  };

  struct Document_Base {
    explicit Document_Base(Document_Type type);

    virtual ~Document_Base() = default;

    virtual void on_config_file_changed(Linting_LSP_Server_Handler&,
                                        String8_View document_uri,
                                        const Configuration_Change&) = 0;

    Trace_LSP_Document_Type trace_type() const;

    Document_Type type;

    LSP_Document_Text doc;
    std::string language_id;
    String8 version_json;
  };

  // quick-lint-js.config
  struct Config_Document final : Document_Base {
    explicit Config_Document();

    void on_config_file_changed(Linting_LSP_Server_Handler&,
                                String8_View document_uri,
                                const Configuration_Change&) override;
  };

  // .js file
  struct Lintable_Document final : Document_Base {
    explicit Lintable_Document();

    void on_config_file_changed(Linting_LSP_Server_Handler&,
                                String8_View document_uri,
                                const Configuration_Change&) override;

    Configuration* config;
    Linter_Options lint_options;
  };

  struct Unknown_Document final : Document_Base {
    explicit Unknown_Document();

    void on_config_file_changed(Linting_LSP_Server_Handler&,
                                String8_View document_uri,
                                const Configuration_Change&) override;
  };

  // Key: URI
  Hash_Map<String8, std::unique_ptr<Document_Base>> documents;
};

// A Linting_LSP_Server_Handler listens for JavaScript code changes and notifies
// the client of diagnostics.
class Linting_LSP_Server_Handler final : public JSON_RPC_Message_Handler {
 public:
  explicit Linting_LSP_Server_Handler(Configuration_Filesystem* fs,
                                      LSP_Linter* linter);
  ~Linting_LSP_Server_Handler() override;

  Linting_LSP_Server_Config& server_config() { return this->server_config_; }

  void handle_request(::simdjson::ondemand::object& request,
                      std::string_view method, String8_View id_json) override;
  void handle_response(JSON_RPC_Message_Handler::Request_ID_Type request_id,
                       ::simdjson::ondemand::value& result) override;
  void handle_error_response(
      JSON_RPC_Message_Handler::Request_ID_Type request_id, std::int64_t code,
      std::string_view message) override;
  void handle_notification(::simdjson::ondemand::object& request,
                           std::string_view method) override;

  void filesystem_changed();

  // Sends notifications and requests to the client.
  // TODO(strager): Rename.
  void flush_pending_notifications(LSP_Endpoint_Remote& remote) {
    this->outgoing_messages_.send(remote);
  }

  void add_watch_io_errors(const std::vector<Watch_IO_Error>&);

 private:
  void handle_initialize_request(::simdjson::ondemand::object& request,
                                 String8_View id_json);
  void handle_shutdown_request(::simdjson::ondemand::object& request,
                               String8_View id_json);

  void handle_workspace_configuration_response(
      ::simdjson::ondemand::value& result);

  void handle_initialized_notification();

  struct LSP_Text_Document_Did_Change_Notification {
    String_JSON_Token uri;
    String8_View version_json;
    ::simdjson::ondemand::array& changes;
  };
  void handle_text_document_did_change_notification(
      ::simdjson::ondemand::object& request);
  void handle_text_document_did_change_notification(
      const LSP_Text_Document_Did_Change_Notification& notification);

  struct LSP_Text_Document_Did_Close_Notification {
    String8_View uri;
  };
  void handle_text_document_did_close_notification(
      ::simdjson::ondemand::object& request);
  void handle_text_document_did_close_notification(
      const LSP_Text_Document_Did_Close_Notification& notification);

  struct LSP_Text_Document_Did_Open_Notification {
    std::string_view language_id;
    String_JSON_Token uri;
    String8_View version_json;
    String8_View text;
  };
  void handle_text_document_did_open_notification(
      ::simdjson::ondemand::object& request);
  void handle_text_document_did_open_notification(
      const LSP_Text_Document_Did_Open_Notification&);

  void handle_workspace_did_change_configuration_notification(
      ::simdjson::ondemand::object& request);

  void handle_config_file_changes(
      Lock_Ptr<LSP_Documents>& documents,
      const std::vector<Configuration_Change>& config_changes);

  void get_config_file_diagnostics_notification(Loaded_Config_File*,
                                                String8_View uri_json,
                                                String8_View version_json,
                                                Byte_Buffer& notification_json);

  void write_configuration_loader_error_notification(
      std::string_view document_path, std::string_view error_details,
      Byte_Buffer& out_json);
  void write_configuration_errors_notification(std::string_view document_path,
                                               Loaded_Config_File*,
                                               Byte_Buffer& out_json);

  struct LSP_Document_Change {
    String8_View text;
    // If a range is not provided, the document's text is entirely replaced.
    std::optional<LSP_Range> range;
  };
  static void apply_document_changes(LSP_Document_Text& doc,
                                     ::simdjson::ondemand::array& changes);
  static void apply_document_change(LSP_Document_Text& doc,
                                    ::simdjson::ondemand::object& raw_change);
  static void apply_document_change(LSP_Document_Text& doc,
                                    const LSP_Document_Change& change);

  void write_method_not_found_error_response(String8_View request_id_json);

  LSP_Overlay_Configuration_Filesystem config_fs_;
  Configuration_Loader config_loader_;
  Configuration default_config_;
  LSP_Linter& linter_;

  // NOTE[lsp_documents thread safety]: LSP_Documents can only be modified on
  // the LSP server thread. Therefore, it is safe to read without taking the
  // lock. LSP_Overlay_Configuration_Filesystem reads without taking the lock.
  Synchronized<LSP_Documents> documents_;

  Monotonic_Allocator workspace_configuration_allocator_{
      "Linting_LSP_Server_Handler::workspace_configuration_allocator_"};

  Outgoing_JSON_RPC_Message_Queue outgoing_messages_;
  Linting_LSP_Server_Config server_config_;
  LSP_Workspace_Configuration workspace_configuration_{
      &this->workspace_configuration_allocator_};
  std::unique_ptr<Trace_Flusher_Directory_Backend> tracer_backend_;
  bool did_report_watch_io_error_ = false;
  bool shutdown_requested_ = false;

  friend class LSP_Linter;
  friend struct LSP_Documents::Config_Document;
  friend struct LSP_Documents::Lintable_Document;
};

class LSP_Linter {
 public:
  LSP_Linter() = default;

  LSP_Linter(const LSP_Linter&) = default;
  LSP_Linter(LSP_Linter&&) = default;
  LSP_Linter& operator=(const LSP_Linter&) = default;
  LSP_Linter& operator=(LSP_Linter&&) = default;

  virtual ~LSP_Linter();

  virtual void lint(Configuration& config, Linter_Options lint_options,
                    Padded_String_View code, String8_View uri_json,
                    String8_View version_json,
                    Outgoing_JSON_RPC_Message_Queue&) = 0;

  void lint(LSP_Documents::Lintable_Document&, String8_View uri_json,
            Outgoing_JSON_RPC_Message_Queue&);
};

class LSP_JavaScript_Linter final : public LSP_Linter {
 public:
  ~LSP_JavaScript_Linter() override = default;

  void lint(Configuration&, Linter_Options, Padded_String_View code,
            String8_View uri_json, String8_View version_json,
            Outgoing_JSON_RPC_Message_Queue&) override;

 private:
  void lint_and_get_diagnostics(Configuration&, Linter_Options,
                                Padded_String_View code,
                                Byte_Buffer& diagnostics_json);
};

// Returns the LSP_Documents for the last-created Linting_LSP_Server_Handler.
//
// Might return nullptr.
//
// In tests, the returned pointer might change. In production, once the returned
// pointer returns non-null, it will not change.
Synchronized<LSP_Documents>* get_lsp_server_documents();

// For testing only.
void set_lsp_server_documents(Synchronized<LSP_Documents>*);
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
