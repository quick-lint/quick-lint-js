// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/debug/debug-probe.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/lsp/lsp-diag-reporter.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-language.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/lsp/lsp-uri.h>
#include <quick-lint-js/lsp/outgoing-json-rpc-message-queue.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/simdjson.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/version.h>
#include <simdjson.h>
#include <string>
#include <utility>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
constexpr JSON_RPC_Message_Handler::Request_ID_Type
    initial_configuration_request_id = 1;

// Returns std::nullopt on failure (e.g. missing key or not a string).
std::optional<String_JSON_Token> maybe_get_string_token(
    ::simdjson::ondemand::value& string);
std::optional<String_JSON_Token> maybe_get_string_token(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& string);

std::atomic<Synchronized<LSP_Documents>*> latest_lsp_server_documents{nullptr};
}

LSP_Overlay_Configuration_Filesystem::LSP_Overlay_Configuration_Filesystem(
    Configuration_Filesystem* underlying_fs)
    : underlying_fs_(underlying_fs) {}

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error>
LSP_Overlay_Configuration_Filesystem::canonicalize_path(
    const std::string& path) {
  return this->underlying_fs_->canonicalize_path(path);
}

Linting_LSP_Server_Handler::~Linting_LSP_Server_Handler() {
  // We are going to deallocate this->tracer_backend_, so unregister it with
  // the trace_flusher.
  if (this->tracer_backend_) {
    Trace_Flusher::instance()->disable_backend(this->tracer_backend_.get());
  }
}

Result<Padded_String, Read_File_IO_Error>
LSP_Overlay_Configuration_Filesystem::read_file(const Canonical_Path& path) {
#if QLJS_HAVE_STD_TRANSPARENT_KEYS
  std::string_view key = path.path();
#else
  std::string key(path.path());
#endif
  auto doc_it = this->overlaid_documents_.find(key);
  if (doc_it == this->overlaid_documents_.end()) {
    return this->underlying_fs_->read_file(path);
  }
  return Padded_String(doc_it->second->string().string_view());
}

void LSP_Overlay_Configuration_Filesystem::open_document(
    const std::string& path, LSP_Document_Text* doc) {
  auto [_it, inserted] = this->overlaid_documents_.emplace(path, doc);
  QLJS_ASSERT(inserted);
}

void LSP_Overlay_Configuration_Filesystem::close_document(
    const std::string& path) {
  std::size_t erased = this->overlaid_documents_.erase(path);
  QLJS_ASSERT(erased > 0);
}

LSP_Documents::Document_Base::Document_Base(Document_Type type) : type(type) {}

Trace_LSP_Document_Type LSP_Documents::Document_Base::trace_type() const {
  switch (this->type) {
  case Document_Type::config:
    return Trace_LSP_Document_Type::config;
  case Document_Type::lintable:
    return Trace_LSP_Document_Type::lintable;
  case Document_Type::unknown:
    return Trace_LSP_Document_Type::unknown;
  }
  QLJS_UNREACHABLE();
}

LSP_Documents::Config_Document::Config_Document()
    : Document_Base(Document_Type::config) {}

LSP_Documents::Lintable_Document::Lintable_Document()
    : Document_Base(Document_Type::lintable) {}

LSP_Documents::Unknown_Document::Unknown_Document()
    : Document_Base(Document_Type::unknown) {}

Linting_LSP_Server_Handler::Linting_LSP_Server_Handler(
    Configuration_Filesystem* fs, LSP_Linter* linter)
    : config_fs_(fs), config_loader_(&this->config_fs_), linter_(*linter) {
  set_lsp_server_documents(&this->documents_);

  this->workspace_configuration_.add_item(
      u8"quick-lint-js.tracing-directory"_sv,
      *this->workspace_configuration_allocator_.new_object_copy(
          [this](std::string_view new_value) -> void {
            bool changed = this->server_config_.tracing_directory != new_value;
            if (changed) {
              this->server_config_.tracing_directory = new_value;
              if (this->tracer_backend_) {
                Trace_Flusher::instance()->disable_backend(
                    this->tracer_backend_.get());
                this->tracer_backend_.reset();
              }
              if (!this->server_config_.tracing_directory.empty()) {
                auto new_backend =
                    Trace_Flusher_Directory_Backend::create_child_directory(
                        this->server_config_.tracing_directory);
                if (new_backend) {
                  this->tracer_backend_ =
                      std::make_unique<Trace_Flusher_Directory_Backend>(
                          std::move(*new_backend));
                  Trace_Flusher::instance()->enable_backend(
                      this->tracer_backend_.get());
                  QLJS_DEBUG_LOG(
                      "enabled tracing in directory %s\n",
                      this->tracer_backend_->trace_directory().c_str());
                }
              }
            }
          }));
}

void Linting_LSP_Server_Handler::handle_request(
    ::simdjson::ondemand::object& request, std::string_view method,
    String8_View id_json) {
  if (method == "initialize") {
    this->handle_initialize_request(request, id_json);
  } else if (method == "shutdown") {
    this->handle_shutdown_request(request, id_json);
  } else {
    this->write_method_not_found_error_response(id_json);
  }
}

void Linting_LSP_Server_Handler::handle_response(
    JSON_RPC_Message_Handler::Request_ID_Type request_id,
    ::simdjson::ondemand::value& result) {
  if (request_id == initial_configuration_request_id) {
    this->handle_workspace_configuration_response(result);
  } else {
    QLJS_DEBUG_LOG("received response for unknown request\n");
    // TODO(strager): Report an error.
  }
}

void Linting_LSP_Server_Handler::handle_error_response(
    JSON_RPC_Message_Handler::Request_ID_Type request_id, std::int64_t code,
    std::string_view message) {
  static_cast<void>(code);
  static_cast<void>(message);
  if (request_id == initial_configuration_request_id) {
    // Do nothing.
  } else {
    QLJS_DEBUG_LOG("received error response for unknown request\n");
    // TODO(strager): Report an error.
  }
}

void Linting_LSP_Server_Handler::handle_notification(
    ::simdjson::ondemand::object& request, std::string_view method) {
  if (method == "textDocument/didChange") {
    this->handle_text_document_did_change_notification(request);
  } else if (method == "textDocument/didOpen") {
    this->handle_text_document_did_open_notification(request);
  } else if (method == "textDocument/didClose") {
    this->handle_text_document_did_close_notification(request);
  } else if (method == "workspace/didChangeConfiguration") {
    this->handle_workspace_did_change_configuration_notification(request);
  } else if (method == "initialized") {
    this->handle_initialized_notification();
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
    // Ignore unknown notification methods.
  }
}

void Linting_LSP_Server_Handler::filesystem_changed() {
  Monotonic_Allocator temporary_allocator(
      "Linting_LSP_Server_Handler::filesystem_changed");
  Span<Configuration_Change> config_changes =
      this->config_loader_.refresh(&temporary_allocator);
  {
    Lock_Ptr<LSP_Documents> documents = this->documents_.lock();
    this->handle_config_file_changes(documents, config_changes);
  }
}

void Linting_LSP_Server_Handler::add_watch_io_errors(
    Span<const Watch_IO_Error> errors) {
  if (!errors.empty() && !this->did_report_watch_io_error_) {
    Byte_Buffer& out_json = this->outgoing_messages_.new_message();
    // clang-format off
    out_json.append_copy(u8R"--({)--"
      u8R"--("jsonrpc":"2.0",)--"
      u8R"--("method":"window/showMessage",)--"
      u8R"--("params":{)--"
        u8R"--("type":2,)--"
        u8R"--("message":")--"_sv);
    // clang-format on
    write_json_escaped_string(out_json, to_string8_view(errors[0].to_string()));
    out_json.append_copy(u8"\"}}"_sv);
    this->did_report_watch_io_error_ = true;
  }
}

void Linting_LSP_Server_Handler::handle_initialize_request(
    ::simdjson::ondemand::object& request, String8_View id_json) {
  ::simdjson::ondemand::object params;
  if (get_object(request, "params", &params)) {
    ::simdjson::ondemand::object initialization_options;
    if (get_object(params, "initializationOptions", &initialization_options)) {
      ::simdjson::ondemand::object configuration;
      if (get_object(initialization_options, "configuration", &configuration)) {
        bool ok = this->workspace_configuration_.process_initialization_options(
            configuration);
        if (!ok) {
          QLJS_DEBUG_LOG(
              "failed to process configuration in initializationOptions\n");
          // TODO(strager): Report an error.
          return;
        }
      }
    }
  }

  Byte_Buffer& response_json = this->outgoing_messages_.new_message();
  response_json.append_copy(u8R"--({"id":)--"_sv);
  response_json.append_copy(id_json);
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
    u8R"--("jsonrpc":"2.0"})--"_sv);
  // clang-format on
}

void Linting_LSP_Server_Handler::handle_shutdown_request(
    ::simdjson::ondemand::object&, String8_View id_json) {
  Byte_Buffer& response_json = this->outgoing_messages_.new_message();
  this->shutdown_requested_ = true;
  response_json.append_copy(u8R"--({"jsonrpc":"2.0","id":)--"_sv);
  response_json.append_copy(id_json);
  response_json.append_copy(u8R"--(,"result":null})--"_sv);
}

void Linting_LSP_Server_Handler::handle_workspace_configuration_response(
    ::simdjson::ondemand::value& result) {
  bool ok = this->workspace_configuration_.process_response(result);
  if (!ok) {
    QLJS_DEBUG_LOG("failed to process configuration response\n");
    // TODO(strager): Report an error.
    return;
  }
}

void Linting_LSP_Server_Handler::handle_initialized_notification() {
  Byte_Buffer& request_json = this->outgoing_messages_.new_message();
  this->workspace_configuration_.build_request(initial_configuration_request_id,
                                               request_json);
}

void Linting_LSP_Server_Handler::handle_text_document_did_change_notification(
    ::simdjson::ondemand::object& request) {
  ::simdjson::ondemand::object text_document;
  if (!get_object(request, "params", "textDocument", &text_document)) {
    // Ignore invalid notification.
    return;
  }
  std::optional<String_JSON_Token> uri =
      maybe_get_string_token(text_document["uri"]);
  if (!uri.has_value()) {
    // Ignore invalid notification.
    return;
  }
  ::simdjson::ondemand::value version;
  if (!get_value(text_document, "version", &version)) {
    // Ignore invalid notification.
    return;
  }

  ::simdjson::ondemand::array changes;
  if (!get_array(request, "params", "contentChanges", &changes)) {
    // Ignore invalid notification.
    return;
  }

  this->handle_text_document_did_change_notification(
      LSP_Text_Document_Did_Change_Notification{
          .uri = *uri,
          .version_json = get_raw_json(version),
          .changes = changes,
      });

  debug_probe_publish_lsp_documents();
}

void Linting_LSP_Server_Handler::handle_text_document_did_change_notification(
    const LSP_Text_Document_Did_Change_Notification& notification) {
  Lock_Ptr<LSP_Documents> documents = this->documents_.lock();
  auto document_it = documents->documents.find(String8(notification.uri.data));
  bool url_is_tracked = document_it != documents->documents.end();
  if (!url_is_tracked) {
    return;
  }
  LSP_Documents::Document_Base& doc = *document_it->second;

  std::string document_path = parse_file_from_lsp_uri(notification.uri.data);
  if (document_path.empty()) {
    // TODO(strager): Report a warning and use a default configuration.
    QLJS_UNIMPLEMENTED();
  }

  this->apply_document_changes(doc.doc, notification.changes);
  doc.version_json = notification.version_json;

  switch (doc.type) {
  case LSP_Documents::Document_Type::config: {
    Monotonic_Allocator temporary_allocator(
        "Linting_LSP_Server_Handler::handle_text_document_did_change_"
        "notification");
    Span<Configuration_Change> config_changes =
        this->config_loader_.refresh(&temporary_allocator);
    this->handle_config_file_changes(documents, config_changes);
    break;
  }

  case LSP_Documents::Document_Type::lintable:
    this->linter_.lint(derived_cast<LSP_Documents::Lintable_Document&>(doc),
                       notification.uri.json, this->outgoing_messages_);
    break;

  case LSP_Documents::Document_Type::unknown:
    break;
  }
}

void Linting_LSP_Server_Handler::handle_text_document_did_close_notification(
    ::simdjson::ondemand::object& request) {
  String8_View uri;
  if (!get_string8(request, "params", "textDocument", "uri", &uri)) {
    // Ignore invalid notification.
    return;
  }
  this->handle_text_document_did_close_notification(
      LSP_Text_Document_Did_Close_Notification{
          .uri = uri,
      });

  debug_probe_publish_lsp_documents();
}

void Linting_LSP_Server_Handler::handle_text_document_did_close_notification(
    const LSP_Text_Document_Did_Close_Notification& notification) {
  std::string path = parse_file_from_lsp_uri(notification.uri);
  if (path.empty()) {
    // TODO(strager): Report a warning.
    QLJS_UNIMPLEMENTED();
  }

  this->config_loader_.unwatch_file(path);
  this->config_fs_.close_document(path);
  this->documents_.lock()->documents.erase(String8(notification.uri));
  // TODO(strager): Signal to configuration_loader and
  // change_detecting_filesystem_* that we no longer need to track changes to
  // this .js document's config file.

  this->filesystem_changed();
}

void Linting_LSP_Server_Handler::handle_text_document_did_open_notification(
    ::simdjson::ondemand::object& request) {
  ::simdjson::ondemand::object text_document;
  if (!get_object(request, "params", "textDocument", &text_document)) {
    // Ignore invalid notification.
    return;
  }
  std::string_view language_id;
  if (!get_string(text_document, "languageId", &language_id)) {
    // Ignore invalid notification.
    return;
  }
  std::optional<String_JSON_Token> uri =
      maybe_get_string_token(text_document["uri"]);
  if (!uri.has_value()) {
    // Ignore invalid notification.
    return;
  }
  ::simdjson::ondemand::value version;
  if (!get_value(text_document, "version", &version)) {
    // Ignore invalid notification.
    return;
  }
  String8_View text;
  if (!get_string8(text_document, "text", &text)) {
    // Ignore invalid notification.
    return;
  }

  this->handle_text_document_did_open_notification(
      LSP_Text_Document_Did_Open_Notification{
          .language_id = language_id,
          .uri = *uri,
          .version_json = get_raw_json(version),
          .text = text,
      });

  debug_probe_publish_lsp_documents();
}

void Linting_LSP_Server_Handler::handle_text_document_did_open_notification(
    const LSP_Text_Document_Did_Open_Notification& notification) {
  std::string document_path = parse_file_from_lsp_uri(notification.uri.data);
  if (document_path.empty()) {
    // TODO(strager): Report a warning and use a default configuration.
    QLJS_UNIMPLEMENTED();
  }

  auto init_document = [&](LSP_Documents::Document_Base& doc) {
    this->config_fs_.open_document(document_path, &doc.doc);

    doc.doc.set_text(notification.text);
    doc.language_id = notification.language_id;
    doc.version_json = notification.version_json;
  };

  std::unique_ptr<LSP_Documents::Document_Base> doc_ptr;
  if (const LSP_Language* lang =
          LSP_Language::find(notification.language_id, notification.uri.data)) {
    auto doc = std::make_unique<LSP_Documents::Lintable_Document>();
    init_document(*doc);
    doc->lint_options = lang->lint_options;

    auto config_file =
        this->config_loader_.watch_and_load_for_file(document_path,
                                                     /*token=*/doc.get());
    if (config_file.ok()) {
      if (*config_file) {
        doc->config = &(*config_file)->config;
        if (!(*config_file)->errors.empty()) {
          Byte_Buffer& message_json = this->outgoing_messages_.new_message();
          this->write_configuration_errors_notification(
              document_path, *config_file, message_json);
        }
      } else {
        doc->config = &this->default_config_;
      }
    } else {
      doc->config = &this->default_config_;
      Byte_Buffer& message_json = this->outgoing_messages_.new_message();
      this->write_configuration_loader_error_notification(
          document_path, config_file.error_to_string(), message_json);
    }
    this->linter_.lint(*doc, notification.uri.json, this->outgoing_messages_);

    doc_ptr = std::move(doc);
  } else if (this->config_loader_.is_config_file_path(document_path)) {
    auto doc = std::make_unique<LSP_Documents::Config_Document>();
    init_document(*doc);

    auto config_file =
        this->config_loader_.watch_and_load_config_file(document_path,
                                                        /*token=*/doc.get());
    QLJS_ASSERT(config_file.ok());
    Byte_Buffer& config_diagnostics_json =
        this->outgoing_messages_.new_message();
    this->get_config_file_diagnostics_notification(
        *config_file, notification.uri.json, doc->version_json,
        config_diagnostics_json);

    Monotonic_Allocator temporary_allocator(
        "Linting_LSP_Server_Handler::handle_text_document_did_open_"
        "notification");
    Span<Configuration_Change> config_changes =
        this->config_loader_.refresh(&temporary_allocator);
    {
      Lock_Ptr<LSP_Documents> documents = this->documents_.lock();
      this->handle_config_file_changes(documents, config_changes);
    }

    doc_ptr = std::move(doc);
  } else {
    doc_ptr = std::make_unique<LSP_Documents::Unknown_Document>();
    init_document(*doc_ptr);
  }

  // If the document already exists, deallocate that document_base and use ours.
  // TODO(strager): Should we report a warning if a document already existed?
  this->documents_.lock()->documents[String8(notification.uri.data)] =
      std::move(doc_ptr);
}

void Linting_LSP_Server_Handler::
    handle_workspace_did_change_configuration_notification(
        ::simdjson::ondemand::object& request) {
  ::simdjson::ondemand::object settings;
  if (!get_object(request, "params", "settings", &settings)) {
    QLJS_DEBUG_LOG("failed to extract configuration notification settings\n");
    // TODO(strager): Report an error.
    return;
  }

  bool ok = this->workspace_configuration_.process_notification(settings);
  if (!ok) {
    QLJS_DEBUG_LOG("failed to process configuration notification\n");
    // TODO(strager): Report an error.
    return;
  }
}

void Linting_LSP_Server_Handler::handle_config_file_changes(
    Lock_Ptr<LSP_Documents>& documents,
    Span<const Configuration_Change> config_changes) {
  for (auto& entry : documents->documents) {
    const String8& document_uri = entry.first;
    LSP_Documents::Document_Base& doc = *entry.second;

    auto change_it =
        find_unique_if(config_changes, [&](const Configuration_Change& change) {
          return change.token == &doc;
        });
    if (change_it == config_changes.end()) {
      continue;
    }

    doc.on_config_file_changed(*this, document_uri, *change_it);
  }
}

void LSP_Documents::Config_Document::on_config_file_changed(
    Linting_LSP_Server_Handler& handler, String8_View document_uri,
    const Configuration_Change& change) {
  QLJS_ASSERT(change.config_file);
  if (change.config_file) {
    Byte_Buffer& config_diagnostics_json =
        handler.outgoing_messages_.new_message();
    handler.get_config_file_diagnostics_notification(
        change.config_file, to_json_escaped_string_with_quotes(document_uri),
        this->version_json, config_diagnostics_json);
  }
}

void LSP_Documents::Lintable_Document::on_config_file_changed(
    Linting_LSP_Server_Handler& handler, String8_View document_uri,
    const Configuration_Change& change) {
  std::string document_path = parse_file_from_lsp_uri(document_uri);
  if (document_path.empty()) {
    // TODO(strager): Report a warning and use a default configuration.
    QLJS_UNIMPLEMENTED();
  }
  if (change.error != nullptr) {
    Byte_Buffer& message_json = handler.outgoing_messages_.new_message();
    handler.write_configuration_loader_error_notification(
        document_path, change.error->to_string(), message_json);
  }
  Configuration* config = change.config_file ? &change.config_file->config
                                             : &handler.default_config_;
  this->config = config;
  // TODO(strager): Don't copy document_uri if it contains only non-special
  // characters.
  // TODO(strager): Cache the result of to_json_escaped_string?
  handler.linter_.lint(*this, to_json_escaped_string_with_quotes(document_uri),
                       handler.outgoing_messages_);
}

void LSP_Documents::Unknown_Document::on_config_file_changed(
    Linting_LSP_Server_Handler&, String8_View, const Configuration_Change&) {
  // Do nothing.
}

void Linting_LSP_Server_Handler::get_config_file_diagnostics_notification(
    Loaded_Config_File* config_file, String8_View uri_json,
    String8_View version_json, Byte_Buffer& notification_json) {
  // clang-format off
  notification_json.append_copy(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--"_sv);
  // clang-format on
  notification_json.append_copy(uri_json);

  notification_json.append_copy(u8R"--(,"version":)--"_sv);
  notification_json.append_copy(version_json);

  notification_json.append_copy(u8R"--(,"diagnostics":)--"_sv);
  LSP_Diag_Reporter diag_reporter(qljs_messages, notification_json,
                                  &config_file->file_content);
  diag_reporter.report(config_file->errors);
  diag_reporter.finish();

  notification_json.append_copy(u8R"--(},"jsonrpc":"2.0"})--"_sv);
}

void Linting_LSP_Server_Handler::write_configuration_loader_error_notification(
    std::string_view document_path, std::string_view error_details,
    Byte_Buffer& out_json) {
  // clang-format off
  out_json.append_copy(u8R"--({)--"
    u8R"--("jsonrpc":"2.0",)--"
    u8R"--("method":"window/showMessage",)--"
    u8R"--("params":{)--"
      u8R"--("type":2,)--"
      u8R"--("message":"Failed to load configuration file for )--"_sv);
  // clang-format on
  write_json_escaped_string(out_json, to_string8_view(document_path));
  out_json.append_copy(u8". Using default configuration.\\nError details: "_sv);
  write_json_escaped_string(out_json, to_string8_view(error_details));
  out_json.append_copy(u8"\"}}"_sv);
}

void Linting_LSP_Server_Handler::write_configuration_errors_notification(
    std::string_view document_path, Loaded_Config_File* config_file,
    Byte_Buffer& out_json) {
  // clang-format off
  out_json.append_copy(u8R"--({)--"
    u8R"--("jsonrpc":"2.0",)--"
    u8R"--("method":"window/showMessage",)--"
    u8R"--("params":{)--"
      u8R"--("type":2,)--"
      u8R"--("message":"Problems found in the config file for )--"_sv);
  // clang-format on
  write_json_escaped_string(out_json, to_string8_view(document_path));
  out_json.append_copy(u8" ("_sv);
  QLJS_ASSERT(config_file->config_path);
  write_json_escaped_string(out_json,
                            to_string8_view(config_file->config_path->path()));
  out_json.append_copy(u8").\"}}"_sv);
}

void Linting_LSP_Server_Handler::apply_document_changes(
    LSP_Document_Text& doc, ::simdjson::ondemand::array& changes) {
  for (::simdjson::simdjson_result<::simdjson::ondemand::value> change :
       changes) {
    ::simdjson::ondemand::object change_object;
    if (change.get(change_object) != ::simdjson::SUCCESS) {
      // Ignore invalid change.
      continue;
    }
    apply_document_change(doc, change_object);
  }
}

void Linting_LSP_Server_Handler::apply_document_change(
    LSP_Document_Text& doc, ::simdjson::ondemand::object& raw_change) {
  LSP_Document_Change change;
  if (!get_string8(raw_change, "text", &change.text)) {
    // Ignore invalid change.
    return;
  }
  ::simdjson::ondemand::object raw_range;
  if (get_object(raw_change, "range", &raw_range)) {
    LSP_Range& range = change.range.emplace();

    ::simdjson::ondemand::object start;
    if (!(get_object(raw_range, "start", &start) &&
          get_int(start, "line", &range.start.line) &&
          get_int(start, "character", &range.start.character))) {
      // Ignore invalid change.
      return;
    }

    ::simdjson::ondemand::object end;
    if (!(get_object(raw_range, "end", &end) &&
          get_int(end, "line", &range.end.line) &&
          get_int(end, "character", &range.end.character))) {
      // Ignore invalid change.
      return;
    }
  }

  apply_document_change(doc, change);
}

void Linting_LSP_Server_Handler::apply_document_change(
    LSP_Document_Text& doc, const LSP_Document_Change& change) {
  if (change.range.has_value()) {
    doc.replace_text(*change.range, change.text);
  } else {
    doc.set_text(change.text);
  }
}

void Linting_LSP_Server_Handler::write_method_not_found_error_response(
    String8_View request_id_json) {
  Byte_Buffer& response_json = this->outgoing_messages_.new_message();
  // clang-format off
  response_json.append_copy(u8R"({)"
    u8R"("jsonrpc":"2.0",)"
    u8R"("id":)"_sv);
  response_json.append_copy(request_id_json);
  response_json.append_copy(u8R"(,)"
    u8R"("error":{)"
      u8R"("code":-32601,)"
      u8R"("message":"Method not found")"
    u8R"(})"
  u8R"(})"_sv);
  // clang-format on
}

LSP_Linter::~LSP_Linter() = default;

void LSP_Linter::lint(LSP_Documents::Lintable_Document& doc,
                      String8_View uri_json,
                      Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
  this->lint(*doc.config, doc.lint_options, doc.doc.string(), uri_json,
             doc.version_json, outgoing_messages);
}

void LSP_JavaScript_Linter::lint(
    Configuration& config, Linter_Options lint_options, Padded_String_View code,
    String8_View uri_json, String8_View version_json,
    Outgoing_JSON_RPC_Message_Queue& outgoing_messages) {
  Byte_Buffer& notification_json = outgoing_messages.new_message();
  // clang-format off
  notification_json.append_copy(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--"_sv);
  // clang-format on
  notification_json.append_copy(uri_json);

  notification_json.append_copy(u8R"--(,"version":)--"_sv);
  notification_json.append_copy(version_json);

  notification_json.append_copy(u8R"--(,"diagnostics":)--"_sv);
  this->lint_and_get_diagnostics(config, lint_options, code, notification_json);

  notification_json.append_copy(u8R"--(},"jsonrpc":"2.0"})--"_sv);
}

void LSP_JavaScript_Linter::lint_and_get_diagnostics(
    Configuration& config, Linter_Options lint_options, Padded_String_View code,
    Byte_Buffer& diagnostics_json) {
  LSP_Diag_Reporter diag_reporter(qljs_messages, diagnostics_json, code);
  parse_and_lint(code, diag_reporter, config.globals(), lint_options);
  diag_reporter.finish();
}

Synchronized<LSP_Documents>* get_lsp_server_documents() {
  return latest_lsp_server_documents.load();
}

void set_lsp_server_documents(Synchronized<LSP_Documents>* documents) {
  latest_lsp_server_documents.store(documents);
}

namespace {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
std::optional<String_JSON_Token> maybe_get_string_token(
    ::simdjson::ondemand::value& string) {
  std::string_view s;
  if (string.get(s) != ::simdjson::SUCCESS) {
    return std::nullopt;
  }
  String8_View data(reinterpret_cast<const Char8*>(s.data()), s.size());
  return String_JSON_Token{
      .data = data,
      .json = to_string8_view(string.raw_json_token()),
  };
}
QLJS_WARNING_POP

std::optional<String_JSON_Token> maybe_get_string_token(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& string) {
  ::simdjson::ondemand::value s;
  if (string.get(s) != ::simdjson::SUCCESS) {
    return std::nullopt;
  }
  return maybe_get_string_token(s);
}
}
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
