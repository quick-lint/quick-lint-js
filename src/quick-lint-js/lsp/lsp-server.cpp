// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/lsp/lsp-diag-reporter.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/lsp/lsp-uri.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/simdjson.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/version.h>
#include <simdjson.h>
#include <string>
#include <utility>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
constexpr lsp_endpoint_handler::request_id_type
    initial_configuration_request_id = 1;

struct string_json_token {
  string8_view data;
  string8_view json;
};

// Returns std::nullopt on failure (e.g. missing key or not a string).
std::optional<string_json_token> maybe_get_string_token(
    ::simdjson::ondemand::value& string);
std::optional<string_json_token> maybe_get_string_token(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& string);
}

lsp_overlay_configuration_filesystem::lsp_overlay_configuration_filesystem(
    configuration_filesystem* underlying_fs)
    : underlying_fs_(underlying_fs) {}

result<canonical_path_result, canonicalize_path_io_error>
lsp_overlay_configuration_filesystem::canonicalize_path(
    const std::string& path) {
  return this->underlying_fs_->canonicalize_path(path);
}

linting_lsp_server_handler::~linting_lsp_server_handler() {
  // We are going to deallocate this->tracer_backend_, so unregister it with
  // the trace_flusher.
  if (this->tracer_backend_) {
    trace_flusher::instance()->disable_backend(this->tracer_backend_.get());
  }
}

result<padded_string, read_file_io_error>
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
  std::size_t erased = this->overlaid_documents_.erase(path);
  QLJS_ASSERT(erased > 0);
}

linting_lsp_server_handler::linting_lsp_server_handler(
    configuration_filesystem* fs, lsp_linter* linter)
    : config_fs_(fs), config_loader_(&this->config_fs_), linter_(*linter) {
  this->workspace_configuration_.add_item(
      u8"quick-lint-js.tracing-directory"sv,
      [this](std::string_view new_value) {
        bool changed = this->server_config_.tracing_directory != new_value;
        if (changed) {
          this->server_config_.tracing_directory = new_value;
          if (this->tracer_backend_) {
            trace_flusher::instance()->disable_backend(
                this->tracer_backend_.get());
            this->tracer_backend_.reset();
          }
          if (!this->server_config_.tracing_directory.empty()) {
            auto new_backend =
                trace_flusher_directory_backend::create_child_directory(
                    this->server_config_.tracing_directory);
            if (new_backend) {
              this->tracer_backend_ =
                  std::make_unique<trace_flusher_directory_backend>(
                      std::move(*new_backend));
              trace_flusher::instance()->enable_backend(
                  this->tracer_backend_.get());
              QLJS_DEBUG_LOG("enabled tracing in directory %s\n",
                             this->tracer_backend_->trace_directory().c_str());
            }
          }
        }
      });
}

void linting_lsp_server_handler::handle_request(
    ::simdjson::ondemand::object& request, std::string_view method,
    string8_view id_json, byte_buffer& response_json) {
  if (method == "initialize") {
    this->handle_initialize_request(request, id_json, response_json);
  } else if (method == "shutdown") {
    this->handle_shutdown_request(request, id_json, response_json);
  } else {
    this->write_method_not_found_error_response(id_json, response_json);
  }
}

void linting_lsp_server_handler::handle_response(
    lsp_endpoint_handler::request_id_type request_id,
    ::simdjson::ondemand::value& result) {
  if (request_id == initial_configuration_request_id) {
    this->handle_workspace_configuration_response(result);
  } else {
    QLJS_DEBUG_LOG("received response for unknown request\n");
    // TODO(strager): Report an error.
  }
}

void linting_lsp_server_handler::handle_error_response(
    lsp_endpoint_handler::request_id_type request_id, std::int64_t code,
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

void linting_lsp_server_handler::handle_notification(
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

void linting_lsp_server_handler::filesystem_changed() {
  std::vector<configuration_change> config_changes =
      this->config_loader_.refresh();
  this->handle_config_file_changes(config_changes);
}

void linting_lsp_server_handler::add_watch_io_errors(
    const std::vector<watch_io_error>& errors) {
  if (!errors.empty() && !this->did_report_watch_io_error_) {
    byte_buffer& out_json = this->pending_notification_jsons_.emplace_back();
    // clang-format off
    out_json.append_copy(u8R"--({)--"
      u8R"--("jsonrpc":"2.0",)--"
      u8R"--("method":"window/showMessage",)--"
      u8R"--("params":{)--"
        u8R"--("type":2,)--"
        u8R"--("message":")--"sv);
    // clang-format on
    write_json_escaped_string(out_json, to_string8_view(errors[0].to_string()));
    out_json.append_copy(u8"\"}}"sv);
    this->did_report_watch_io_error_ = true;
  }
}

void linting_lsp_server_handler::handle_initialize_request(
    ::simdjson::ondemand::object&, string8_view id_json,
    byte_buffer& response_json) {
  response_json.append_copy(u8R"--({"id":)--"sv);
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
    u8R"--("jsonrpc":"2.0"})--"sv);
  // clang-format on
}

void linting_lsp_server_handler::handle_shutdown_request(
    ::simdjson::ondemand::object&, string8_view id_json,
    byte_buffer& response_json) {
  this->shutdown_requested_ = true;
  response_json.append_copy(u8R"--({"jsonrpc":"2.0","id":)--"sv);
  response_json.append_copy(id_json);
  response_json.append_copy(u8R"--(,"result":null})--"sv);
}

void linting_lsp_server_handler::handle_workspace_configuration_response(
    ::simdjson::ondemand::value& result) {
  bool ok = this->workspace_configuration_.process_response(result);
  if (!ok) {
    QLJS_DEBUG_LOG("failed to process configuration response\n");
    // TODO(strager): Report an error.
    return;
  }
}

void linting_lsp_server_handler::handle_initialized_notification() {
  byte_buffer& request_json = this->pending_notification_jsons_.emplace_back();
  this->workspace_configuration_.build_request(initial_configuration_request_id,
                                               request_json);
}

void linting_lsp_server_handler::handle_text_document_did_change_notification(
    ::simdjson::ondemand::object& request) {
  ::simdjson::ondemand::object text_document;
  if (!get_object(request, "params", "textDocument", &text_document)) {
    // Ignore invalid notification.
    return;
  }
  std::optional<string_json_token> uri =
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

  auto document_it = this->documents_.find(string8(uri->data));
  bool url_is_tracked = document_it != this->documents_.end();
  if (!url_is_tracked) {
    return;
  }
  document_base& doc = *document_it->second;

  std::string document_path = parse_file_from_lsp_uri(uri->data);
  if (document_path.empty()) {
    // TODO(strager): Report a warning and use a default configuration.
    QLJS_UNIMPLEMENTED();
  }

  ::simdjson::ondemand::array changes;
  if (!get_array(request, "params", "contentChanges", &changes)) {
    // Ignore invalid notification.
    return;
  }
  this->apply_document_changes(doc.doc, changes);
  doc.version_json = get_raw_json(version);

  doc.on_text_changed(*this, uri->json);
}

void linting_lsp_server_handler::config_document::on_text_changed(
    linting_lsp_server_handler& handler, string8_view) {
  std::vector<configuration_change> config_changes =
      handler.config_loader_.refresh();
  handler.handle_config_file_changes(config_changes);
}

void linting_lsp_server_handler::lintable_document::on_text_changed(
    linting_lsp_server_handler& handler, string8_view document_uri_json) {
  byte_buffer& notification_json =
      handler.pending_notification_jsons_.emplace_back();
  handler.linter_.lint_and_get_diagnostics_notification(
      *this->config, this->doc.string(), document_uri_json, this->version_json,
      notification_json);
}

void linting_lsp_server_handler::unknown_document::on_text_changed(
    linting_lsp_server_handler&, string8_view) {
  // Do nothing.
}

void linting_lsp_server_handler::handle_text_document_did_close_notification(
    ::simdjson::ondemand::object& request) {
  string8_view uri;
  if (!get_string8(request, "params", "textDocument", "uri", &uri)) {
    // Ignore invalid notification.
    return;
  }
  std::string path = parse_file_from_lsp_uri(uri);
  if (path.empty()) {
    // TODO(strager): Report a warning.
    QLJS_UNIMPLEMENTED();
  }

  this->config_loader_.unwatch_file(path);
  this->config_fs_.close_document(path);
  this->documents_.erase(string8(uri));
  // TODO(strager): Signal to configuration_loader and
  // change_detecting_filesystem_* that we no longer need to track changes to
  // this .js document's config file.

  this->filesystem_changed();
}

void linting_lsp_server_handler::handle_text_document_did_open_notification(
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
  std::optional<string_json_token> uri =
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
  string8_view text;
  if (!get_string8(text_document, "text", &text)) {
    // Ignore invalid notification.
    return;
  }

  std::string document_path = parse_file_from_lsp_uri(uri->data);
  if (document_path.empty()) {
    // TODO(strager): Report a warning and use a default configuration.
    QLJS_UNIMPLEMENTED();
  }

  auto init_document = [&](document_base& doc) {
    this->config_fs_.open_document(document_path, &doc.doc);

    doc.doc.set_text(text);
    doc.version_json = get_raw_json(version);
  };

  std::unique_ptr<document_base> doc_ptr;
  if (language_id == "javascript" || language_id == "javascriptreact" ||
      language_id == "js" || language_id == "js-jsx") {
    auto doc = std::make_unique<lintable_document>();
    init_document(*doc);
    doc->type = document_type::lintable;

    auto config_file =
        this->config_loader_.watch_and_load_for_file(document_path,
                                                     /*token=*/doc.get());
    if (config_file.ok()) {
      if (*config_file) {
        doc->config = &(*config_file)->config;
        if (!(*config_file)->errors.empty()) {
          byte_buffer& message_json =
              this->pending_notification_jsons_.emplace_back();
          this->write_configuration_errors_notification(
              document_path, *config_file, message_json);
        }
      } else {
        doc->config = &this->default_config_;
      }
    } else {
      doc->config = &this->default_config_;
      byte_buffer& message_json =
          this->pending_notification_jsons_.emplace_back();
      this->write_configuration_loader_error_notification(
          document_path, config_file.error_to_string(), message_json);
    }
    byte_buffer& notification_json =
        this->pending_notification_jsons_.emplace_back();
    this->linter_.lint_and_get_diagnostics_notification(
        *doc->config, doc->doc.string(), uri->json, doc->version_json,
        notification_json);

    doc_ptr = std::move(doc);
  } else if (this->config_loader_.is_config_file_path(document_path)) {
    auto doc = std::make_unique<config_document>();
    init_document(*doc);
    doc->type = document_type::config;

    auto config_file =
        this->config_loader_.watch_and_load_config_file(document_path,
                                                        /*token=*/doc.get());
    QLJS_ASSERT(config_file.ok());
    byte_buffer& config_diagnostics_json =
        this->pending_notification_jsons_.emplace_back();
    this->get_config_file_diagnostics_notification(
        *config_file, uri->json, doc->version_json, config_diagnostics_json);

    std::vector<configuration_change> config_changes =
        this->config_loader_.refresh();
    this->handle_config_file_changes(config_changes);

    doc_ptr = std::move(doc);
  } else {
    doc_ptr = std::make_unique<unknown_document>();
    init_document(*doc_ptr);
  }

  // If the document already exists, deallocate that document_base and use ours.
  // TODO(strager): Should we report a warning if a document already existed?
  this->documents_[string8(uri->data)] = std::move(doc_ptr);
}

void linting_lsp_server_handler::
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

void linting_lsp_server_handler::handle_config_file_changes(
    const std::vector<configuration_change>& config_changes) {
  for (auto& entry : this->documents_) {
    const string8& document_uri = entry.first;
    document_base& doc = *entry.second;

    auto change_it =
        find_unique_if(config_changes, [&](const configuration_change& change) {
          return change.token == &doc;
        });
    if (change_it == config_changes.end()) {
      continue;
    }

    doc.on_config_file_changed(*this, document_uri, *change_it);
  }
}

void linting_lsp_server_handler::config_document::on_config_file_changed(
    linting_lsp_server_handler& handler, string8_view document_uri,
    const configuration_change& change) {
  QLJS_ASSERT(change.config_file);
  if (change.config_file) {
    byte_buffer& config_diagnostics_json =
        handler.pending_notification_jsons_.emplace_back();
    handler.get_config_file_diagnostics_notification(
        change.config_file, to_json_escaped_string_with_quotes(document_uri),
        this->version_json, config_diagnostics_json);
  }
}

void linting_lsp_server_handler::lintable_document::on_config_file_changed(
    linting_lsp_server_handler& handler, string8_view document_uri,
    const configuration_change& change) {
  std::string document_path = parse_file_from_lsp_uri(document_uri);
  if (document_path.empty()) {
    // TODO(strager): Report a warning and use a default configuration.
    QLJS_UNIMPLEMENTED();
  }
  if (change.error) {
    byte_buffer& message_json =
        handler.pending_notification_jsons_.emplace_back();
    handler.write_configuration_loader_error_notification(
        document_path, change.error->error_to_string(), message_json);
  }
  configuration* config = change.config_file ? &change.config_file->config
                                             : &handler.default_config_;
  this->config = config;
  byte_buffer& notification_json =
      handler.pending_notification_jsons_.emplace_back();
  // TODO(strager): Don't copy document_uri if it contains only non-special
  // characters.
  // TODO(strager): Cache the result of to_json_escaped_string?
  handler.linter_.lint_and_get_diagnostics_notification(
      *config, this->doc.string(),
      to_json_escaped_string_with_quotes(document_uri), this->version_json,
      notification_json);
}

void linting_lsp_server_handler::unknown_document::on_config_file_changed(
    linting_lsp_server_handler&, string8_view, const configuration_change&) {
  // Do nothing.
}

void linting_lsp_server_handler::get_config_file_diagnostics_notification(
    loaded_config_file* config_file, string8_view uri_json,
    string8_view version_json, byte_buffer& notification_json) {
  // clang-format off
  notification_json.append_copy(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--"sv);
  // clang-format on
  notification_json.append_copy(uri_json);

  notification_json.append_copy(u8R"--(,"version":)--"sv);
  notification_json.append_copy(version_json);

  notification_json.append_copy(u8R"--(,"diagnostics":)--"sv);
  lsp_diag_reporter diag_reporter(qljs_messages, notification_json,
                                  &config_file->file_content);
  config_file->errors.copy_into(&diag_reporter);
  diag_reporter.finish();

  notification_json.append_copy(u8R"--(},"jsonrpc":"2.0"})--"sv);
}

void linting_lsp_server_handler::write_configuration_loader_error_notification(
    std::string_view document_path, std::string_view error_details,
    byte_buffer& out_json) {
  // clang-format off
  out_json.append_copy(u8R"--({)--"
    u8R"--("jsonrpc":"2.0",)--"
    u8R"--("method":"window/showMessage",)--"
    u8R"--("params":{)--"
      u8R"--("type":2,)--"
      u8R"--("message":"Failed to load configuration file for )--"sv);
  // clang-format on
  write_json_escaped_string(out_json, to_string8_view(document_path));
  out_json.append_copy(u8". Using default configuration.\\nError details: "sv);
  write_json_escaped_string(out_json, to_string8_view(error_details));
  out_json.append_copy(u8"\"}}"sv);
}

void linting_lsp_server_handler::write_configuration_errors_notification(
    std::string_view document_path, loaded_config_file* config_file,
    byte_buffer& out_json) {
  // clang-format off
  out_json.append_copy(u8R"--({)--"
    u8R"--("jsonrpc":"2.0",)--"
    u8R"--("method":"window/showMessage",)--"
    u8R"--("params":{)--"
      u8R"--("type":2,)--"
      u8R"--("message":"Problems found in the config file for )--"sv);
  // clang-format on
  write_json_escaped_string(out_json, to_string8_view(document_path));
  out_json.append_copy(u8" ("sv);
  QLJS_ASSERT(config_file->config_path);
  write_json_escaped_string(out_json,
                            to_string8_view(config_file->config_path->path()));
  out_json.append_copy(u8").\"}}"sv);
}

void linting_lsp_server_handler::apply_document_changes(
    quick_lint_js::document<lsp_locator>& doc,
    ::simdjson::ondemand::array& changes) {
  for (::simdjson::simdjson_result<::simdjson::ondemand::value> change :
       changes) {
    string8_view change_text;
    if (!get_string8(change, "text", &change_text)) {
      // Ignore invalid change.
      continue;
    }
    ::simdjson::ondemand::object raw_range;
    bool is_incremental = get_object(change, "range", &raw_range);
    if (is_incremental) {
      lsp_range range;

      ::simdjson::ondemand::object start;
      if (!(get_object(raw_range, "start", &start) &&
            get_int(start, "line", &range.start.line) &&
            get_int(start, "character", &range.start.character))) {
        // Ignore invalid change.
        continue;
      }

      ::simdjson::ondemand::object end;
      if (!(get_object(raw_range, "end", &end) &&
            get_int(end, "line", &range.end.line) &&
            get_int(end, "character", &range.end.character))) {
        // Ignore invalid change.
        continue;
      }

      doc.replace_text(range, change_text);
    } else {
      doc.set_text(change_text);
    }
  }
}

void linting_lsp_server_handler::write_method_not_found_error_response(
    string8_view request_id_json, byte_buffer& response_json) {
  // clang-format off
  response_json.append_copy(u8R"({)"
    u8R"("jsonrpc":"2.0",)"
    u8R"("id":)"sv);
  response_json.append_copy(request_id_json);
  response_json.append_copy(u8R"(,)"
    u8R"("error":{)"
      u8R"("code":-32601,)"
      u8R"("message":"Method not found")"
    u8R"(})"
  u8R"(})"sv);
  // clang-format on
}

void linting_lsp_server_handler::write_invalid_request_error_response(
    byte_buffer& response_json) {
  // clang-format off
  response_json.append_copy(u8R"({)"
    u8R"("jsonrpc":"2.0",)"
    u8R"("id":null,)"
    u8R"("error":{)"
      u8R"("code":-32600,)"
      u8R"("message":"Invalid Request")"
    u8R"(})"
  u8R"(})"sv);
  // clang-format on
}

lsp_linter::~lsp_linter() = default;

void lsp_javascript_linter::lint_and_get_diagnostics_notification(
    configuration& config, padded_string_view code, string8_view uri_json,
    string8_view version_json, byte_buffer& notification_json) {
  // clang-format off
  notification_json.append_copy(
    u8R"--({)--"
      u8R"--("method":"textDocument/publishDiagnostics",)--"
      u8R"--("params":{)--"
        u8R"--("uri":)--"sv);
  // clang-format on
  notification_json.append_copy(uri_json);

  notification_json.append_copy(u8R"--(,"version":)--"sv);
  notification_json.append_copy(version_json);

  notification_json.append_copy(u8R"--(,"diagnostics":)--"sv);
  this->lint_and_get_diagnostics(config, code, notification_json);

  notification_json.append_copy(u8R"--(},"jsonrpc":"2.0"})--"sv);
}

void lsp_javascript_linter::lint_and_get_diagnostics(
    configuration& config, padded_string_view code,
    byte_buffer& diagnostics_json) {
  lsp_diag_reporter diag_reporter(qljs_messages, diagnostics_json, code);

  linter_options lint_options;
  lint_options.jsx = true;
  parse_and_lint(code, diag_reporter, config.globals(), lint_options);

  diag_reporter.finish();
}

namespace {
QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
std::optional<string_json_token> maybe_get_string_token(
    ::simdjson::ondemand::value& string) {
  std::string_view s;
  if (string.get(s) != ::simdjson::error_code::SUCCESS) {
    return std::nullopt;
  }
  string8_view data(reinterpret_cast<const char8*>(s.data()), s.size());
  return string_json_token{
      .data = data,
      .json = to_string8_view(string.raw_json_token()),
  };
}
QLJS_WARNING_POP

std::optional<string_json_token> maybe_get_string_token(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& string) {
  ::simdjson::ondemand::value s;
  if (string.get(s) != ::simdjson::error_code::SUCCESS) {
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
