// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <memory>
#include <napi.h>
#include <optional>
#include <quick-lint-js/configuration/basic-configuration-filesystem.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/logger.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread-name.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/vscode/addon.h>
#include <quick-lint-js/vscode/napi-support.h>
#include <quick-lint-js/vscode/qljs-document.h>
#include <quick-lint-js/vscode/qljs-logger.h>
#include <quick-lint-js/vscode/qljs-workspace.h>
#include <quick-lint-js/vscode/thread-safe-js-function.h>
#include <quick-lint-js/vscode/vscode-configuration-filesystem.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode-language.h>
#include <quick-lint-js/vscode/vscode-tracer.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
namespace {
class Extension_Configuration {
 public:
  enum class Logging_Value {
    off,  // default
    verbose,
  };

  enum class Tracing_Value {
    off,  // default
    verbose,
  };

  explicit Extension_Configuration(::Napi::Env env, VSCode_Module& vscode)
      : config_ref_(::Napi::Persistent(
            vscode.get_configuration(env, "quick-lint-js"))) {}

  Logging_Value get_logging(::Napi::Env env) {
    ::Napi::Value value = this->get(env, "logging");
    if (!value.IsString()) {
      return Logging_Value::off;
    }
    std::string string_value = value.As<::Napi::String>().Utf8Value();
    if (string_value == "verbose") {
      return Logging_Value::verbose;
    } else {
      return Logging_Value::off;
    }
  }

  Tracing_Value get_tracing(::Napi::Env env) {
    ::Napi::Value value = this->get(env, "tracing");
    if (!value.IsString()) {
      return Tracing_Value::off;
    }
    std::string string_value = value.As<::Napi::String>().Utf8Value();
    if (string_value == "verbose") {
      return Tracing_Value::verbose;
    } else {
      return Tracing_Value::off;
    }
  }

  ::Napi::Value get(::Napi::Env env, const char* section) {
    return this->config_ref_.Get("get").As<::Napi::Function>().Call(
        this->config_ref_.Value(), {::Napi::String::New(env, section)});
  }

 private:
  ::Napi::ObjectReference config_ref_;
};
}

::Napi::Function QLJS_Workspace::init(::Napi::Env env) {
  return DefineClass(
      env, "QLJSWorkspace",
      {
          InstanceMethod<&QLJS_Workspace::close_document>("closeDocument"),
          InstanceMethod<&QLJS_Workspace::configuration_changed>(
              "configurationChanged"),
          InstanceMethod<&QLJS_Workspace::dispose>("dispose"),
          InstanceMethod<&QLJS_Workspace::document_changed>("documentChanged"),
          InstanceMethod<&QLJS_Workspace::document_saved>("documentSaved"),
          InstanceMethod<&QLJS_Workspace::editor_visibility_changed>(
              "editorVisibilityChanged"),
      });
}

QLJS_Workspace::QLJS_Workspace(const Napi::CallbackInfo& info)
    : ::Napi::ObjectWrap<QLJS_Workspace>(info),
      tracer_(info[0].As<::Napi::String>().Utf8Value()),
      vscode_(info[1].As<::Napi::Object>()),
      check_for_config_file_changes_on_js_thread_(
          /*env=*/info.Env(),
          /*resource_name=*/"quick-lint-js-fs-thread",
          /*object=*/this->Value()),
      qljs_documents_(info.Env()),
      vscode_diagnostic_collection_ref_(
          ::Napi::Persistent(info[2].As<::Napi::Object>())),
      ui_(this) {
  QLJS_DEBUG_LOG("Workspace %p: created\n", this);
  this->update_logging(info.Env());
  this->fs_change_detection_thread_ =
      Thread([this]() -> void { this->run_fs_change_detection_thread(); });
}

void QLJS_Workspace::update_logging(::Napi::Env env) {
  Extension_Configuration config(env, this->vscode_);
  switch (config.get_logging(env)) {
  case Extension_Configuration::Logging_Value::off:
    this->disable_logging();
    break;
  case Extension_Configuration::Logging_Value::verbose:
    this->enable_logging(env);
    break;
  }
  switch (config.get_tracing(env)) {
  case Extension_Configuration::Tracing_Value::off:
    this->tracer_.disable();
    break;
  case Extension_Configuration::Tracing_Value::verbose:
    this->tracer_.enable();
    break;
  }
}

void QLJS_Workspace::enable_logging(::Napi::Env env) {
  if (this->logger_enabled_) {
    return;
  }
  if (this->logger_.IsEmpty()) {
    Addon_State* state = env.GetInstanceData<Addon_State>();
    this->logger_ = ::Napi::Persistent(state->qljs_logger_class.New(
        {this->vscode_.create_output_channel(env, "quick-lint-js")}));
  }
  enable_logger(QLJS_Logger::Unwrap(this->logger_.Value()));
  this->logger_enabled_ = true;
  QLJS_DEBUG_LOG("Configured VS Code logger\n");
}

void QLJS_Workspace::disable_logging() {
  if (!this->logger_enabled_) {
    return;
  }
  QLJS_DEBUG_LOG("Disabling VS Code logger\n");
  QLJS_ASSERT(!this->logger_.IsEmpty());
  disable_logger(QLJS_Logger::Unwrap(this->logger_.Value()));
  this->logger_enabled_ = false;
}

QLJS_Workspace::~QLJS_Workspace() {
  // See NOTE[workspace-cleanup].
  this->dispose();
}

::Napi::Value QLJS_Workspace::dispose(const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();
  this->dispose();
  return env.Undefined();
}

void QLJS_Workspace::dispose() {
  if (this->disposed_) {
    return;
  }

  QLJS_DEBUG_LOG("Workspace %p: disposing\n", this);
  this->fs_change_detection_event_loop_.stop();
  this->fs_change_detection_thread_.join();
  this->dispose_documents();

  this->disable_logging();

  this->disposed_ = true;
}

void QLJS_Workspace::dispose_documents() {
  this->qljs_documents_.for_each([this](::Napi::Value value) -> void {
    QLJS_Document_Base* doc = QLJS_Document_Base::unwrap(value);
    this->delete_diagnostics(doc);
  });
  this->qljs_documents_.clear();

  this->config_loader_.unwatch_all_files();

  this->fs_.clear();
}

::Napi::Value QLJS_Workspace::close_document(const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  VSCode_Document vscode_doc(info[0].As<::Napi::Object>());
  ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_doc.get());
  QLJS_Document_Base* doc =
      qljs_doc.IsUndefined() ? nullptr : QLJS_Document_Base::unwrap(qljs_doc);
  this->tracer_.trace_vscode_document_closed(env, vscode_doc, doc);
  if (doc) {
    QLJS_DEBUG_LOG("Document %p: Closing\n", doc);
    this->delete_diagnostics(doc);
    this->qljs_documents_.erase(vscode_doc.get());
    this->fs_.forget_document(doc);
  }

  return env.Undefined();
}

::Napi::Value QLJS_Workspace::configuration_changed(
    const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  this->update_logging(env);

  return env.Undefined();
}

::Napi::Value QLJS_Workspace::editor_visibility_changed(
    const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  ::Napi::Object vscode_doc = info[0].As<::Napi::Object>();
  ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_doc);
  QLJS_Document_Base* doc;
  if (qljs_doc.IsUndefined()) {
    VSCode_Document d(vscode_doc);
    doc = this->maybe_create_document(
        env,
        /*vscode_doc=*/d,
        /*text=*/to_string8_view(d.get_text().Utf8Value()));
    if (doc) {
      doc->after_modification(env, *this, this->diagnostic_collection());
    }
  } else {
    doc = QLJS_Document_Base::unwrap(qljs_doc);
    doc->after_modification(env, *this, this->diagnostic_collection());
  }

  return env.Undefined();
}

::Napi::Value QLJS_Workspace::document_changed(const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  ::Napi::Value vscode_document = info[0];
  ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_document);
  if (!qljs_doc.IsUndefined()) {
    QLJS_Document_Base* doc = QLJS_Document_Base::unwrap(qljs_doc);
    ::Napi::Array changes = info[1].As<::Napi::Array>();
    this->tracer_.trace_vscode_document_changed(env, doc, changes);
    doc->replace_text(changes);
    doc->after_modification(env, *this, this->diagnostic_collection());
  }

  return env.Undefined();
}

::Napi::Value QLJS_Workspace::document_saved(const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  ::Napi::Object vscode_doc = info[0].As<::Napi::Object>();
  ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_doc);
  if (!qljs_doc.IsUndefined()) {
    QLJS_Document_Base* doc = QLJS_Document_Base::unwrap(qljs_doc);
    QLJS_DEBUG_LOG("Document %p: Saved\n", doc);
    this->tracer_.trace_vscode_document_sync(env, VSCode_Document(vscode_doc),
                                             doc);
  }

  return env.Undefined();
}

QLJS_Document_Base* QLJS_Workspace::maybe_create_document(
    ::Napi::Env env, VSCode_Document vscode_doc, String8_View text) {
  ::Napi::Object vscode_document_uri = vscode_doc.uri();
  std::optional<std::string> file_path = std::nullopt;
  if (to_string(vscode_document_uri.Get("scheme")) == "file") {
    file_path = to_string(vscode_document_uri.Get("fsPath"));
  }

  QLJS_Document_Base* doc;
  if (const VSCode_Language* lang = VSCode_Language::find(
          vscode_doc.language_id(), to_string8_view(vscode_doc.uri_string()),
          /*allow_typescript=*/true)) {
    doc = new QLJS_Lintable_Document(vscode_doc, file_path, lang->lint_options);
  } else if (file_path.has_value() &&
             this->config_loader_.is_config_file_path(*file_path)) {
    doc = new QLJS_Config_Document(vscode_doc, file_path);
  } else {
    return nullptr;
  }
  ::Napi::Value js_doc = doc->create_wrapper(env);

  doc->document_.set_text(text);
  if (file_path.has_value()) {
    this->fs_.overlay_document(*file_path, doc);
  }
  this->qljs_documents_.set(vscode_doc, js_doc);

  this->tracer_.trace_vscode_document_opened(env, vscode_doc, doc);

  doc->finish_init(env, *this, file_path);

  this->report_pending_watch_io_errors(env);

  return doc;
}

VSCode_Diagnostic_Collection QLJS_Workspace::diagnostic_collection() const {
  return VSCode_Diagnostic_Collection(
      this->vscode_diagnostic_collection_ref_.Value());
}

void QLJS_Workspace::delete_diagnostics(QLJS_Document_Base* doc) {
  VSCode_Diagnostic_Collection(this->vscode_diagnostic_collection_ref_.Value())
      .delete_(doc->uri());
}

void QLJS_Workspace::report_pending_watch_io_errors(::Napi::Env env) {
  std::vector<Watch_IO_Error> errors =
      this->fs_change_detection_event_loop_.fs()->take_watch_errors();
  if (!errors.empty() && !this->did_report_watch_io_error_) {
    this->ui_.show_watch_io_errors(env, Span<const Watch_IO_Error>(errors));
    this->did_report_watch_io_error_ = true;
  }
}

void QLJS_Workspace::check_for_config_file_changes_from_thread(
    ::Napi::Env env, ::Napi::Object workspace_object) {
  QLJS_Workspace* workspace = QLJS_Workspace::Unwrap(workspace_object);
  if (workspace->disposed_) {
    QLJS_DEBUG_LOG(
        "Workspace %p: check_for_config_file_changes_from_thread: workspace "
        "object has been disposed\n",
        workspace);
    return;
  }
  workspace->check_for_config_file_changes(env);
}

void QLJS_Workspace::check_for_config_file_changes(::Napi::Env env) {
  Monotonic_Allocator temporary_allocator(
      "QLJS_Workspace::check_for_config_file_changes");
  Span<Configuration_Change> changes =
      this->config_loader_.refresh(&temporary_allocator);
  for (const Configuration_Change& change : changes) {
    QLJS_DEBUG_LOG("Configuration changed for %s\n",
                   change.watched_path->c_str());
    QLJS_Document_Base* doc =
        reinterpret_cast<QLJS_Document_Base*>(change.token);
    doc->on_config_file_changed(env, *this, this->diagnostic_collection(),
                                change.config_file);
  }
}

void QLJS_Workspace::run_fs_change_detection_thread() {
  QLJS_DEBUG_LOG("Workspace %p: starting run_fs_change_detection_thread\n",
                 this);
  if constexpr (max_thread_name_length < 16) {
    set_current_thread_name(u8"qljs-fs-change");
  } else {
    set_current_thread_name(u8"quick-lint-js fs_change_detection");
  }
  this->fs_change_detection_event_loop_.run();
  this->check_for_config_file_changes_on_js_thread_.Release();
  QLJS_DEBUG_LOG("Workspace %p: stopping run_fs_change_detection_thread\n",
                 this);
}

::Napi::Object create_workspace(const ::Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();
  Addon_State* state = env.GetInstanceData<Addon_State>();

  ::Napi::Object options = info[0].As<::Napi::Object>();
  return state->qljs_workspace_class.New({
      /*log_directory=*/options.Get("logDirectory"),
      /*vscode=*/options.Get("vscode"),
      /*vscode_diagnostic_collection=*/
      options.Get("vscodeDiagnosticCollection"),
  });
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
