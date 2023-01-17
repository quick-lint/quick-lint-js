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
#include <quick-lint-js/document.h>
#include <quick-lint-js/fe/diag-reporter.h>
#include <quick-lint-js/fe/diagnostic-formatter.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/diagnostic.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/event-loop.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/logger.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/vscode/addon.h>
#include <quick-lint-js/vscode/napi-support.h>
#include <quick-lint-js/vscode/qljs-document.h>
#include <quick-lint-js/vscode/qljs-logger.h>
#include <quick-lint-js/vscode/qljs-workspace.h>
#include <quick-lint-js/vscode/thread-safe-js-function.h>
#include <quick-lint-js/vscode/vscode-configuration-filesystem.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode-tracer.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
namespace {
class extension_configuration {
 public:
  enum class logging_value {
    off,  // default
    verbose,
  };

  enum class tracing_value {
    off,  // default
    verbose,
  };

  explicit extension_configuration(::Napi::Env env, vscode_module& vscode)
      : config_ref_(::Napi::Persistent(
            vscode.get_configuration(env, "quick-lint-js"))) {}

  logging_value get_logging(::Napi::Env env) {
    ::Napi::Value value = this->get(env, "logging");
    if (!value.IsString()) {
      return logging_value::off;
    }
    std::string string_value = value.As<::Napi::String>().Utf8Value();
    if (string_value == "verbose") {
      return logging_value::verbose;
    } else {
      return logging_value::off;
    }
  }

  tracing_value get_tracing(::Napi::Env env) {
    ::Napi::Value value = this->get(env, "tracing");
    if (!value.IsString()) {
      return tracing_value::off;
    }
    std::string string_value = value.As<::Napi::String>().Utf8Value();
    if (string_value == "verbose") {
      return tracing_value::verbose;
    } else {
      return tracing_value::off;
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

::Napi::Function qljs_workspace::init(::Napi::Env env) {
  return DefineClass(
      env, "QLJSWorkspace",
      {
          InstanceMethod<&qljs_workspace::close_document>("closeDocument"),
          InstanceMethod<&qljs_workspace::configuration_changed>(
              "configurationChanged"),
          InstanceMethod<&qljs_workspace::dispose>("dispose"),
          InstanceMethod<&qljs_workspace::document_changed>("documentChanged"),
          InstanceMethod<&qljs_workspace::document_saved>("documentSaved"),
          InstanceMethod<&qljs_workspace::editor_visibility_changed>(
              "editorVisibilityChanged"),
      });
}

qljs_workspace::qljs_workspace(const Napi::CallbackInfo& info)
    : ::Napi::ObjectWrap<qljs_workspace>(info),
      tracer_(info[0].As<::Napi::String>().Utf8Value()),
      vscode_(info[1].As<::Napi::Object>()),
      check_for_config_file_changes_on_js_thread_(
          /*env=*/info.Env(),
          /*resource_name=*/"quick-lint-js-fs-thread",
          /*object=*/this->Value()),
      qljs_documents_(info.Env()),
      vscode_diagnostic_collection_ref_(
          ::Napi::Persistent(info[2].As<::Napi::Object>())) {
  QLJS_DEBUG_LOG("Workspace %p: created\n", this);
  this->update_logging(info.Env());
  this->fs_change_detection_thread_ =
      thread([this]() -> void { this->run_fs_change_detection_thread(); });
}

void qljs_workspace::update_logging(::Napi::Env env) {
  extension_configuration config(env, this->vscode_);
  switch (config.get_logging(env)) {
  case extension_configuration::logging_value::off:
    this->disable_logging();
    break;
  case extension_configuration::logging_value::verbose:
    this->enable_logging(env);
    break;
  }
  switch (config.get_tracing(env)) {
  case extension_configuration::tracing_value::off:
    this->tracer_.disable();
    break;
  case extension_configuration::tracing_value::verbose:
    this->tracer_.enable();
    break;
  }
}

void qljs_workspace::enable_logging(::Napi::Env env) {
  if (this->logger_enabled_) {
    return;
  }
  if (this->logger_.IsEmpty()) {
    addon_state* state = env.GetInstanceData<addon_state>();
    this->logger_ = ::Napi::Persistent(state->qljs_logger_class.New(
        {this->vscode_.create_output_channel(env, "quick-lint-js")}));
  }
  enable_logger(qljs_logger::Unwrap(this->logger_.Value()));
  this->logger_enabled_ = true;
  QLJS_DEBUG_LOG("Configured VS Code logger\n");
}

void qljs_workspace::disable_logging() {
  if (!this->logger_enabled_) {
    return;
  }
  QLJS_DEBUG_LOG("Disabling VS Code logger\n");
  QLJS_ASSERT(!this->logger_.IsEmpty());
  disable_logger(qljs_logger::Unwrap(this->logger_.Value()));
  this->logger_enabled_ = false;
}

qljs_workspace::~qljs_workspace() {
  // See NOTE[workspace-cleanup].
  this->dispose();
}

::Napi::Value qljs_workspace::dispose(const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();
  this->dispose();
  return env.Undefined();
}

void qljs_workspace::dispose() {
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

void qljs_workspace::dispose_documents() {
  this->qljs_documents_.for_each([this](::Napi::Value value) -> void {
    qljs_document_base* doc = qljs_document_base::unwrap(value);
    this->delete_diagnostics(doc);
  });
  this->qljs_documents_.clear();

  this->config_loader_.unwatch_all_files();

  this->fs_.clear();
}

::Napi::Value qljs_workspace::close_document(const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  vscode_document vscode_doc(info[0].As<::Napi::Object>());
  ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_doc.get());
  qljs_document_base* doc =
      qljs_doc.IsUndefined() ? nullptr : qljs_document_base::unwrap(qljs_doc);
  this->tracer_.trace_vscode_document_closed(env, vscode_doc, doc);
  if (doc) {
    QLJS_DEBUG_LOG("Document %p: Closing\n", doc);
    this->delete_diagnostics(doc);
    this->qljs_documents_.erase(vscode_doc.get());
    this->fs_.forget_document(doc);
  }

  return env.Undefined();
}

::Napi::Value qljs_workspace::configuration_changed(
    const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  this->update_logging(env);

  return env.Undefined();
}

::Napi::Value qljs_workspace::editor_visibility_changed(
    const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  ::Napi::Object vscode_doc = info[0].As<::Napi::Object>();
  ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_doc);
  qljs_document_base* doc;
  if (qljs_doc.IsUndefined()) {
    vscode_document d(vscode_doc);
    doc = this->maybe_create_document(
        env,
        /*vscode_doc=*/d,
        /*text=*/to_string8_view(d.get_text().Utf8Value()));
    if (doc) {
      doc->after_modification(env, *this, this->diagnostic_collection());
    }
  } else {
    doc = qljs_document_base::unwrap(qljs_doc);
    doc->after_modification(env, *this, this->diagnostic_collection());
  }

  return env.Undefined();
}

::Napi::Value qljs_workspace::document_changed(const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  ::Napi::Value vscode_document = info[0];
  ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_document);
  if (!qljs_doc.IsUndefined()) {
    qljs_document_base* doc = qljs_document_base::unwrap(qljs_doc);
    ::Napi::Array changes = info[1].As<::Napi::Array>();
    this->tracer_.trace_vscode_document_changed(env, doc, changes);
    doc->replace_text(changes);
    doc->after_modification(env, *this, this->diagnostic_collection());
  }

  return env.Undefined();
}

::Napi::Value qljs_workspace::document_saved(const Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  ::Napi::Object vscode_doc = info[0].As<::Napi::Object>();
  ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_doc);
  if (!qljs_doc.IsUndefined()) {
    qljs_document_base* doc = qljs_document_base::unwrap(qljs_doc);
    QLJS_DEBUG_LOG("Document %p: Saved\n", doc);
    this->tracer_.trace_vscode_document_sync(env, vscode_document(vscode_doc),
                                             doc);
  }

  return env.Undefined();
}

qljs_document_base* qljs_workspace::maybe_create_document(
    ::Napi::Env env, vscode_document vscode_doc, string8_view text) {
  ::Napi::Object vscode_document_uri = vscode_doc.uri();
  std::optional<std::string> file_path = std::nullopt;
  if (to_string(vscode_document_uri.Get("scheme")) == "file") {
    file_path = to_string(vscode_document_uri.Get("fsPath"));
  }

  std::string language_id = vscode_doc.language_id();
  qljs_document_base* doc;
  if (language_id == "javascript" || language_id == "javascriptreact") {
    doc = new qljs_lintable_document(vscode_doc, file_path);
  } else if (file_path.has_value() &&
             this->config_loader_.is_config_file_path(*file_path)) {
    doc = new qljs_config_document(vscode_doc, file_path);
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

vscode_diagnostic_collection qljs_workspace::diagnostic_collection() const {
  return vscode_diagnostic_collection(
      this->vscode_diagnostic_collection_ref_.Value());
}

void qljs_workspace::delete_diagnostics(qljs_document_base* doc) {
  vscode_diagnostic_collection(this->vscode_diagnostic_collection_ref_.Value())
      .delete_(doc->uri());
}

void qljs_workspace::report_pending_watch_io_errors(::Napi::Env env) {
  std::vector<watch_io_error> errors =
      this->fs_change_detection_event_loop_.fs()->take_watch_errors();
  if (!errors.empty() && !this->did_report_watch_io_error_) {
    this->vscode_.window_show_warning_message.Value().Call(
        /*this=*/this->vscode_.window_namespace.Value(),
        {
            ::Napi::String::New(env, errors[0].to_string()),
        });
    this->did_report_watch_io_error_ = true;
  }
}

void qljs_workspace::check_for_config_file_changes_from_thread(
    ::Napi::Env env, ::Napi::Object workspace_object) {
  qljs_workspace* workspace = qljs_workspace::Unwrap(workspace_object);
  if (workspace->disposed_) {
    QLJS_DEBUG_LOG(
        "Workspace %p: check_for_config_file_changes_from_thread: workspace "
        "object has been disposed\n",
        workspace);
    return;
  }
  workspace->check_for_config_file_changes(env);
}

void qljs_workspace::check_for_config_file_changes(::Napi::Env env) {
  std::vector<configuration_change> changes = this->config_loader_.refresh();
  for (const configuration_change& change : changes) {
    QLJS_DEBUG_LOG("Configuration changed for %s\n",
                   change.watched_path->c_str());
    qljs_document_base* doc =
        reinterpret_cast<qljs_document_base*>(change.token);
    doc->on_config_file_changed(env, *this, this->diagnostic_collection(),
                                change.config_file);
  }
}

void qljs_workspace::run_fs_change_detection_thread() {
  QLJS_DEBUG_LOG("Workspace %p: starting run_fs_change_detection_thread\n",
                 this);
  this->fs_change_detection_event_loop_.run();
  this->check_for_config_file_changes_on_js_thread_.Release();
  QLJS_DEBUG_LOG("Workspace %p: stopping run_fs_change_detection_thread\n",
                 this);
}

::Napi::Object create_workspace(const ::Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();
  addon_state* state = env.GetInstanceData<addon_state>();

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
