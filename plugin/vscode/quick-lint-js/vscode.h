// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VSCODE_H
#define QUICK_LINT_JS_VSCODE_H

#include <memory>
#include <napi.h>
#include <quick-lint-js/napi-support.h>
#include <string>
#include <utility>

namespace quick_lint_js {
// The 'vscode' CommonJS module.
//
// This object caches commonly-used classes and primitives from the 'vscode'
// module.
struct vscode_module {
  explicit vscode_module(::Napi::Object module)
      : diagnostic_class(::Napi::Persistent(
            module.Get("Diagnostic").As<::Napi::Function>())),
        diagnostic_related_information_class(::Napi::Persistent(
            module.Get("DiagnosticRelatedInformation").As<::Napi::Function>())),
        location_class(
            ::Napi::Persistent(module.Get("Location").As<::Napi::Function>())),
        position_class(
            ::Napi::Persistent(module.Get("Position").As<::Napi::Function>())),
        range_class(
            ::Napi::Persistent(module.Get("Range").As<::Napi::Function>())),
        uri_class(::Napi::Persistent(module.Get("Uri").As<::Napi::Function>())),
        uri_file(::Napi::Persistent(
            this->uri_class.Value().Get("file").As<::Napi::Function>())),
        diagnostic_severity_enum(::Napi::Persistent(
            module.Get("DiagnosticSeverity").As<::Napi::Object>())),
        window_namespace(
            ::Napi::Persistent(module.Get("window").As<::Napi::Object>())),
        window_create_output_channel(
            ::Napi::Persistent(this->window_namespace.Get("createOutputChannel")
                                   .As<::Napi::Function>())),
        window_show_error_message(
            ::Napi::Persistent(this->window_namespace.Get("showErrorMessage")
                                   .As<::Napi::Function>())),
        window_show_text_document(
            ::Napi::Persistent(this->window_namespace.Get("showTextDocument")
                                   .As<::Napi::Function>())),
        window_show_warning_message(
            ::Napi::Persistent(this->window_namespace.Get("showWarningMessage")
                                   .As<::Napi::Function>())),
        workspace_namespace(
            ::Napi::Persistent(module.Get("workspace").As<::Napi::Object>())),
        workspace_get_configuration(
            ::Napi::Persistent(this->workspace_namespace.Get("getConfiguration")
                                   .As<::Napi::Function>())),
        workspace_open_text_document(
            ::Napi::Persistent(this->workspace_namespace.Get("openTextDocument")
                                   .As<::Napi::Function>())) {}

  void load_non_persistent(::Napi::Env) {
    this->diagnostic_severity_error =
        this->diagnostic_severity_enum.Get("Error").As<::Napi::Number>();
    this->diagnostic_severity_warning =
        this->diagnostic_severity_enum.Get("Warning").As<::Napi::Number>();
  }

  // Calls window.workspace.openTextDocument(vscode.Uri.file(path))
  //       .then(callback).
  //
  // Note: to avoid use-after-free errors, 'callback' should capture a
  // ::Napi::Reference to each object it uses.
  //
  // Signature of callback:
  // void callback(::Napi::Env, ::Napi::Value document);
  template <class Func>
  void open_text_document_by_path(::Napi::Env env, const std::string& path,
                                  Func&& callback) {
    struct async_state {
      ::Napi::ObjectReference workspace_namespace;
      ::Napi::FunctionReference workspace_open_text_document;
      ::Napi::FunctionReference uri_class;
      ::Napi::FunctionReference uri_file;
      Func callback;
      std::string path;
    };
    std::shared_ptr<async_state> state(new async_state{
        .workspace_namespace =
            ::Napi::Persistent(this->workspace_namespace.Value()),
        .workspace_open_text_document =
            ::Napi::Persistent(this->workspace_open_text_document.Value()),
        .uri_class = ::Napi::Persistent(this->uri_class.Value()),
        .uri_file = ::Napi::Persistent(this->uri_file.Value()),
        .callback = std::forward<Func>(callback),
        .path = path,
    });

    ::Napi::Function open_text_document_func = ::Napi::Function::New(
        env,
        [state = std::move(state)](const ::Napi::CallbackInfo& info) -> void {
          ::Napi::Env env = info.Env();

          ::Napi::Value uri = state->uri_file.Value().Call(
              /*this=*/state->uri_class.Value(),
              {::Napi::String::New(env, state->path)});
          ::Napi::Value promise =
              state->workspace_open_text_document.Value().Call(
                  /*this=*/state->workspace_namespace.Value(), {uri});

          promise_then(promise,
                       [state](const ::Napi::CallbackInfo& info) -> void {
                         std::move(state->callback)(info.Env(), info[0]);
                       });

          state->workspace_namespace.Reset();
          state->workspace_open_text_document.Reset();
          state->uri_class.Reset();
          state->uri_file.Reset();
          state->path.clear();
        });
    call_on_next_tick(env, open_text_document_func,
                      /*this=*/env.Undefined(), {});
  }

  void open_and_show_text_document_by_path(::Napi::Env env,
                                           const std::string& path) {
    struct async_state {
      ::Napi::ObjectReference window_namespace;
      ::Napi::FunctionReference window_show_text_document;
    };
    std::shared_ptr<async_state> state(new async_state{
        .window_namespace = ::Napi::Persistent(this->window_namespace.Value()),
        .window_show_text_document =
            ::Napi::Persistent(this->window_show_text_document.Value()),
    });

    this->open_text_document_by_path(
        env, path,
        [state = std::move(state)](::Napi::Env, ::Napi::Value document) {
          state->window_show_text_document.Value().Call(
              /*this=*/state->window_namespace.Value(), {document});
        });
  }

  // Calls window.showErrorMessage(message, ...button_labels).then(callback).
  //
  // Note: to avoid use-after-free errors, 'callback' should capture a
  // ::Napi::Reference to each object it uses.
  //
  // Signature of callback:
  // void callback(::Napi::Env, ::Napi::Value);
  template <class Func>
  void show_error_message(::Napi::Env env, const std::string& message,
                          std::vector<std::string> button_labels,
                          Func callback) {
    std::vector<::napi_value> args;
    args.push_back(::Napi::String::New(env, message));
    for (std::string& label : button_labels) {
      args.push_back(::Napi::String::New(env, std::move(label)));
    }
    ::Napi::Value promise = this->window_show_error_message.Value().Call(
        /*this=*/this->window_namespace.Value(), std::move(args));

    promise_then(promise,
                 [shared_callback = std::make_shared<Func>(std::move(
                      callback))](const ::Napi::CallbackInfo& info) -> void {
                   std::move (*shared_callback)(info.Env(), info[0]);
                 });
  }

  // Calls window.createOutputChannel(name).
  ::Napi::Object create_output_channel(::Napi::Env env, const char* name) {
    return this->window_create_output_channel
        .Call(this->window_namespace.Value(), {::Napi::String::New(env, name)})
        .As<::Napi::Object>();
  }

  // Calls window.getConfiguration(section).
  ::Napi::Object get_configuration(::Napi::Env env, const char* section) {
    return this->workspace_get_configuration
        .Call(this->workspace_namespace.Value(),
              {::Napi::String::New(env, section)})
        .As<::Napi::Object>();
  }

  // vscode.Diagnostic
  ::Napi::FunctionReference diagnostic_class;
  // vscode.DiagnosticRelatedInformation
  ::Napi::FunctionReference diagnostic_related_information_class;
  // vscode.Location
  ::Napi::FunctionReference location_class;
  // vscode.Position
  ::Napi::FunctionReference position_class;
  // vscode.Range
  ::Napi::FunctionReference range_class;

  // vscode.Uri
  ::Napi::FunctionReference uri_class;
  // vscode.Uri.file
  ::Napi::FunctionReference uri_file;

  // vscode.DiagnosticSeverity
  ::Napi::ObjectReference diagnostic_severity_enum;
  // vscode.DiagnosticSeverity.Error (not persistent)
  ::Napi::Number diagnostic_severity_error;
  // vscode.DiagnosticSeverity.Warning (not persistent)
  ::Napi::Number diagnostic_severity_warning;

  // vscode.window
  ::Napi::ObjectReference window_namespace;
  // vscode.window.createOutputChannel
  ::Napi::FunctionReference window_create_output_channel;
  // vscode.window.showErrorMessage
  ::Napi::FunctionReference window_show_error_message;
  // vscode.window.showTextDocument
  ::Napi::FunctionReference window_show_text_document;
  // vscode.window.showWarningMessage
  ::Napi::FunctionReference window_show_warning_message;

  // vscode.workspace
  ::Napi::ObjectReference workspace_namespace;
  // vscode.workspace.getConfiguration
  ::Napi::FunctionReference workspace_get_configuration;
  // vscode.workspace.openTextDocument
  ::Napi::FunctionReference workspace_open_text_document;
};

// A non-owning wrapper around a vscode.Document.
class vscode_document {
 public:
  // Do not call.
  explicit vscode_document() : vscode_document(::Napi::Object()) {}

  // Do not call.
  explicit vscode_document(napi_env env, napi_value value)
      : vscode_document(::Napi::Object(env, value)) {}

  explicit vscode_document(::Napi::Object doc) : doc_(doc) {}

  // vscode.Document#getText()
  ::Napi::String get_text() {
    return this->doc_.Get("getText")
        .As<::Napi::Function>()
        .Call(this->doc_, {})
        .As<::Napi::String>();
  }

  // vscode.Document#languageId
  std::string language_id() { return to_string(this->doc_.Get("languageId")); }

  // vscode.Document#uri
  ::Napi::Object uri() { return this->doc_.Get("uri").As<::Napi::Object>(); }

  napi_env Env() const { return this->doc_.Env(); }
  operator napi_value() const { return this->doc_; }
  operator ::Napi::Value() const { return this->doc_; }

 private:
  ::Napi::Object doc_;
};
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
