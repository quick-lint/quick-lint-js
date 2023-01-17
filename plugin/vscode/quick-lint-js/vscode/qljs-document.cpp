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
#include <quick-lint-js/vscode/napi-support.h>
#include <quick-lint-js/vscode/qljs-document.h>
#include <quick-lint-js/vscode/qljs-workspace.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode-tracer.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>
#include <vector>
// TODO(strager): Trim includes.

namespace quick_lint_js {
void qljs_config_document::after_modification(::Napi::Env env,
                                              qljs_workspace& workspace,
                                              vscode_diagnostic_collection) {
  workspace.check_for_config_file_changes(env);
}

void qljs_lintable_document::after_modification(
    ::Napi::Env env, qljs_workspace& workspace,
    vscode_diagnostic_collection diagnostic_collection) {
  this->lint_javascript_and_publish_diagnostics(env, workspace.vscode_,
                                                diagnostic_collection);
}

void qljs_lintable_document::finish_init(
    ::Napi::Env env, qljs_workspace& workspace,
    const std::optional<std::string>& file_path) {
  this->config_ = &workspace.default_config_;
  if (file_path.has_value()) {
    QLJS_DEBUG_LOG("Workspace %p: watching config for: %s\n", &workspace,
                   file_path->c_str());
    auto loaded_config_result =
        workspace.config_loader_.watch_and_load_for_file(*file_path, this);
    if (loaded_config_result.ok()) {
      loaded_config_file* loaded_config = *loaded_config_result;
      if (loaded_config) {
        if (!loaded_config->errors.empty()) {
          QLJS_ASSERT(loaded_config->config_path);
          std::string config_file_path(loaded_config->config_path->path());
          std::string message = "Problems found in the config file for " +
                                *file_path + " (" + config_file_path + ").";
          workspace.vscode_.show_error_message(
              env, message, {"Open config"},
              [&workspace, self = ::Napi::Persistent(workspace.Value()),
               config_file_path](::Napi::Env callback_env,
                                 ::Napi::Value clicked_button_label) -> void {
                bool popup_dismissed = clicked_button_label.IsUndefined();
                if (popup_dismissed) {
                  return;
                }
                std::string clicked_button_label_string =
                    clicked_button_label.As<::Napi::String>().Utf8Value();
                QLJS_ASSERT(clicked_button_label_string == "Open config");
                workspace.vscode_.open_and_show_text_document_by_path(
                    callback_env, config_file_path);
              });
        }
        this->config_ = &loaded_config->config;
      }
    } else {
      std::string message = "Failed to load configuration file for " +
                            *file_path +
                            ". Using default configuration.\nError details: " +
                            loaded_config_result.error_to_string();
      workspace.vscode_.window_show_error_message.Value().Call(
          /*this=*/workspace.vscode_.window_namespace.Value(),
          {
              ::Napi::String::New(env, message),
          });
    }
  }
}

void qljs_config_document::finish_init(
    ::Napi::Env env, qljs_workspace& workspace,
    const std::optional<std::string>& file_path) {
  QLJS_ASSERT(file_path.has_value());
  QLJS_DEBUG_LOG("Workspace %p: watching config file: %s\n", &workspace,
                 file_path->c_str());
  auto loaded_config_result =
      workspace.config_loader_.watch_and_load_config_file(*file_path, this);
  if (loaded_config_result.ok()) {
    workspace.vscode_.load_non_persistent(env);
    this->lint_config_and_publish_diagnostics(env, workspace.vscode_,
                                              workspace.diagnostic_collection(),
                                              *loaded_config_result);
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

void qljs_config_document::on_config_file_changed(
    ::Napi::Env env, qljs_workspace& workspace,
    vscode_diagnostic_collection diagnostic_collection,
    loaded_config_file* config_file) {
  this->lint_config_and_publish_diagnostics(env, workspace.vscode_,
                                            diagnostic_collection, config_file);
}

void qljs_config_document::lint_config_and_publish_diagnostics(
    ::Napi::Env env, vscode_module& vscode,
    vscode_diagnostic_collection diagnostic_collection,
    loaded_config_file* loaded_config) {
  diagnostic_collection.set(this->uri(),
                            this->lint_config(env, &vscode, loaded_config));
}

void qljs_lintable_document::on_config_file_changed(
    ::Napi::Env env, qljs_workspace& workspace,
    vscode_diagnostic_collection diagnostic_collection,
    loaded_config_file* config_file) {
  this->config_ =
      config_file ? &config_file->config : &workspace.default_config_;
  this->lint_javascript_and_publish_diagnostics(env, workspace.vscode_,
                                                diagnostic_collection);
}

void qljs_lintable_document::lint_javascript_and_publish_diagnostics(
    ::Napi::Env env, vscode_module& vscode,
    vscode_diagnostic_collection diagnostic_collection) {
  diagnostic_collection.set(this->uri(), this->lint_javascript(env, &vscode));
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
