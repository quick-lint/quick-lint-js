// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <napi.h>
#include <optional>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/vscode/qljs-document.h>
#include <quick-lint-js/vscode/qljs-workspace.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
void QLJS_Config_Document::after_modification(::Napi::Env env,
                                              QLJS_Workspace& workspace,
                                              VSCode_Diagnostic_Collection) {
  workspace.check_for_config_file_changes(env);
}

void QLJS_Lintable_Document::after_modification(
    ::Napi::Env env, QLJS_Workspace& workspace,
    VSCode_Diagnostic_Collection diagnostic_collection) {
  this->lint_javascript_and_publish_diagnostics(env, workspace,
                                                diagnostic_collection);
}

void QLJS_Lintable_Document::finish_init(
    ::Napi::Env env, QLJS_Workspace& workspace,
    const std::optional<std::string>& file_path) {
  this->config_ = &workspace.default_config_;
  if (file_path.has_value()) {
    QLJS_DEBUG_LOG("Workspace %p: watching config for: %s\n", &workspace,
                   file_path->c_str());
    auto loaded_config_result =
        workspace.config_loader_.watch_and_load_for_file(*file_path, this);
    if (loaded_config_result.ok()) {
      Loaded_Config_File* loaded_config = *loaded_config_result;
      if (loaded_config) {
        if (!loaded_config->errors.empty()) {
          QLJS_ASSERT(loaded_config->config_path);
          workspace.ui_.show_associated_config_file_errors(
              env, *file_path, loaded_config->config_path->path());
        }
        this->config_ = &loaded_config->config;
      }
    } else {
      workspace.ui_.show_config_file_load_errors(env, *file_path,
                                                 loaded_config_result);
    }
  }
}

void QLJS_Config_Document::finish_init(
    ::Napi::Env env, QLJS_Workspace& workspace,
    const std::optional<std::string>& file_path) {
  QLJS_ASSERT(file_path.has_value());
  QLJS_DEBUG_LOG("Workspace %p: watching config file: %s\n", &workspace,
                 file_path->c_str());
  auto loaded_config_result =
      workspace.config_loader_.watch_and_load_config_file(*file_path, this);
  if (loaded_config_result.ok()) {
    this->loaded_config_ = *loaded_config_result;
    workspace.vscode_.load_non_persistent(env);
    this->lint_config_and_publish_diagnostics(
        env, workspace, workspace.diagnostic_collection());
  } else {
    QLJS_UNIMPLEMENTED();
  }
}

void QLJS_Config_Document::on_config_file_changed(
    ::Napi::Env env, QLJS_Workspace& workspace,
    VSCode_Diagnostic_Collection diagnostic_collection,
    Loaded_Config_File* config_file) {
  this->loaded_config_ = config_file;
  this->lint_config_and_publish_diagnostics(env, workspace,
                                            diagnostic_collection);
}

void QLJS_Config_Document::on_translator_changed(
    ::Napi::Env env, QLJS_Workspace& workspace,
    VSCode_Diagnostic_Collection diagnostic_collection) {
  this->lint_config_and_publish_diagnostics(env, workspace,
                                            diagnostic_collection);
}
void QLJS_Config_Document::lint_config_and_publish_diagnostics(
    ::Napi::Env env, QLJS_Workspace& workspace,
    VSCode_Diagnostic_Collection diagnostic_collection) {
  diagnostic_collection.set(this->uri(), this->lint_config(env, workspace));
}

::Napi::Array QLJS_Config_Document::lint_config(::Napi::Env env,
                                                QLJS_Workspace& workspace) {
  workspace.vscode_.load_non_persistent(env);

  LSP_Locator locator(&this->loaded_config_->file_content);
  VSCode_Diag_Reporter diag_reporter(&workspace.vscode_, env, &locator,
                                     this->uri(), workspace.translator_);
  diag_reporter.report(this->loaded_config_->errors);
  return std::move(diag_reporter).diagnostics();
}

void QLJS_Lintable_Document::on_config_file_changed(
    ::Napi::Env env, QLJS_Workspace& workspace,
    VSCode_Diagnostic_Collection diagnostic_collection,
    Loaded_Config_File* config_file) {
  this->config_ =
      config_file ? &config_file->config : &workspace.default_config_;
  this->lint_javascript_and_publish_diagnostics(env, workspace,
                                                diagnostic_collection);
}

void QLJS_Lintable_Document::on_translator_changed(
    ::Napi::Env env, QLJS_Workspace& workspace,
    VSCode_Diagnostic_Collection diagnostic_collection) {
  this->lint_javascript_and_publish_diagnostics(env, workspace,
                                                diagnostic_collection);
}
void QLJS_Lintable_Document::lint_javascript_and_publish_diagnostics(
    ::Napi::Env env, QLJS_Workspace& workspace,
    VSCode_Diagnostic_Collection diagnostic_collection) {
  diagnostic_collection.set(this->uri(), this->lint_javascript(env, workspace));
}

::Napi::Array QLJS_Lintable_Document::lint_javascript(
    ::Napi::Env env, QLJS_Workspace& workspace) {
  VSCode_Module& vscode = workspace.vscode_;
  vscode.load_non_persistent(env);

  VSCode_Diag_Reporter diag_reporter(&vscode, env, &this->document_.locator(),
                                     this->uri(), workspace.translator_);
  parse_and_lint(this->document_.string(), diag_reporter,
                 Linter_Options{
                     .language = this->language_,
                     .configuration = this->config_,
                 });

  return std::move(diag_reporter).diagnostics();
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
