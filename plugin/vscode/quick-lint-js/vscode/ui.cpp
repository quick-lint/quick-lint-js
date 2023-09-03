// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/vscode/qljs-workspace.h>
#include <quick-lint-js/vscode/ui.h>
#include <quick-lint-js/vscode/vscode.h>

namespace quick_lint_js {
VSCode_UI::VSCode_UI(QLJS_Workspace* workspace) : workspace_(workspace) {}

void VSCode_UI::show_watch_io_errors(::Napi::Env env,
                                     Span<const Watch_IO_Error> errors) {
  QLJS_ASSERT(!errors.empty());
  this->vscode().window_show_warning_message.Value().Call(
      /*this=*/this->vscode().window_namespace.Value(),
      {
          ::Napi::String::New(env, errors[0].to_string()),
      });
}

void VSCode_UI::show_associated_config_file_errors(
    ::Napi::Env env, std::string_view document_path,
    std::string_view config_file_path) {
  std::string message = concat("Problems found in the config file for "sv,
                               document_path, " ("sv, config_file_path, ")."sv);
  this->vscode().show_error_message(
      env, message, {"Open config"},
      [this,
       // See NOTE[VSCode_UI-lifetime].
       self = ::Napi::Persistent(this->workspace_->Value()),
       config_file_path = std::string(config_file_path)](
          ::Napi::Env callback_env,
          ::Napi::Value clicked_button_label) -> void {
        bool popup_dismissed = clicked_button_label.IsUndefined();
        if (popup_dismissed) {
          return;
        }
        std::string clicked_button_label_string =
            clicked_button_label.As<::Napi::String>().Utf8Value();
        QLJS_ASSERT(clicked_button_label_string == "Open config");
        this->vscode().open_and_show_text_document_by_path(callback_env,
                                                           config_file_path);
      });
}

void VSCode_UI::show_config_file_load_errors(
    ::Napi::Env env, std::string_view document_path,
    const Result<Loaded_Config_File*, Configuration_Load_IO_Error>& error) {
  QLJS_ASSERT(!error.ok());
  std::string message =
      concat("Failed to load configuration file for "sv, document_path,
             ". Using default configuration.\nError details: "sv,
             error.error_to_string());
  this->vscode().window_show_error_message.Value().Call(
      /*this=*/this->vscode().window_namespace.Value(),
      {
          ::Napi::String::New(env, message),
      });
}

VSCode_Module& VSCode_UI::vscode() { return this->workspace_->vscode_; }
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
