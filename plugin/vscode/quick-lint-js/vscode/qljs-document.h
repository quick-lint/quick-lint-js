// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <napi.h>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
class QLJS_Workspace;

class QLJS_Document_Base {
 public:
  static QLJS_Document_Base* unwrap(::Napi::Value document) {
    QLJS_ASSERT(document.IsExternal());
    ::Napi::External<QLJS_Document_Base> wrapped =
        document.As<::Napi::External<QLJS_Document_Base>>();
    return wrapped.Data();
  }

  explicit QLJS_Document_Base(VSCode_Document doc,
                              const std::optional<std::string>& file_path)
      : vscode_document_(::Napi::Persistent(doc)) {
    if (file_path.has_value()) {
      QLJS_DEBUG_LOG("Document %p: Opened document: %s\n", this,
                     file_path->c_str());
    } else {
      QLJS_DEBUG_LOG("Document %p: Opened unnamed document\n", this);
    }
  }

  virtual ~QLJS_Document_Base() = default;

  // Takes ownership of this qljs_document_base.
  //
  // This qljs_document_base must have been allocated with 'new'.
  ::Napi::Value create_wrapper(::Napi::Env env) {
    ::Napi::External<QLJS_Document_Base> wrapped =
        ::Napi::External<QLJS_Document_Base>::New(
            env, this,
            /*finalize=*/[](::Napi::Env, QLJS_Document_Base* data) -> void {
              delete data;
            });
    return wrapped;
  }

  void replace_text(::Napi::Array changes) {
    QLJS_DEBUG_LOG("Document %p: Replacing text\n", this);
    for (std::uint32_t i = 0; i < changes.Length(); ++i) {
      ::Napi::Object change = changes.Get(i).As<::Napi::Object>();
      ::Napi::Object range = change.Get("range").As<::Napi::Object>();
      ::Napi::Object start = range.Get("start").As<::Napi::Object>();
      ::Napi::Object end = range.Get("end").As<::Napi::Object>();
      LSP_Range r = {
          .start =
              {
                  .line = to_int(start.Get("line")),
                  .character = to_int(start.Get("character")),
              },
          .end =
              {
                  .line = to_int(end.Get("line")),
                  .character = to_int(end.Get("character")),
              },
      };
      std::string replacement_text =
          change.Get("text").As<::Napi::String>().Utf8Value();
      this->document_.replace_text(r, to_string8_view(replacement_text));
    }
  }

  Padded_String_View document_string() { return this->document_.string(); }

  virtual void after_modification(::Napi::Env, QLJS_Workspace&,
                                  VSCode_Diagnostic_Collection) = 0;

  virtual void finish_init(::Napi::Env, QLJS_Workspace&,
                           const std::optional<std::string>& file_path) = 0;

  // config_file is optional.
  virtual void on_config_file_changed(::Napi::Env, QLJS_Workspace&,
                                      VSCode_Diagnostic_Collection,
                                      Loaded_Config_File* config_file) = 0;

 protected:
  ::Napi::Value uri() { return this->vscode_document_.Value().uri(); }

  LSP_Document_Text document_;
  ::Napi::Reference<VSCode_Document> vscode_document_;

  friend class QLJS_Workspace;
};

class QLJS_Config_Document : public QLJS_Document_Base {
 public:
  explicit QLJS_Config_Document(VSCode_Document doc,
                                const std::optional<std::string>& file_path)
      : QLJS_Document_Base(doc, file_path) {
    QLJS_ASSERT(file_path.has_value());
  }

  void after_modification(::Napi::Env, QLJS_Workspace&,
                          VSCode_Diagnostic_Collection) override;
  void finish_init(::Napi::Env, QLJS_Workspace&,
                   const std::optional<std::string>& file_path) override;
  void on_config_file_changed(::Napi::Env, QLJS_Workspace&,
                              VSCode_Diagnostic_Collection,
                              Loaded_Config_File* config_file) override;

 private:
  void lint_config_and_publish_diagnostics(::Napi::Env, VSCode_Module&,
                                           VSCode_Diagnostic_Collection,
                                           Loaded_Config_File* loaded_config);

  ::Napi::Array lint_config(::Napi::Env env, VSCode_Module* vscode,
                            Loaded_Config_File* loaded_config) {
    vscode->load_non_persistent(env);

    LSP_Locator locator(&loaded_config->file_content);
    VSCode_Diag_Reporter diag_reporter(vscode, env, &locator, this->uri());
    diag_reporter.report(loaded_config->errors);
    return std::move(diag_reporter).diagnostics();
  }
};

class QLJS_Lintable_Document : public QLJS_Document_Base {
 public:
  explicit QLJS_Lintable_Document(VSCode_Document doc,
                                  const std::optional<std::string>& file_path,
                                  Linter_Options lint_options)
      : QLJS_Document_Base(doc, file_path), lint_options_(lint_options) {}

  void after_modification(::Napi::Env, QLJS_Workspace&,
                          VSCode_Diagnostic_Collection) override;
  void finish_init(::Napi::Env, QLJS_Workspace&,
                   const std::optional<std::string>& file_path) override;
  void on_config_file_changed(::Napi::Env, QLJS_Workspace&,
                              VSCode_Diagnostic_Collection,
                              Loaded_Config_File* config_file) override;

  void lint_javascript_and_publish_diagnostics(::Napi::Env, VSCode_Module&,
                                               VSCode_Diagnostic_Collection);

 private:
  ::Napi::Array lint_javascript(::Napi::Env env, VSCode_Module* vscode) {
    vscode->load_non_persistent(env);

    VSCode_Diag_Reporter diag_reporter(vscode, env, &this->document_.locator(),
                                       this->uri());
    parse_and_lint(this->document_.string(), diag_reporter,
                   this->config_->globals(), this->lint_options_);

    return std::move(diag_reporter).diagnostics();
  }

  Configuration* config_;  // Initialized by finish_init.
  Linter_Options lint_options_;

  friend class QLJS_Document_Base;
  friend class QLJS_Workspace;
};
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
