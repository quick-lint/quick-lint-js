// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VSCODE_QLJS_DOCUMENT_H
#define QUICK_LINT_JS_VSCODE_QLJS_DOCUMENT_H

#include <napi.h>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
class qljs_workspace;

class qljs_document_base {
 public:
  static qljs_document_base* unwrap(::Napi::Value document) {
    QLJS_ASSERT(document.IsExternal());
    ::Napi::External<qljs_document_base> wrapped =
        document.As<::Napi::External<qljs_document_base>>();
    return wrapped.Data();
  }

  explicit qljs_document_base(vscode_document doc,
                              const std::optional<std::string>& file_path)
      : vscode_document_(::Napi::Persistent(doc)) {
    if (file_path.has_value()) {
      QLJS_DEBUG_LOG("Document %p: Opened document: %s\n", this,
                     file_path->c_str());
    } else {
      QLJS_DEBUG_LOG("Document %p: Opened unnamed document\n", this);
    }
  }

  virtual ~qljs_document_base() = default;

  // Takes ownership of this qljs_document_base.
  //
  // This qljs_document_base must have been allocated with 'new'.
  ::Napi::Value create_wrapper(::Napi::Env env) {
    ::Napi::External<qljs_document_base> wrapped =
        ::Napi::External<qljs_document_base>::New(
            env, this,
            /*finalize=*/[](::Napi::Env, qljs_document_base* data) -> void {
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
      lsp_range r = {
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

  padded_string_view document_string() noexcept {
    return this->document_.string();
  }

  virtual void after_modification(::Napi::Env, qljs_workspace&,
                                  vscode_diagnostic_collection) = 0;

  virtual void finish_init(::Napi::Env, qljs_workspace&,
                           const std::optional<std::string>& file_path) = 0;

  // config_file is optional.
  virtual void on_config_file_changed(::Napi::Env, qljs_workspace&,
                                      vscode_diagnostic_collection,
                                      loaded_config_file* config_file) = 0;

 protected:
  ::Napi::Value uri() { return this->vscode_document_.Value().uri(); }

  document document_;
  ::Napi::Reference<vscode_document> vscode_document_;

  friend class qljs_workspace;
};

class qljs_config_document : public qljs_document_base {
 public:
  explicit qljs_config_document(vscode_document doc,
                                const std::optional<std::string>& file_path)
      : qljs_document_base(doc, file_path) {
    QLJS_ASSERT(file_path.has_value());
  }

  void after_modification(::Napi::Env, qljs_workspace&,
                          vscode_diagnostic_collection) override;
  void finish_init(::Napi::Env, qljs_workspace&,
                   const std::optional<std::string>& file_path) override;
  void on_config_file_changed(::Napi::Env, qljs_workspace&,
                              vscode_diagnostic_collection,
                              loaded_config_file* config_file) override;

 private:
  void lint_config_and_publish_diagnostics(::Napi::Env, vscode_module&,
                                           vscode_diagnostic_collection,
                                           loaded_config_file* loaded_config);

  ::Napi::Array lint_config(::Napi::Env env, vscode_module* vscode,
                            loaded_config_file* loaded_config) {
    vscode->load_non_persistent(env);

    lsp_locator locator(&loaded_config->file_content);
    vscode_diag_reporter diag_reporter(vscode, env, &locator, this->uri());
    loaded_config->errors.copy_into(&diag_reporter);
    return std::move(diag_reporter).diagnostics();
  }
};

class qljs_lintable_document : public qljs_document_base {
 public:
  explicit qljs_lintable_document(vscode_document doc,
                                  const std::optional<std::string>& file_path,
                                  linter_options lint_options)
      : qljs_document_base(doc, file_path), lint_options_(lint_options) {}

  void after_modification(::Napi::Env, qljs_workspace&,
                          vscode_diagnostic_collection) override;
  void finish_init(::Napi::Env, qljs_workspace&,
                   const std::optional<std::string>& file_path) override;
  void on_config_file_changed(::Napi::Env, qljs_workspace&,
                              vscode_diagnostic_collection,
                              loaded_config_file* config_file) override;

  void lint_javascript_and_publish_diagnostics(::Napi::Env, vscode_module&,
                                               vscode_diagnostic_collection);

 private:
  ::Napi::Array lint_javascript(::Napi::Env env, vscode_module* vscode) {
    vscode->load_non_persistent(env);

    vscode_diag_reporter diag_reporter(vscode, env, &this->document_.locator(),
                                       this->uri());
    parse_and_lint(this->document_.string(), diag_reporter,
                   this->config_->globals(), this->lint_options_);

    return std::move(diag_reporter).diagnostics();
  }

  configuration* config_;  // Initialized by finish_init.
  linter_options lint_options_;

  friend class qljs_document_base;
  friend class qljs_workspace;
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
