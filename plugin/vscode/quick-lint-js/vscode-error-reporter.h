// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VSCODE_ERROR_REPORTER_H
#define QUICK_LINT_JS_VSCODE_ERROR_REPORTER_H

#include <napi.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diagnostic-formatter.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/vscode.h>
#include <string_view>

namespace quick_lint_js {
class vscode_error_formatter
    : public diagnostic_formatter<vscode_error_formatter> {
 public:
  explicit vscode_error_formatter(vscode_module* vscode, ::Napi::Env env,
                                  ::Napi::Array diagnostics,
                                  const lsp_locator* locator)
      : vscode_(vscode),
        env_(env),
        diagnostics_(diagnostics),
        locator_(locator) {}

  void write_before_message([[maybe_unused]] std::string_view code,
                            diagnostic_severity, const source_code_span&) {}

  void write_message_part([[maybe_unused]] std::string_view code,
                          diagnostic_severity, string8_view message_part) {
    this->message_.append(message_part);
  }

  void write_after_message(std::string_view code, diagnostic_severity sev,
                           const source_code_span& origin) {
    ::Napi::Value severity;
    switch (sev) {
    case diagnostic_severity::error:
      severity = this->vscode_->diagnostic_severity_error;
      break;
    case diagnostic_severity::warning:
      severity = this->vscode_->diagnostic_severity_warning;
      break;
    case diagnostic_severity::note:
      // Don't write notes. Only write the main message.
      return;
    }

    lsp_range r = this->locator_->range(origin);
    ::Napi::Value start = this->vscode_->position_class.New(
        {::Napi::Number::New(this->env_, r.start.line),
         ::Napi::Number::New(this->env_, r.start.character)});
    ::Napi::Value end = this->vscode_->position_class.New(
        {::Napi::Number::New(this->env_, r.end.line),
         ::Napi::Number::New(this->env_, r.end.character)});
    ::Napi::Object diag = this->vscode_->diagnostic_class.New({
        /*range=*/this->vscode_->range_class.New({start, end}),
        /*message=*/
        ::Napi::String::New(
            this->env_, reinterpret_cast<const char*>(this->message_.data())),
        /*severity=*/severity,
    });

    char error_path[14];
    sprintf(error_path, "/errors/%s", code.data());

    ::Napi::Object uri = ::Napi::Object::New(this->env_);
    uri.Set("scheme", ::Napi::String::New(this->env_, "https"));
    uri.Set("authority", ::Napi::String::New(this->env_, "quick-lint-js.com"));
    uri.Set("path", ::Napi::String::New(this->env_, error_path));
    uri.Set("fragment", ::Napi::String::New(this->env_, ""));
    uri.Set("query", ::Napi::String::New(this->env_, ""));

    ::Napi::Object code_obj = ::Napi::Object::New(this->env_);
    code_obj.Set("target", uri);
    code_obj.Set("value", code.data());

    diag.Set("code", code_obj);
    diag.Set("source", ::Napi::String::New(this->env_, "quick-lint-js"));
    this->diagnostics_.Set(this->diagnostics_.Length(), diag);
  }

 private:
  vscode_module* vscode_;
  ::Napi::Env env_;
  ::Napi::Array diagnostics_;
  const lsp_locator* locator_;
  string8 message_;
};

class vscode_error_reporter final : public error_reporter {
 public:
  explicit vscode_error_reporter(vscode_module* vscode, ::Napi::Env env,
                                 const lsp_locator* locator) noexcept
      : vscode_(vscode),
        env_(env),
        diagnostics_(::Napi::Array::New(env)),
        locator_(locator) {}

  ::Napi::Array diagnostics() const { return this->diagnostics_; }

  void report_impl(error_type type, void* error) override {
    vscode_error_formatter formatter(
        /*vscode=*/this->vscode_,
        /*env=*/this->env_,
        /*diagnostics=*/this->diagnostics_,
        /*locator=*/this->locator_);
    formatter.format(get_diagnostic_info(type), error);
  }

 private:
  vscode_module* vscode_;
  ::Napi::Env env_;
  ::Napi::Array diagnostics_;
  const lsp_locator* locator_;
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
