// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <napi.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/i18n/translation.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string_view>

namespace quick_lint_js {
class VSCode_Diag_Formatter
    : public Diagnostic_Formatter<VSCode_Diag_Formatter> {
 public:
  explicit VSCode_Diag_Formatter(VSCode_Module* vscode, ::Napi::Env env,
                                 ::Napi::Array diagnostics,
                                 const LSP_Locator* locator,
                                 ::Napi::Value document_uri)
      : Diagnostic_Formatter(qljs_messages),
        vscode_(vscode),
        env_(env),
        diagnostics_(diagnostics),
        locator_(locator),
        document_uri_(document_uri) {}

  void write_before_message([[maybe_unused]] std::string_view code,
                            Diagnostic_Severity, const Source_Code_Span&) {}

  void write_message_part([[maybe_unused]] std::string_view code,
                          Diagnostic_Severity, String8_View message_part) {
    this->message_.append(message_part);
  }

  void write_after_message(std::string_view code, Diagnostic_Severity sev,
                           const Source_Code_Span& origin) {
    switch (sev) {
    case Diagnostic_Severity::error:
      this->append_diagnostic(code, this->vscode_->diagnostic_severity_error,
                              origin);
      break;
    case Diagnostic_Severity::warning:
      this->append_diagnostic(code, this->vscode_->diagnostic_severity_warning,
                              origin);
      break;
    case Diagnostic_Severity::note: {
      ::Napi::Object location = this->vscode_->location_class.New({
          /*uri=*/this->document_uri_,
          /*rangeOrPosition=*/this->new_range(origin),
      });
      ::Napi::Object related_info =
          this->vscode_->diagnostic_related_information_class.New({
              /*location=*/location,
              /*message=*/
              ::Napi::String::New(this->env_, reinterpret_cast<const char*>(
                                                  this->message_.data())),
          });

      ::Napi::Object last_diagnostic =
          this->diagnostics_.Get(this->diagnostics_.Length() - 1)
              .As<::Napi::Object>();
      // NOTE(multiple notes): If the following assertion fails, then it's
      // probably because quick-lint-js started reporting more than one note for
      // a diagnostic.
      QLJS_ASSERT(last_diagnostic.Get("relatedInformation").IsUndefined());

      ::Napi::Array related_infos = ::Napi::Array::New(this->env_);
      related_infos.Set(related_infos.Length(), related_info);
      last_diagnostic.Set("relatedInformation", related_infos);
      break;
    }
    }

    this->message_.clear();
  }

 private:
  void append_diagnostic(std::string_view code, ::Napi::Value severity,
                         const Source_Code_Span& origin) {
    ::Napi::Object diag = this->vscode_->diagnostic_class.New({
        /*range=*/this->new_range(origin),
        /*message=*/
        ::Napi::String::New(
            this->env_, reinterpret_cast<const char*>(this->message_.data())),
        /*severity=*/severity,
    });

    std::string code_string(code);
    ::Napi::Value uri = this->vscode_->uri_class.New(
        {/*scheme=*/::Napi::String::New(this->env_, "https"),
         /*authority=*/::Napi::String::New(this->env_, "quick-lint-js.com"),
         /*path=*/
         ::Napi::String::New(this->env_, "/errors/" + code_string + "/"),
         /*query=*/::Napi::String::New(this->env_, ""),
         /*fragment=*/::Napi::String::New(this->env_, "")});

    ::Napi::Object code_obj = ::Napi::Object::New(this->env_);
    code_obj.Set("target", uri);
    code_obj.Set("value", code_string);

    diag.Set("code", code_obj);
    diag.Set("source", ::Napi::String::New(this->env_, "quick-lint-js"));
    this->diagnostics_.Set(this->diagnostics_.Length(), diag);
  }

  // Returns a vscode.Range object.
  ::Napi::Value new_range(const Source_Code_Span& span) {
    LSP_Range r = this->locator_->range(span);
    ::Napi::Value start = this->vscode_->position_class.New(
        {::Napi::Number::New(this->env_, r.start.line),
         ::Napi::Number::New(this->env_, r.start.character)});
    ::Napi::Value end = this->vscode_->position_class.New(
        {::Napi::Number::New(this->env_, r.end.line),
         ::Napi::Number::New(this->env_, r.end.character)});
    return this->vscode_->range_class.New({start, end});
  }

  VSCode_Module* vscode_;
  ::Napi::Env env_;
  ::Napi::Array diagnostics_;
  const LSP_Locator* locator_;
  ::Napi::Value document_uri_;
  String8 message_;
};

class VSCode_Diag_Reporter final : public Diag_Reporter {
 public:
  explicit VSCode_Diag_Reporter(VSCode_Module* vscode, ::Napi::Env env,
                                const LSP_Locator* locator,
                                ::Napi::Value document_uri)
      : vscode_(vscode),
        env_(env),
        diagnostics_(::Napi::Array::New(env)),
        locator_(locator),
        document_uri_(document_uri) {}

  ::Napi::Array diagnostics() const { return this->diagnostics_; }

  void report_impl(Diag_Type type, void* diag) override {
    VSCode_Diag_Formatter formatter(
        /*vscode=*/this->vscode_,
        /*env=*/this->env_,
        /*diagnostics=*/this->diagnostics_,
        /*locator=*/this->locator_,
        /*document_uri=*/this->document_uri_);
    formatter.format(get_diagnostic_info(type), diag);
  }

 private:
  VSCode_Module* vscode_;
  ::Napi::Env env_;
  ::Napi::Array diagnostics_;
  const LSP_Locator* locator_;
  ::Napi::Value document_uri_;
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
