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
                                  const lsp_locator* locator,
                                  ::Napi::Value document_uri)
      : vscode_(vscode),
        env_(env),
        diagnostics_(diagnostics),
        locator_(locator),
        document_uri_(document_uri) {}

  void write_before_message([[maybe_unused]] std::string_view code,
                            diagnostic_severity, const source_code_span&) {}

  void write_message_part([[maybe_unused]] std::string_view code,
                          diagnostic_severity, string8_view message_part) {
    this->message_.append(message_part);
  }

  void write_after_message(std::string_view code, diagnostic_severity sev,
                           const source_code_span& origin) {
    switch (sev) {
    case diagnostic_severity::error:
      this->append_diagnostic(code, this->vscode_->diagnostic_severity_error,
                              origin);
      break;
    case diagnostic_severity::warning:
      this->append_diagnostic(code, this->vscode_->diagnostic_severity_warning,
                              origin);
      break;
    case diagnostic_severity::note: {
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
                         const source_code_span& origin) {
    ::Napi::Object diag = this->vscode_->diagnostic_class.New({
        /*range=*/this->new_range(origin),
        /*message=*/
        ::Napi::String::New(
            this->env_, reinterpret_cast<const char*>(this->message_.data())),
        /*severity=*/severity,
    });

    ::Napi::Value uri = this->vscode_->uri_class.New(
        {/*scheme=*/::Napi::String::New(this->env_, "https"),
         /*authority=*/::Napi::String::New(this->env_, "quick-lint-js.com"),
         /*path=*/
         ::Napi::String::New(this->env_, "/errors/" + std::string(code) + "/"),
         /*query=*/::Napi::String::New(this->env_, ""),
         /*fragment=*/::Napi::String::New(this->env_, "")});

    ::Napi::Object code_obj = ::Napi::Object::New(this->env_);
    code_obj.Set("target", uri);
    code_obj.Set("value", code.data());

    diag.Set("code", code_obj);
    diag.Set("source", ::Napi::String::New(this->env_, "quick-lint-js"));
    this->diagnostics_.Set(this->diagnostics_.Length(), diag);
  }

  // Returns a vscode.Range object.
  ::Napi::Value new_range(const source_code_span& span) {
    lsp_range r = this->locator_->range(span);
    ::Napi::Value start = this->vscode_->position_class.New(
        {::Napi::Number::New(this->env_, r.start.line),
         ::Napi::Number::New(this->env_, r.start.character)});
    ::Napi::Value end = this->vscode_->position_class.New(
        {::Napi::Number::New(this->env_, r.end.line),
         ::Napi::Number::New(this->env_, r.end.character)});
    return this->vscode_->range_class.New({start, end});
  }

  vscode_module* vscode_;
  ::Napi::Env env_;
  ::Napi::Array diagnostics_;
  const lsp_locator* locator_;
  ::Napi::Value document_uri_;
  string8 message_;
};

class vscode_error_reporter final : public error_reporter {
 public:
  explicit vscode_error_reporter(vscode_module* vscode, ::Napi::Env env,
                                 const lsp_locator* locator,
                                 ::Napi::Value document_uri) noexcept
      : vscode_(vscode),
        env_(env),
        diagnostics_(::Napi::Array::New(env)),
        locator_(locator),
        document_uri_(document_uri) {}

  ::Napi::Array diagnostics() const { return this->diagnostics_; }

  void report_impl(error_type type, void* error) override {
    vscode_error_formatter formatter(
        /*vscode=*/this->vscode_,
        /*env=*/this->env_,
        /*diagnostics=*/this->diagnostics_,
        /*locator=*/this->locator_,
        /*document_uri=*/this->document_uri_);
    formatter.format(get_diagnostic_info(type), error);
  }

 private:
  vscode_module* vscode_;
  ::Napi::Env env_;
  ::Napi::Array diagnostics_;
  const lsp_locator* locator_;
  ::Napi::Value document_uri_;
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
