// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <memory>
#include <napi.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/error-formatter.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/parse.h>

namespace quick_lint_js {
namespace {
int to_int(::Napi::Value);

// State global to a specific Node.js instance/thread.
class addon_state {
 public:
  static std::unique_ptr<addon_state> create(::Napi::Env env);

  ::Napi::FunctionReference qljs_document_class;
};

// The 'vscode' CommonJS module.
//
// This object caches commonly-used classes and primitives from the 'vscode'
// module.
struct vscode_module {
  explicit vscode_module(::Napi::Object module)
      : diagnostic_class(::Napi::Persistent(
            module.Get("Diagnostic").As<::Napi::Function>())),
        position_class(
            ::Napi::Persistent(module.Get("Position").As<::Napi::Function>())),
        range_class(
            ::Napi::Persistent(module.Get("Range").As<::Napi::Function>())),
        diagnostic_severity_enum(::Napi::Persistent(
            module.Get("DiagnosticSeverity").As<::Napi::Object>())) {}

  void load_non_persistent(::Napi::Env) {
    this->diagnostic_severity_error =
        this->diagnostic_severity_enum.Get("Error").As<::Napi::Number>();
    this->diagnostic_severity_warning =
        this->diagnostic_severity_enum.Get("Warning").As<::Napi::Number>();
  }

  // vscode.Diagnostic
  ::Napi::FunctionReference diagnostic_class;
  // vscode.Position
  ::Napi::FunctionReference position_class;
  // vscode.Range
  ::Napi::FunctionReference range_class;

  // vscode.DiagnosticSeverity
  ::Napi::ObjectReference diagnostic_severity_enum;
  // vscode.DiagnosticSeverity.Error (not persistent)
  ::Napi::Number diagnostic_severity_error;
  // vscode.DiagnosticSeverity.Warning (not persistent)
  ::Napi::Number diagnostic_severity_warning;
};

class vscode_error_formatter : public error_formatter<vscode_error_formatter> {
 public:
  explicit vscode_error_formatter(vscode_module* vscode, ::Napi::Env env,
                                  ::Napi::Array diagnostics,
                                  const lsp_locator* locator, const char* code)
      : vscode_(vscode),
        env_(env),
        diagnostics_(diagnostics),
        locator_(locator),
        code_(code) {}

  void write_before_message(severity, const source_code_span&) {}

  void write_message_part(severity, string8_view message_part) {
    this->message_.append(message_part);
  }

  void write_after_message(severity sev, const source_code_span& origin) {
    ::Napi::Value severity;
    switch (sev) {
    case severity::error:
      severity = this->vscode_->diagnostic_severity_error;
      break;
    case severity::warning:
      severity = this->vscode_->diagnostic_severity_warning;
      break;
    case severity::note:
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
    diag.Set("code", ::Napi::String::New(this->env_, this->code_));
    diag.Set("source", ::Napi::String::New(this->env_, "quick-lint-js"));
    this->diagnostics_.Set(this->diagnostics_.Length(), diag);
  }

 private:
  vscode_module* vscode_;
  ::Napi::Env env_;
  ::Napi::Array diagnostics_;
  const lsp_locator* locator_;
  const char* code_;
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

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  void report(name e) { format_error(e, this->format(code)); }
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  ::Napi::Array diagnostics() const { return this->diagnostics_; }

 private:
  vscode_error_formatter format(const char* code) {
    return vscode_error_formatter(
        /*vscode=*/this->vscode_,
        /*env=*/this->env_,
        /*diagnostics=*/this->diagnostics_,
        /*locator=*/this->locator_,
        /*code=*/code);
  }

  vscode_module* vscode_;
  ::Napi::Env env_;
  ::Napi::Array diagnostics_;
  const lsp_locator* locator_;
};

class qljs_document : public ::Napi::ObjectWrap<qljs_document> {
 public:
  static ::Napi::Function init(::Napi::Env env) {
    return DefineClass(
        env, "Document",
        {
            InstanceMethod<&qljs_document::dispose>("dispose"),
            InstanceMethod<&qljs_document::lint>("lint"),
            InstanceMethod<&qljs_document::replace_text>("replaceText"),
            InstanceMethod<&qljs_document::set_text>("setText"),
        });
  }

  explicit qljs_document(const ::Napi::CallbackInfo& info)
      : ::Napi::ObjectWrap<qljs_document>(info),
        vscode_(info[0].As<::Napi::Object>()) {}

  ::Napi::Value dispose(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();

    // TODO(strager): Reduce memory usage.

    return env.Undefined();
  }

  ::Napi::Value replace_text(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();
    if (!(info.Length() >= 1 && info[0].IsArray())) {
      ::Napi::TypeError::New(env, "Expected Array")
          .ThrowAsJavaScriptException();
      return env.Undefined();
    }

    ::Napi::Array changes = info[0].As<::Napi::Array>();
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

    return env.Undefined();
  }

  ::Napi::Value set_text(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();
    if (!(info.Length() >= 1 && info[0].IsString())) {
      ::Napi::TypeError::New(env, "Expected string")
          .ThrowAsJavaScriptException();
      return env.Undefined();
    }

    ::Napi::String text = info[0].As<::Napi::String>();
    this->document_.set_text(to_string8_view(text.Utf8Value()));

    return env.Undefined();
  }

  ::Napi::Value lint(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();
    this->vscode_.load_non_persistent(env);

    vscode_error_reporter error_reporter(&this->vscode_, env,
                                         &this->document_.locator());
    parser p(this->document_.string(), &error_reporter);
    linter l(&error_reporter, &this->config_.globals());
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(l);
    if (!ok) {
      // TODO(strager): Show a pop-up message explaining that the parser
      // crashed.
    }
    return error_reporter.diagnostics();
  }

 private:
  vscode_module vscode_;
  document<lsp_locator> document_;
  configuration config_;
};

::Napi::Object create_document(const ::Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();
  addon_state* state = env.GetInstanceData<addon_state>();

  return state->qljs_document_class.New({
      /*vscode=*/info[0],
  });
}

int to_int(::Napi::Value v) {
  return narrow_cast<int>(v.As<::Napi::Number>().Int64Value());
}

std::unique_ptr<addon_state> addon_state::create(::Napi::Env env) {
  return std::unique_ptr<addon_state>(new addon_state{
      .qljs_document_class = ::Napi::Persistent(qljs_document::init(env)),
  });
}

::Napi::Object initialize_addon(::Napi::Env env, ::Napi::Object exports) {
  std::unique_ptr<addon_state> state = addon_state::create(env);
  env.SetInstanceData<addon_state>(state.get());
  state.release();

  exports.Set("createDocument",
              ::Napi::Function::New(env, create_document, "createDocument"));
  return exports;
}
}
}

namespace {
::Napi::Object initialize_addon(::Napi::Env env, ::Napi::Object exports) {
  return quick_lint_js::initialize_addon(env, exports);
}
}

NODE_API_MODULE(quick_lint_js_vscode_node, initialize_addon)

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
