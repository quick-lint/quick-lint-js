// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <memory>
#include <napi.h>
#include <optional>
#include <quick-lint-js/basic-configuration-filesystem.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/diagnostic-formatter.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace quick_lint_js {
namespace {
int to_int(::Napi::Value);
std::optional<std::string> to_optional_string(::Napi::Value);
void call_on_next_tick(::Napi::Env, ::Napi::Function, ::napi_value self,
                       std::vector<::napi_value> args);

class qljs_document;
class qljs_workspace;

// State global to a specific Node.js instance/thread.
class addon_state {
 public:
  static std::unique_ptr<addon_state> create(::Napi::Env env);

  ::Napi::FunctionReference qljs_document_class;
  ::Napi::FunctionReference qljs_workspace_class;
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
    diag.Set("code", ::Napi::String::New(this->env_, code.data(), code.size()));
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

// A configuration_filesystem which allows unsaved VS Code documents to appear
// as real files.
class vscode_configuration_filesystem : public configuration_filesystem {
 public:
  explicit vscode_configuration_filesystem(qljs_workspace* workspace)
      : workspace_(workspace) {}

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string&) override;
  result<padded_string, read_file_io_error, watch_io_error> read_file(
      const canonical_path&) override;

 private:
  qljs_workspace* workspace_;
  basic_configuration_filesystem underlying_fs_;
};

class qljs_workspace : public ::Napi::ObjectWrap<qljs_workspace> {
 public:
  static ::Napi::Function init(::Napi::Env env) {
    return DefineClass(
        env, "QLJSWorkspace",
        {
            InstanceMethod<&qljs_workspace::create_document>("createDocument"),
            InstanceMethod<&qljs_workspace::dispose>("dispose"),
            InstanceMethod<&qljs_workspace::is_config_file_path>(
                "isConfigFilePath"),
        });
  }

  explicit qljs_workspace(const ::Napi::CallbackInfo& info)
      : ::Napi::ObjectWrap<qljs_workspace>(info),
        vscode_(info[0].As<::Napi::Object>()),
        configuration_load_io_error_callback_(
            ::Napi::Persistent(info[1].As<::Napi::Function>())),
        fs_(this) {}

  ::Napi::Value dispose(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();

    // TODO(strager): Reduce memory usage.

    return env.Undefined();
  }

  ::Napi::Value is_config_file_path(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();
    std::string path = info[0].As<::Napi::String>().Utf8Value();
    return ::Napi::Boolean::New(env,
                                this->config_loader_.is_config_file_path(path));
  }

  ::Napi::Value create_document(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();
    addon_state* state = env.GetInstanceData<addon_state>();

    ::Napi::Object doc = state->qljs_document_class.New({
        /*workspace=*/info.This(),
        /*file_path=*/info[0],
    });
    if (std::optional<std::string> file_path = to_optional_string(info[0])) {
      auto [_it, inserted] =
          this->documents_.try_emplace(*file_path, ::Napi::Persistent(doc));
      QLJS_ASSERT(inserted);
    }
    return doc;
  }

  qljs_document* find_document(std::string_view path);

 private:
  vscode_module vscode_;
  ::Napi::FunctionReference configuration_load_io_error_callback_;
  vscode_configuration_filesystem fs_;
  configuration_loader config_loader_{&fs_};
  configuration default_config_;

  // qljs_document-s opened for editing.
  std::unordered_map<std::string, ::Napi::ObjectReference> documents_;

  friend class qljs_document;
};

class qljs_document : public ::Napi::ObjectWrap<qljs_document> {
 public:
  static ::Napi::Function init(::Napi::Env env) {
    return DefineClass(
        env, "QLJSDocument",
        {
            InstanceMethod<&qljs_document::dispose>("dispose"),
            InstanceMethod<&qljs_document::lint>("lint"),
            InstanceMethod<&qljs_document::replace_text>("replaceText"),
            InstanceMethod<&qljs_document::set_text>("setText"),
        });
  }

  explicit qljs_document(const ::Napi::CallbackInfo& info)
      : ::Napi::ObjectWrap<qljs_document>(info),
        workspace_ref_(::Napi::Persistent(info[0].As<::Napi::Object>())),
        workspace_(qljs_workspace::Unwrap(workspace_ref_.Value())) {
    ::Napi::Env env = info.Env();

    std::optional<std::string> file_path = to_optional_string(info[1]);
    this->config_ = &this->workspace_->default_config_;
    if (file_path.has_value() &&
        !this->workspace_->config_loader_.is_config_file_path(*file_path)) {
      auto loaded_config_result =
          this->workspace_->config_loader_.watch_and_load_for_file(*file_path,
                                                                   this);
      if (loaded_config_result.ok()) {
        loaded_config_file* loaded_config = *loaded_config_result;
        if (loaded_config) {
          // TODO(strager): Show config parse errors.
          this->config_ = &loaded_config->config;
        }
      } else {
        call_on_next_tick(
            env,
            this->workspace_->configuration_load_io_error_callback_.Value(),
            /*this=*/env.Null(),
            {
                /*qljsDocument=*/info.This(),
                /*errorMessage=*/
                ::Napi::String::New(env,
                                    loaded_config_result.error_to_string()),
            });
      }
    }
  }

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
    vscode_module* vscode = &this->workspace_->vscode_;
    vscode->load_non_persistent(env);

    vscode_error_reporter error_reporter(vscode, env,
                                         &this->document_.locator());
    parser p(this->document_.string(), &error_reporter);
    linter l(&error_reporter, &this->config_->globals());
    bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(l);
    if (!ok) {
      // TODO(strager): Show a pop-up message explaining that the parser
      // crashed.
    }
    return error_reporter.diagnostics();
  }

  padded_string_view document_string() noexcept {
    return this->document_.string();
  }

 private:
  document<lsp_locator> document_;
  configuration* config_;
  ::Napi::ObjectReference workspace_ref_;
  qljs_workspace* workspace_;
};

result<canonical_path_result, canonicalize_path_io_error>
vscode_configuration_filesystem::canonicalize_path(const std::string& path) {
  return this->underlying_fs_.canonicalize_path(path);
}

result<padded_string, read_file_io_error, watch_io_error>
vscode_configuration_filesystem::read_file(const canonical_path& path) {
  qljs_document* doc = this->workspace_->find_document(path.path());
  if (!doc) {
    return this->underlying_fs_.read_file(path);
  }
  return padded_string(doc->document_string().string_view());
}

qljs_document* qljs_workspace::find_document(std::string_view path) {
#if QLJS_HAVE_STD_TRANSPARENT_KEYS
  std::string_view key = path;
#else
  std::string key(path);
#endif
  auto doc_it = this->documents_.find(key);
  if (doc_it == this->documents_.end()) {
    return nullptr;
  }
  return qljs_document::Unwrap(doc_it->second.Value());
}
::Napi::Object create_workspace(const ::Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();
  addon_state* state = env.GetInstanceData<addon_state>();

  ::Napi::Object options = info[0].As<::Napi::Object>();
  return state->qljs_workspace_class.New({
      /*vscode=*/options.Get("vscode"),
      /*on_configuration_load_io_error=*/
      options.Get("onConfigurationLoadIOError"),
  });
}

int to_int(::Napi::Value v) {
  return narrow_cast<int>(v.As<::Napi::Number>().Int64Value());
}

std::optional<std::string> to_optional_string(::Napi::Value v) {
  if (v.IsNull()) {
    return std::nullopt;
  } else {
    return v.As<::Napi::String>().Utf8Value();
  }
}

void call_on_next_tick(::Napi::Env env, ::Napi::Function func,
                       ::napi_value self, std::vector<::napi_value> args) {
  args.insert(args.begin(), self);
  ::Napi::Value next_tick_callback =
      func.Get("bind").As<::Napi::Function>().Call(func, std::move(args));
  env.Global()
      .Get("process")
      .As<::Napi::Object>()
      .Get("nextTick")
      .As<::Napi::Function>()
      .Call({next_tick_callback});
}

std::unique_ptr<addon_state> addon_state::create(::Napi::Env env) {
  return std::unique_ptr<addon_state>(new addon_state{
      .qljs_document_class = ::Napi::Persistent(qljs_document::init(env)),
      .qljs_workspace_class = ::Napi::Persistent(qljs_workspace::init(env)),
  });
}

::Napi::Object initialize_addon(::Napi::Env env, ::Napi::Object exports) {
  std::unique_ptr<addon_state> state = addon_state::create(env);
  env.SetInstanceData<addon_state>(state.get());
  state.release();

  exports.Set("createWorkspace",
              ::Napi::Function::New(env, create_workspace, "createWorkspace"));
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
