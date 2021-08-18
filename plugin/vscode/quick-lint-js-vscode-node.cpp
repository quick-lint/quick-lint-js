// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
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
#include <quick-lint-js/napi-support.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

// Define this macro to a non-empty string to log to the specified file:
// #define QLJS_DEBUG_LOGGING_FILE "/tmp/qljs.log"

#if defined(QLJS_DEBUG_LOGGING_FILE)
#define QLJS_DEBUG_LOG(...)                          \
  do {                                               \
    ::quick_lint_js::debug_log_to_file(__VA_ARGS__); \
  } while (false)
#else
#define QLJS_DEBUG_LOG(...) \
  do {                      \
  } while (false)
#endif

namespace quick_lint_js {
namespace {
#if defined(QLJS_DEBUG_LOGGING_FILE)
template <class... Args>
void debug_log_to_file(const char* format, Args&&...);
#endif

class qljs_document;
class qljs_workspace;

enum class document_type {
  config,
  lintable,
  unknown,
};

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
            module.Get("DiagnosticSeverity").As<::Napi::Object>())),
        window_namespace(
            ::Napi::Persistent(module.Get("window").As<::Napi::Object>())),
        window_show_error_message(
            ::Napi::Persistent(this->window_namespace.Get("showErrorMessage")
                                   .As<::Napi::Function>())) {}

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

  // vscode.window
  ::Napi::ObjectReference window_namespace;
  // vscode.window.showErrorMessage
  ::Napi::FunctionReference window_show_error_message;
};

// A wrapper around a vscode.Document.
class vscode_document {
 public:
  explicit vscode_document(::Napi::ObjectReference&& doc)
      : doc_(std::move(doc)) {}

  // vscode.Document#getText()
  ::Napi::String get_text() {
    return this->doc_.Get("getText")
        .As<::Napi::Function>()
        .Call(this->doc_.Value(), {})
        .As<::Napi::String>();
  }

  // vscode.Document#uri
  ::Napi::Object uri() { return this->doc_.Get("uri").As<::Napi::Object>(); }

 private:
  ::Napi::ObjectReference doc_;
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

class qljs_document : public ::Napi::ObjectWrap<qljs_document> {
 public:
  static ::Napi::Function init(::Napi::Env env) {
    return DefineClass(env, "QLJSDocument", {});
  }

  explicit qljs_document(const ::Napi::CallbackInfo& info)
      : ::Napi::ObjectWrap<qljs_document>(info),
        vscode_document_(::Napi::Persistent(info[0].As<::Napi::Object>())) {}

  void init(::Napi::Env env, configuration* default_config,
            configuration_loader& config_loader, vscode_module* vscode,
            const std::optional<std::string>& file_path, document_type type) {
    this->type_ = type;
    this->config_ = default_config;
    if (file_path.has_value() && type == document_type::lintable) {
      auto loaded_config_result =
          config_loader.watch_and_load_for_file(*file_path, this);
      if (loaded_config_result.ok()) {
        loaded_config_file* loaded_config = *loaded_config_result;
        if (loaded_config) {
          // TODO(strager): Show config parse errors.
          this->config_ = &loaded_config->config;
        }
      } else {
        std::string message =
            "Failed to load configuration file for " + *file_path +
            ". Using default configuration.\nError details: " +
            loaded_config_result.error_to_string();
        call_on_next_tick(env, vscode->window_show_error_message.Value(),
                          /*this=*/vscode->window_namespace.Value(),
                          {
                              ::Napi::String::New(env, message),
                          });
      }
    }
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

 private:
  ::Napi::Array lint(::Napi::Env env, vscode_module* vscode) {
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
    return std::move(error_reporter).diagnostics();
  }

  document<lsp_locator> document_;
  document_type type_;
  configuration* config_;
  vscode_document vscode_document_;

  friend class qljs_workspace;
};

// A configuration_filesystem which allows unsaved VS Code documents to appear
// as real files.
class vscode_configuration_filesystem : public configuration_filesystem {
 public:
  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string& path) override {
    return this->underlying_fs_.canonicalize_path(path);
  }

  result<padded_string, read_file_io_error, watch_io_error> read_file(
      const canonical_path& path) override {
    qljs_document* doc = this->find_document(path.path());
    if (!doc) {
      QLJS_DEBUG_LOG("Reading file from disk: %s\n", path.c_str());
      return this->underlying_fs_.read_file(path);
    }
    QLJS_DEBUG_LOG("Reading file from open document: %s\n", path.c_str());
    return padded_string(doc->document_string().string_view());
  }

  void clear() { this->overlaid_documents_.clear(); }

  void overlay_document(std::string&& file_path, qljs_document* doc) {
    auto [_it, inserted] =
        this->overlaid_documents_.try_emplace(std::move(file_path), doc);
    QLJS_ASSERT(inserted);
  }

  void forget_document(qljs_document* doc) {
    auto doc_it = std::find_if(this->overlaid_documents_.begin(),
                               this->overlaid_documents_.end(),
                               [&](auto& pair) { return pair.second == doc; });
    if (doc_it == this->overlaid_documents_.end()) {
      // The document could be missing for any of the following reasons:
      // * Our extension was loaded after the document was opened.
      //   (TODO(strager): We should fix this.)
      // * The closed document was unnamed.
    } else {
      this->overlaid_documents_.erase(doc_it);
    }
  }

 private:
  qljs_document* find_document(std::string_view path) {
#if QLJS_HAVE_STD_TRANSPARENT_KEYS
    std::string_view key = path;
#else
    std::string key(path);
#endif
    auto doc_it = this->overlaid_documents_.find(key);
    if (doc_it == this->overlaid_documents_.end()) {
      return nullptr;
    }
    return doc_it->second;
  }

  std::unordered_map<std::string, qljs_document*> overlaid_documents_;
  basic_configuration_filesystem underlying_fs_;
};

class qljs_workspace : public ::Napi::ObjectWrap<qljs_workspace> {
 public:
  static ::Napi::Function init(::Napi::Env env) {
    return DefineClass(
        env, "QLJSWorkspace",
        {
            InstanceMethod<&qljs_workspace::close_document>("closeDocument"),
            InstanceMethod<&qljs_workspace::dispose>("dispose"),
            InstanceMethod<&qljs_workspace::document_changed>(
                "documentChanged"),
            InstanceMethod<&qljs_workspace::editor_visibility_changed>(
                "editorVisibilityChanged"),
        });
  }

  explicit qljs_workspace(const ::Napi::CallbackInfo& info)
      : ::Napi::ObjectWrap<qljs_workspace>(info),
        vscode_(info[0].As<::Napi::Object>()),
        qljs_documents_(info.Env()),
        vscode_diagnostic_collection_ref_(
            ::Napi::Persistent(info[1].As<::Napi::Object>())) {}

  ::Napi::Value dispose(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();

    this->dispose_documents();
    // TODO(strager): Reduce memory usage.

    return env.Undefined();
  }

  void dispose_documents() {
    this->qljs_documents_.for_each([this](::Napi::Value value) -> void {
      qljs_document* doc = qljs_document::Unwrap(value.As<::Napi::Object>());
      this->delete_diagnostics(doc);
    });
    this->qljs_documents_.clear();
    this->fs_.clear();
  }

  ::Napi::Value close_document(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();

    ::Napi::Value vscode_document = info[0];
    ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_document);
    if (!qljs_doc.IsUndefined()) {
      qljs_document* doc = qljs_document::Unwrap(qljs_doc.As<::Napi::Object>());
      QLJS_DEBUG_LOG("Document %p: Closing\n", doc);
      this->delete_diagnostics(doc);
      this->qljs_documents_.erase(vscode_document);
      this->fs_.forget_document(doc);
    }

    return env.Undefined();
  }

  ::Napi::Value editor_visibility_changed(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();

    ::Napi::Object vscode_document = info[0].As<::Napi::Object>();
    ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_document);
    if (qljs_doc.IsUndefined()) {
      document_type type = this->classify_document(vscode_document);
      switch (type) {
      case document_type::config:
      case document_type::lintable:
        qljs_doc = this->create_document(env,
                                         /*vscode_document=*/vscode_document,
                                         /*type=*/
                                         type);
        this->qljs_documents_.set(vscode_document, qljs_doc);
        break;

      case document_type::unknown:
        // Ignore.
        break;
      }
    }

    if (!qljs_doc.IsUndefined()) {
      qljs_document* doc = qljs_document::Unwrap(qljs_doc.As<::Napi::Object>());
      ::Napi::String text = doc->vscode_document_.get_text();
      doc->document_.set_text(to_string8_view(text.Utf8Value()));

      this->after_modification(env, doc);
    }

    return env.Undefined();
  }

  ::Napi::Value document_changed(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();

    ::Napi::Value vscode_document = info[0];
    ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_document);
    if (!qljs_doc.IsUndefined()) {
      qljs_document* doc = qljs_document::Unwrap(qljs_doc.As<::Napi::Object>());
      ::Napi::Array changes = info[1].As<::Napi::Array>();
      doc->replace_text(changes);
      this->after_modification(env, doc);
    }

    return env.Undefined();
  }

  ::Napi::Value create_document(::Napi::Env env, ::Napi::Object vscode_document,
                                document_type type) {
    addon_state* state = env.GetInstanceData<addon_state>();

    ::Napi::Object vscode_document_uri =
        vscode_document.Get("uri").As<::Napi::Object>();
    std::optional<std::string> file_path = std::nullopt;
    if (to_string(vscode_document_uri.Get("scheme")) == "file") {
      file_path = to_string(vscode_document_uri.Get("fsPath"));
    }
    ::Napi::Object js_doc = state->qljs_document_class.New({vscode_document});
    qljs_document* doc = qljs_document::Unwrap(js_doc);
    if (file_path.has_value()) {
      QLJS_DEBUG_LOG("Document %p: Opened document: %s\n", doc,
                     file_path->c_str());
    } else {
      QLJS_DEBUG_LOG("Document %p: Opened unnamed document\n", doc,
                     file_path->c_str());
    }
    doc->init(env, &this->default_config_, this->config_loader_, &this->vscode_,
              file_path, type);
    if (file_path.has_value()) {
      this->fs_.overlay_document(std::move(*file_path), doc);
    }
    return js_doc;
  }

  void publish_diagnostics(qljs_document* doc, ::Napi::Value diagnostics) {
    this->vscode_diagnostic_collection_ref_.Get("set")
        .As<::Napi::Function>()
        .Call(/*this=*/this->vscode_diagnostic_collection_ref_.Value(),
              {
                  doc->vscode_document_.uri(),
                  diagnostics,
              });
  }

  void delete_diagnostics(qljs_document* doc) {
    this->vscode_diagnostic_collection_ref_.Get("delete")
        .As<::Napi::Function>()
        .Call(/*this=*/this->vscode_diagnostic_collection_ref_.Value(),
              {
                  doc->vscode_document_.uri(),
              });
  }

 private:
  document_type classify_document(::Napi::Object vscode_document) {
    if (to_string(vscode_document.Get("languageId")) == "javascript") {
      return document_type::lintable;
    }
    ::Napi::Object uri = vscode_document.Get("uri").As<::Napi::Object>();
    if (to_string(uri.Get("scheme")) == "file" &&
        this->config_loader_.is_config_file_path(
            uri.Get("fsPath").As<::Napi::String>().Utf8Value())) {
      return document_type::config;
    }
    return document_type::unknown;
  }

  void after_modification(::Napi::Env env, qljs_document* doc) {
    switch (doc->type_) {
    case document_type::config:
      break;

    case document_type::lintable:
      this->publish_diagnostics(doc, doc->lint(env, &this->vscode_));
      break;

    case document_type::unknown:
      QLJS_UNREACHABLE();
      break;
    }
  }

  vscode_module vscode_;
  vscode_configuration_filesystem fs_;
  configuration_loader config_loader_{&fs_};
  configuration default_config_;
  // Mapping from vscode.Document to qljs.QLJSDocument (qljs_document).
  js_map qljs_documents_;
  ::Napi::ObjectReference vscode_diagnostic_collection_ref_;
};

::Napi::Object create_workspace(const ::Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();
  addon_state* state = env.GetInstanceData<addon_state>();

  ::Napi::Object options = info[0].As<::Napi::Object>();
  return state->qljs_workspace_class.New({
      /*vscode=*/options.Get("vscode"),
      /*vscode_diagnostic_collection=*/
      options.Get("vscodeDiagnosticCollection"),
  });
}

#if defined(QLJS_DEBUG_LOGGING_FILE)
template <class... Args>
void debug_log_to_file(const char* format, Args&&... args) {
  static FILE* file = std::fopen(QLJS_DEBUG_LOGGING_FILE, "a");
  if (file) {
#if QLJS_HAVE_GETPID
    std::fprintf(file, "[%d] ", ::getpid());
#endif
    std::fprintf(file, format, args...);
    std::fflush(file);
  }
}
#endif

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
