// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <memory>
#include <napi.h>
#include <optional>
#include <quick-lint-js/basic-configuration-filesystem.h>
#include <quick-lint-js/change-detecting-filesystem.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/diagnostic-formatter.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/event-loop.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/log.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/napi-support.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/pipe.h>
#include <quick-lint-js/vscode-error-reporter.h>
#include <quick-lint-js/vscode.h>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <vector>

namespace quick_lint_js {
namespace {
class qljs_document;
class qljs_workspace;

enum class document_type {
  config,
  lintable,
};

// State global to a specific Node.js instance/thread.
class addon_state {
 public:
  static std::unique_ptr<addon_state> create(::Napi::Env env);

  ::Napi::FunctionReference qljs_document_class;
  ::Napi::FunctionReference qljs_workspace_class;
};

class qljs_document : public ::Napi::ObjectWrap<qljs_document> {
 public:
  static ::Napi::Function init(::Napi::Env env) {
    return DefineClass(env, "QLJSDocument", {});
  }

  explicit qljs_document(const ::Napi::CallbackInfo& info)
      : ::Napi::ObjectWrap<qljs_document>(info),
        vscode_document_(
            ::Napi::Persistent(vscode_document(info[0].As<::Napi::Object>()))) {
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
  ::Napi::Array lint_javascript(::Napi::Env env, vscode_module* vscode) {
    QLJS_ASSERT(this->type_ == document_type::lintable);
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

  ::Napi::Array lint_config(::Napi::Env env, vscode_module* vscode,
                            loaded_config_file* loaded_config) {
    QLJS_ASSERT(this->type_ == document_type::config);
    vscode->load_non_persistent(env);

    lsp_locator locator(&loaded_config->file_content);
    vscode_error_reporter error_reporter(vscode, env, &locator);
    loaded_config->errors.copy_into(&error_reporter);
    return std::move(error_reporter).diagnostics();
  }

  document<lsp_locator> document_;
  document_type type_;
  ::Napi::Reference<vscode_document> vscode_document_;

  // Used only if type_ == document_type::lintable:
  configuration* config_;

  friend class qljs_workspace;
};

template <class UnderlyingFilesystem>
class thread_safe_configuration_filesystem : public configuration_filesystem {
 public:
  template <class... Args>
  explicit thread_safe_configuration_filesystem(Args&&... args)
      : underlying_fs_(std::forward<Args>(args)...) {}

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string& path) override {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.canonicalize_path(path);
  }

  result<padded_string, read_file_io_error, watch_io_error> read_file(
      const canonical_path& path) override {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.read_file(path);
  }

  auto get_inotify_fd() {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.get_inotify_fd();
  }

  template <class Event>
  auto handle_poll_event(Event&& event) {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.handle_poll_event(std::forward<Event>(event));
  }

  template <class Overlapped, class Number, class Error>
  auto handle_event(Overlapped overlapped, Number number_of_bytes_transferred,
                    Error error) {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.handle_event(
        overlapped, number_of_bytes_transferred, error);
  }

  void clear_watches() {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.clear_watches();
  }

 private:
  std::mutex lock_;
  UnderlyingFilesystem underlying_fs_;
};

// A configuration_filesystem which allows unsaved VS Code documents to appear
// as real files.
class vscode_configuration_filesystem : public configuration_filesystem {
 public:
  explicit vscode_configuration_filesystem(
      configuration_filesystem* underlying_fs)
      : underlying_fs_(underlying_fs) {}

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string& path) override {
    return this->underlying_fs_->canonicalize_path(path);
  }

  result<padded_string, read_file_io_error, watch_io_error> read_file(
      const canonical_path& path) override {
    qljs_document* doc = this->find_document(path.path());
    if (!doc) {
      QLJS_DEBUG_LOG("Reading file from disk: %s\n", path.c_str());
      return this->underlying_fs_->read_file(path);
    }
    QLJS_DEBUG_LOG("Reading file from open document: %s\n", path.c_str());
    return padded_string(doc->document_string().string_view());
  }

  void clear() { this->overlaid_documents_.clear(); }

  void overlay_document(const std::string& file_path, qljs_document* doc) {
    auto [_it, inserted] =
        this->overlaid_documents_.try_emplace(file_path, doc);
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
  configuration_filesystem* underlying_fs_;

  friend class qljs_workspace;  // HACK(strager)
};

// NOTE[workspace-cleanup]:
//
// If a qljs_workspace is deleted, its
// check_for_config_file_changes_on_js_thread_ TypedThreadSafeFunction might
// still have some calls in its queue.
// check_for_config_file_changes_from_thread() might use a qljs_workspace
// after it has been deleted. To prevent user-after-free errors, we create a
// JS weak reference to the qljs_workspace object. The weak reference is
// stored as check_for_config_file_changes_on_js_thread_'s Context.
// check_for_config_file_changes_from_thread does nothing if the
// qljs_workspace has been deleted.
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
        check_for_config_file_changes_on_js_thread_(
            ::Napi::TypedThreadSafeFunction<
                /*ContextType=*/void, /*DataType=*/void,
                /*Callback=*/check_for_config_file_changes_from_thread>::
                New(
                    /*env=*/info.Env(),
                    /*callback=*/nullptr,
                    /*resource=*/::Napi::Object(),
                    /*resourceName=*/"quick-lint-js-fs-thread",
                    /*maxQueueSize=*/0,
                    /*initialThreadCount=*/1,
                    /*context=*/this->create_weak_reference_to_self(info.Env()),
                    /*finalizeCallback=*/
                    [](Napi::Env env, [[maybe_unused]] void* data,
                       void* context) -> void {
                      // See NOTE[workspace-cleanup].
                      ::napi_status status = ::napi_delete_reference(
                          env, reinterpret_cast<::napi_ref>(context));
                      QLJS_ASSERT(status == ::napi_ok);
                    },
                    /*data=*/static_cast<void*>(nullptr))),
        qljs_documents_(info.Env()),
        vscode_diagnostic_collection_ref_(
            ::Napi::Persistent(info[1].As<::Napi::Object>())) {
    QLJS_DEBUG_LOG("Workspace %p: created\n", this);
    this->fs_change_detection_thread_ = std::thread(
        [this]() -> void { this->run_fs_change_detection_thread(); });
  }

  ~qljs_workspace() {
    // See NOTE[workspace-cleanup].
    this->dispose();
  }

  ::Napi::Value dispose(const ::Napi::CallbackInfo& info) {
    ::Napi::Env env = info.Env();
    this->dispose();
    return env.Undefined();
  }

  void dispose() {
    if (this->disposed_) {
      return;
    }

    QLJS_DEBUG_LOG("Workspace %p: disposing\n", this);
    this->fs_change_detection_event_loop_.stop();
    this->fs_change_detection_thread_.join();
    this->dispose_documents();

    this->disposed_ = true;
  }

  void dispose_documents() {
    this->qljs_documents_.for_each([this](::Napi::Value value) -> void {
      qljs_document* doc = qljs_document::Unwrap(value.As<::Napi::Object>());
      this->delete_diagnostics(doc);
    });
    this->qljs_documents_.clear();

    // TODO(strager): Create a configuration_loader::clear() function.
    for (auto& [path, _doc] : this->fs_.overlaid_documents_) {
      this->config_loader_.unwatch_file(path);
    }
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

    ::Napi::Object vscode_doc = info[0].As<::Napi::Object>();
    ::Napi::Value qljs_doc = this->qljs_documents_.get(vscode_doc);
    qljs_document* doc;
    if (qljs_doc.IsUndefined()) {
      vscode_document d(vscode_doc);
      doc = this->maybe_create_document(
          env,
          /*vscode_doc=*/d,
          /*text=*/to_string8_view(d.get_text().Utf8Value()));
      if (doc) {
        this->after_modification(env, doc);
      }
    } else {
      doc = qljs_document::Unwrap(qljs_doc.As<::Napi::Object>());
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

  qljs_document* maybe_create_document(::Napi::Env env,
                                       vscode_document vscode_doc,
                                       string8_view text) {
    addon_state* state = env.GetInstanceData<addon_state>();

    ::Napi::Object vscode_document_uri = vscode_doc.uri();
    std::optional<std::string> file_path = std::nullopt;
    if (to_string(vscode_document_uri.Get("scheme")) == "file") {
      file_path = to_string(vscode_document_uri.Get("fsPath"));
    }

    document_type type;
    if (vscode_doc.language_id() == "javascript") {
      type = document_type::lintable;
    } else if (file_path.has_value() &&
               this->config_loader_.is_config_file_path(*file_path)) {
      type = document_type::config;
    } else {
      return nullptr;
    }

    ::Napi::Object js_doc = state->qljs_document_class.New({vscode_doc});
    qljs_document* doc = qljs_document::Unwrap(js_doc);
    if (file_path.has_value()) {
      QLJS_DEBUG_LOG("Document %p: Opened document: %s\n", doc,
                     file_path->c_str());
    } else {
      QLJS_DEBUG_LOG("Document %p: Opened unnamed document\n", doc);
    }
    doc->type_ = type;
    doc->document_.set_text(text);
    if (file_path.has_value()) {
      this->fs_.overlay_document(*file_path, doc);
    }
    this->qljs_documents_.set(vscode_doc, js_doc);

    switch (type) {
    case document_type::lintable:
      doc->config_ = &this->default_config_;
      if (file_path.has_value()) {
        QLJS_DEBUG_LOG("Workspace %p: watching config for: %s\n", this,
                       file_path->c_str());
        auto loaded_config_result =
            this->config_loader_.watch_and_load_for_file(*file_path, doc);
        if (loaded_config_result.ok()) {
          loaded_config_file* loaded_config = *loaded_config_result;
          if (loaded_config) {
            if (!loaded_config->errors.empty()) {
              QLJS_ASSERT(loaded_config->config_path);
              std::string config_file_path(loaded_config->config_path->path());
              std::string message = "Problems found in the config file for " +
                                    *file_path + " (" + config_file_path + ").";
              this->vscode_.show_error_message(
                  env, message, {"Open config"},
                  [this, self = ::Napi::Persistent(this->Value()),
                   config_file_path](
                      ::Napi::Env env,
                      ::Napi::Value clicked_button_label) -> void {
                    bool popup_dismissed = clicked_button_label.IsUndefined();
                    if (popup_dismissed) {
                      return;
                    }
                    std::string clicked_button_label_string =
                        clicked_button_label.As<::Napi::String>().Utf8Value();
                    QLJS_ASSERT(clicked_button_label_string == "Open config");
                    this->vscode_.open_and_show_text_document_by_path(
                        env, config_file_path);
                  });
            }
            doc->config_ = &loaded_config->config;
          }
        } else {
          std::string message =
              "Failed to load configuration file for " + *file_path +
              ". Using default configuration.\nError details: " +
              loaded_config_result.error_to_string();
          call_on_next_tick(env,
                            this->vscode_.window_show_error_message.Value(),
                            /*this=*/this->vscode_.window_namespace.Value(),
                            {
                                ::Napi::String::New(env, message),
                            });
        }
      }
      break;

    case document_type::config:
      if (file_path.has_value()) {
        QLJS_DEBUG_LOG("Workspace %p: watching config file: %s\n", this,
                       file_path->c_str());
        auto loaded_config_result =
            this->config_loader_.watch_and_load_config_file(*file_path, doc);
        if (loaded_config_result.ok()) {
          this->vscode_.load_non_persistent(env);
          this->lint_config_and_publish_diagnostics(env, doc,
                                                    *loaded_config_result);
        } else {
          QLJS_UNIMPLEMENTED();
        }
      }
      break;
    }

    return doc;
  }

  void publish_diagnostics(qljs_document* doc, ::Napi::Value diagnostics) {
    this->vscode_diagnostic_collection_ref_.Get("set")
        .As<::Napi::Function>()
        .Call(/*this=*/this->vscode_diagnostic_collection_ref_.Value(),
              {
                  doc->vscode_document_.Value().uri(),
                  diagnostics,
              });
  }

  void delete_diagnostics(qljs_document* doc) {
    this->vscode_diagnostic_collection_ref_.Get("delete")
        .As<::Napi::Function>()
        .Call(/*this=*/this->vscode_diagnostic_collection_ref_.Value(),
              {
                  doc->vscode_document_.Value().uri(),
              });
  }

 private:
  void after_modification(::Napi::Env env, qljs_document* doc) {
    switch (doc->type_) {
    case document_type::config:
      this->check_for_config_file_changes(env);
      break;

    case document_type::lintable:
      this->lint_javascript_and_publish_diagnostics(env, doc);
      break;
    }
  }

  // This function is called on the main JS thread.
  static void check_for_config_file_changes_from_thread(
      ::Napi::Env env, ::Napi::Function, void* context,
      [[maybe_unused]] void* data) {
    if (!env) {
      return;
    }

    ::napi_value raw_workspace_object;
    ::napi_status status = ::napi_get_reference_value(
        env, reinterpret_cast<::napi_ref>(context), &raw_workspace_object);
    QLJS_ASSERT(status == ::napi_ok);
    ::Napi::Object workspace_object(env, raw_workspace_object);

    if (workspace_object.IsEmpty()) {
      // See NOTE[workspace-cleanup].
      QLJS_DEBUG_LOG(
          "check_for_config_file_changes_from_thread: workspace object has "
          "been garbage-collected\n");
      return;
    }
    qljs_workspace* workspace = qljs_workspace::Unwrap(workspace_object);
    if (workspace->disposed_) {
      QLJS_DEBUG_LOG(
          "Workspace %p: check_for_config_file_changes_from_thread: workspace "
          "object has been disposed\n",
          this);
      return;
    }
    workspace->check_for_config_file_changes(env);
  }

  void check_for_config_file_changes(::Napi::Env env) {
    std::vector<configuration_change> changes = this->config_loader_.refresh();
    for (const configuration_change& change : changes) {
      QLJS_DEBUG_LOG("Configuration changed for %s\n",
                     change.watched_path->c_str());
      qljs_document* doc = reinterpret_cast<qljs_document*>(change.token);
      switch (doc->type_) {
      case document_type::config:
        this->lint_config_and_publish_diagnostics(env, doc, change.config_file);
        break;

      case document_type::lintable:
        doc->config_ = change.config_file ? &change.config_file->config
                                          : &this->default_config_;
        this->lint_javascript_and_publish_diagnostics(env, doc);
        break;
      }
    }
  }

  void lint_javascript_and_publish_diagnostics(::Napi::Env env,
                                               qljs_document* doc) {
    this->publish_diagnostics(doc, doc->lint_javascript(env, &this->vscode_));
  }

  void lint_config_and_publish_diagnostics(::Napi::Env env, qljs_document* doc,
                                           loaded_config_file* loaded_config) {
    this->publish_diagnostics(
        doc, doc->lint_config(env, &this->vscode_, loaded_config));
  }

  // This function runs on a background thread.
  void run_fs_change_detection_thread() {
    QLJS_DEBUG_LOG("Workspace %p: starting run_fs_change_detection_thread\n",
                   this);
    this->fs_change_detection_event_loop_.run();
    this->check_for_config_file_changes_on_js_thread_.Release();
    QLJS_DEBUG_LOG("Workspace %p: stopping run_fs_change_detection_thread\n",
                   this);
  }

  ::napi_ref create_weak_reference_to_self(::Napi::Env env) {
    ::napi_ref result;
    ::napi_status status = ::napi_create_reference(
        /*env=*/env,
        /*value=*/this->Value(),
        /*initial_refcount=*/0,
        /*result=*/&result);
    QLJS_ASSERT(status == ::napi_ok);
    return result;
  }

  class fs_change_detection_event_loop
      : public event_loop<fs_change_detection_event_loop> {
   public:
    explicit fs_change_detection_event_loop(qljs_workspace* workspace)
        :
#if QLJS_HAVE_KQUEUE
          fs_(this->kqueue_fd(),
              reinterpret_cast<void*>(event_udata_fs_changed)),
#elif QLJS_HAVE_INOTIFY
          fs_(),
#elif defined(_WIN32)
          fs_(this->io_completion_port(), completion_key_fs_changed),
#else
#error "Unsupported platform"
#endif
          workspace_(workspace) {
#if QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING
      this->stop_pipe_.reader.set_pipe_non_blocking();
#endif
    }

    void stop() {
      this->stop_pipe_.writer.close();
#if defined(_WIN32)
      this->fs_.clear_watches();
#endif
    }

    platform_file_ref get_readable_pipe() const {
      return this->stop_pipe_.reader.ref();
    }

    void append(string8_view) { QLJS_UNREACHABLE(); }

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
    std::optional<posix_fd_file_ref> get_pipe_write_fd() {
      return std::nullopt;
    }
#endif

#if QLJS_HAVE_KQUEUE
    void on_pipe_write_event(const struct ::kevent&) { QLJS_UNREACHABLE(); }
#elif QLJS_HAVE_POLL
    void on_pipe_write_event(const ::pollfd&) { QLJS_UNREACHABLE(); }
#endif

    void filesystem_changed() {
      QLJS_DEBUG_LOG("Workspace %p: filesystem changed detected\n",
                     this->workspace_);
      ::napi_status status =
          this->workspace_->check_for_config_file_changes_on_js_thread_
              .BlockingCall();
      // BlockingCall should not return napi_closing because we never call
      // Abort.
      QLJS_ASSERT(status == ::napi_ok);
    }

#if QLJS_HAVE_KQUEUE
    void on_fs_changed_kevents() { this->filesystem_changed(); }
#endif

#if QLJS_HAVE_INOTIFY
    std::optional<posix_fd_file_ref> get_inotify_fd() {
      return this->fs_.get_inotify_fd();
    }

    void on_fs_changed_event(const ::pollfd& event) {
      this->fs_.handle_poll_event(event);
      this->filesystem_changed();
    }
#endif

#if defined(_WIN32)
    void on_fs_changed_event(::OVERLAPPED* overlapped,
                             ::DWORD number_of_bytes_transferred,
                             ::DWORD error) {
      bool fs_changed = this->fs_.handle_event(
          overlapped, number_of_bytes_transferred, error);
      if (fs_changed) {
        this->filesystem_changed();
      }
    }
#endif

    configuration_filesystem* fs() noexcept { return &this->fs_; }

   private:
    using underlying_fs_type =
#if QLJS_HAVE_KQUEUE
        change_detecting_filesystem_kqueue
#elif QLJS_HAVE_INOTIFY
        change_detecting_filesystem_inotify
#elif defined(_WIN32)
        change_detecting_filesystem_win32;
#else
#error "Unsupported platform"
#endif
        ;

    thread_safe_configuration_filesystem<underlying_fs_type> fs_;
    pipe_fds stop_pipe_ = make_pipe();
    qljs_workspace* workspace_;
  };

  bool disposed_ = false;
  vscode_module vscode_;
  fs_change_detection_event_loop fs_change_detection_event_loop_{this};
  vscode_configuration_filesystem fs_{fs_change_detection_event_loop_.fs()};
  configuration_loader config_loader_{&fs_};
  configuration default_config_;
  std::thread fs_change_detection_thread_;

  // See NOTE[workspace-cleanup].
  ::Napi::TypedThreadSafeFunction<void, void,
                                  check_for_config_file_changes_from_thread>
      check_for_config_file_changes_on_js_thread_;

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
