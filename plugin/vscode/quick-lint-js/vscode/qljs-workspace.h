// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

// TODO(strager): Trim includes.
#include <cstdio>
#include <memory>
#include <napi.h>
#include <optional>
#include <quick-lint-js/configuration/basic-configuration-filesystem.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/event-loop.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/logger.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/vscode/napi-support.h>
#include <quick-lint-js/vscode/qljs-document.h>
#include <quick-lint-js/vscode/qljs-logger.h>
#include <quick-lint-js/vscode/thread-safe-js-function.h>
#include <quick-lint-js/vscode/ui.h>
#include <quick-lint-js/vscode/vscode-configuration-filesystem.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode-tracer.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
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
class QLJS_Workspace : public ::Napi::ObjectWrap<QLJS_Workspace> {
 public:
  static ::Napi::Function init(::Napi::Env env);

  explicit QLJS_Workspace(const ::Napi::CallbackInfo& info);

  // Enable or disable logging depending on the user's configuration.
  void update_logging(::Napi::Env env);

  // Enable logging if logging is disabled.
  void enable_logging(::Napi::Env env);

  // Disable logging if logging is enabled.
  void disable_logging();

  ~QLJS_Workspace();

  ::Napi::Value dispose(const ::Napi::CallbackInfo& info);

  void dispose();

  void dispose_documents();

  ::Napi::Value close_document(const ::Napi::CallbackInfo& info);

  ::Napi::Value configuration_changed(const ::Napi::CallbackInfo& info);

  ::Napi::Value editor_visibility_changed(const ::Napi::CallbackInfo& info);

  ::Napi::Value document_changed(const ::Napi::CallbackInfo& info);

  ::Napi::Value document_saved(const ::Napi::CallbackInfo& info);

  QLJS_Document_Base* maybe_create_document(::Napi::Env env,
                                            VSCode_Document vscode_doc,
                                            String8_View text);

  VSCode_Diagnostic_Collection diagnostic_collection() const;

  void delete_diagnostics(QLJS_Document_Base* doc);

  void report_pending_watch_io_errors(::Napi::Env env);

 private:
  // This function is called on the main JS thread.
  static void check_for_config_file_changes_from_thread(
      ::Napi::Env env, ::Napi::Object workspace_object);

 public:
  void check_for_config_file_changes(::Napi::Env env);

 private:
  // This function runs on a background thread.
  void run_fs_change_detection_thread();

  class FS_Change_Detection_Event_Loop
      : public Event_Loop<FS_Change_Detection_Event_Loop> {
   public:
    explicit FS_Change_Detection_Event_Loop(QLJS_Workspace* workspace)
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

    Platform_File_Ref get_readable_pipe() const {
      return this->stop_pipe_.reader.ref();
    }

    [[noreturn]] void append(String8_View) { QLJS_UNREACHABLE(); }

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
    std::optional<POSIX_FD_File_Ref> get_pipe_write_fd() {
      return std::nullopt;
    }
#endif

#if QLJS_HAVE_KQUEUE
    [[noreturn]] void on_pipe_write_event(const struct ::kevent&) {
      QLJS_UNREACHABLE();
    }
#elif QLJS_HAVE_POLL
    [[noreturn]] void on_pipe_write_event(const ::pollfd&) {
      QLJS_UNREACHABLE();
    }
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
    void on_fs_changed_kevent(const struct ::kevent& event) {
      this->fs_.handle_kqueue_event(event);
    }

    void on_fs_changed_kevents() { this->filesystem_changed(); }
#endif

#if QLJS_HAVE_INOTIFY
    std::optional<POSIX_FD_File_Ref> get_inotify_fd() {
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

    using Underlying_FS_Type =
#if QLJS_HAVE_KQUEUE
        Change_Detecting_Filesystem_Kqueue
#elif QLJS_HAVE_INOTIFY
        Change_Detecting_Filesystem_Inotify
#elif defined(_WIN32)
        Change_Detecting_Filesystem_Win32;
#else
#error "Unsupported platform"
#endif
        ;

    Thread_Safe_Configuration_Filesystem<Underlying_FS_Type>* fs() {
      return &this->fs_;
    }

   private:
    Thread_Safe_Configuration_Filesystem<Underlying_FS_Type> fs_;
    Pipe_FDs stop_pipe_ = make_pipe();
    QLJS_Workspace* workspace_;
  };

  bool disposed_ = false;
  VSCode_Tracer tracer_;
  VSCode_Module vscode_;
  FS_Change_Detection_Event_Loop fs_change_detection_event_loop_{this};
  VSCode_Configuration_Filesystem fs_{fs_change_detection_event_loop_.fs()};
  Configuration_Loader config_loader_{&fs_};
  Configuration default_config_;
  Thread fs_change_detection_thread_;
  bool did_report_watch_io_error_ = false;

  // See NOTE[workspace-cleanup].
  Thread_Safe_JS_Function<check_for_config_file_changes_from_thread>
      check_for_config_file_changes_on_js_thread_;

  // Mapping from vscode.Document to wrapped qljs_document_base.
  JS_Map qljs_documents_;
  ::Napi::ObjectReference vscode_diagnostic_collection_ref_;

  ::Napi::ObjectReference logger_;  // An optional qljs_logger.
  bool logger_enabled_ = false;

  VSCode_UI ui_;

  friend class QLJS_Config_Document;
  friend class QLJS_Lintable_Document;
  friend class VSCode_UI;
};

::Napi::Object create_workspace(const ::Napi::CallbackInfo& info);
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
