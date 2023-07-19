// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <memory>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/vscode/addon.h>
#include <quick-lint-js/vscode/napi-support.h>
#include <quick-lint-js/vscode/qljs-logger.h>
#include <quick-lint-js/vscode/qljs-workspace.h>

namespace quick_lint_js {
std::unique_ptr<Addon_State> Addon_State::create(::Napi::Env env) {
  std::unique_ptr<Addon_State> state(new Addon_State{
      .qljs_logger_class = ::Napi::Persistent(QLJS_Logger::init(env)),
      .qljs_workspace_class = ::Napi::Persistent(QLJS_Workspace::init(env)),
  });
  Trace_Flusher* tracer = Trace_Flusher::instance();
  tracer->register_current_thread();
  tracer->start_flushing_thread();
  return state;
}

namespace {
#if QLJS_HAVE_INOTIFY
::Napi::Value mock_inotify_errors(const ::Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  int init_error = narrow_cast<int>(info[0].As<::Napi::Number>().Int32Value());
  int add_watch_error =
      narrow_cast<int>(info[1].As<::Napi::Number>().Int32Value());
  mock_inotify_force_init_error = init_error;
  mock_inotify_force_add_watch_error = add_watch_error;

  return env.Undefined();
}
#endif

#if QLJS_HAVE_KQUEUE
::Napi::Value mock_kqueue_errors(const ::Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();

  int directory_open_error =
      narrow_cast<int>(info[0].As<::Napi::Number>().Int32Value());
  mock_kqueue_force_directory_open_error = directory_open_error;

  return env.Undefined();
}
#endif

// Work around a bug in MinGW's dlltool when calling certain
// N-API functions.
// https://sourceware.org/bugzilla/show_bug.cgi?id=29189
void work_around_dlltool_bug(::napi_env env) {
  // Call all napi_ functions with any double parameters.
  ::napi_value value;
  ::napi_create_double(env, 0.0, &value);
  ::napi_create_date(env, 0.0, &value);
}

::Napi::Object initialize_addon(::Napi::Env env, ::Napi::Object exports) {
  work_around_dlltool_bug(env);

  std::unique_ptr<Addon_State> state = Addon_State::create(env);
  env.SetInstanceData<Addon_State>(state.get());
  state.release();

  exports.Set("createWorkspace",
              ::Napi::Function::New(env, create_workspace, "createWorkspace"));
#if QLJS_HAVE_INOTIFY
  exports.Set(
      "mockInotifyErrors",
      ::Napi::Function::New(env, mock_inotify_errors, "mockInotifyErrors"));
#endif
#if QLJS_HAVE_KQUEUE
  exports.Set("mockKqueueErrors", ::Napi::Function::New(env, mock_kqueue_errors,
                                                        "mockKqueueErrors"));
#endif
  return exports;
}
}
}

namespace {
::Napi::Object initialize_addon(::Napi::Env env, ::Napi::Object exports) {
  return quick_lint_js::initialize_addon(env, exports);
}
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")
NODE_API_MODULE(quick_lint_js_vscode_node, initialize_addon)
QLJS_WARNING_POP

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
