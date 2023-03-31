// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VSCODE_THREAD_SAFE_JS_FUNCTION_H
#define QUICK_LINT_JS_VSCODE_THREAD_SAFE_JS_FUNCTION_H

#include <napi.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/logging/log.h>

namespace quick_lint_js {
// Like ::Napi::TypedThreadSafeFunction, but with saner lifetime semantics.
template <void (*Func)(::Napi::Env, ::Napi::Object)>
class thread_safe_js_function {
 public:
  // thread_safe_js_function holds a weak reference to 'object'. If 'object'
  // is garbage-collected, then this thread_safe_js_function will not call
  // 'Func'.
  explicit thread_safe_js_function(::Napi::Env env, const char* resource_name,
                                   ::Napi::Object object)
      : function_(::Napi::TypedThreadSafeFunction<
                  /*ContextType=*/void, /*DataType=*/void,
                  /*Callback=*/call_func>::
                      New(
                          /*env=*/env,
                          /*callback=*/nullptr,
                          /*resource=*/::Napi::Object(),
                          /*resourceName=*/resource_name,
                          /*maxQueueSize=*/0,
                          /*initialThreadCount=*/1,
                          /*context=*/this->create_weak_reference(env, object),
                          /*finalizeCallback=*/finalize_weak_reference,
                          /*data=*/static_cast<void*>(nullptr))) {}

  // Like ::Napi::TypedThreadSafeFunction::BlockingCall.
  ::napi_status BlockingCall() { return this->function_.BlockingCall(); }

  // Like ::Napi::TypedThreadSafeFunction::Release.
  void Release() { this->function_.Release(); }

 private:
  static ::napi_ref create_weak_reference(::Napi::Env env,
                                          ::Napi::Object object) {
    ::napi_ref result;
    ::napi_status status = ::napi_create_reference(
        /*env=*/env,
        /*value=*/object,
        /*initial_refcount=*/0,
        /*result=*/&result);
    QLJS_ASSERT(status == ::napi_ok);
    return result;
  }

  static void finalize_weak_reference(::Napi::Env env,
                                      [[maybe_unused]] void* data,
                                      void* context) {
    // See NOTE[workspace-cleanup].
    ::napi_status status =
        ::napi_delete_reference(env, reinterpret_cast<::napi_ref>(context));
    QLJS_ASSERT(status == ::napi_ok);
  }

  static void call_func(::Napi::Env env, ::Napi::Function, void* context,
                        [[maybe_unused]] void* data) {
    if (!env) {
      return;
    }

    ::napi_value raw_object;
    ::napi_status status = ::napi_get_reference_value(
        env, reinterpret_cast<::napi_ref>(context), &raw_object);
    QLJS_ASSERT(status == ::napi_ok);
    ::Napi::Object object(env, raw_object);

    if (object.IsEmpty()) {
      // See NOTE[workspace-cleanup].
      QLJS_DEBUG_LOG(
          "not calling Func because object has been garbage-collected\n");
      return;
    }

    Func(env, object);
  }

  ::Napi::TypedThreadSafeFunction<void, void, call_func> function_;
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
