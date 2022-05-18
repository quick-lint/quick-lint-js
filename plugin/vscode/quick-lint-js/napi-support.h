// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_NAPI_SUPPORT_H
#define QUICK_LINT_JS_NAPI_SUPPORT_H

#include <napi.h>
#include <optional>
#include <string>
#include <vector>

namespace quick_lint_js {
inline int to_int(::Napi::Value v) {
  return narrow_cast<int>(v.As<::Napi::Number>().Int64Value());
}

inline std::uint64_t to_uint64(::Napi::Value v) {
  return narrow_cast<std::uint64_t>(v.As<::Napi::Number>().Int64Value());
}

inline std::optional<std::string> to_optional_string(::Napi::Value v) {
  if (v.IsNull()) {
    return std::nullopt;
  } else {
    return v.As<::Napi::String>().Utf8Value();
  }
}

inline std::string to_string(::Napi::Value v) {
  return v.As<::Napi::String>().Utf8Value();
}

inline void call_on_next_tick(::Napi::Env env, ::Napi::Function func,
                              ::napi_value self,
                              std::vector<::napi_value> args) {
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

template <class Func>
::Napi::Value promise_then(::Napi::Value promise, Func&& callback) {
  return promise.As<::Napi::Object>().Get("then").As<::Napi::Function>().Call(
      /*this=*/promise, {::Napi::Function::New(promise.Env(), callback)});
}

// The JavaScript Map class.
class js_map {
 public:
  // new Map()
  explicit js_map(::Napi::Env env)
      : map_(::Napi::Persistent(
            env.Global().Get("Map").As<::Napi::Function>().New({}))) {}

  // Map#get(key)
  ::Napi::Value get(::Napi::Value key) {
    return this->map_.Get("get").As<::Napi::Function>().Call(
        /*this=*/this->map_.Value(), {key});
  }

  // Map#set(key, value)
  void set(::Napi::Value key, ::Napi::Value value) {
    this->map_.Get("set").As<::Napi::Function>().Call(
        /*this=*/this->map_.Value(), {key, value});
  }

  // Map#delete(key)
  void erase(::Napi::Value key) {
    this->map_.Get("delete").As<::Napi::Function>().Call(
        /*this=*/this->map_.Value(), {key});
  }

  // Map#clear()
  void clear() {
    this->map_.Get("clear").As<::Napi::Function>().Call(
        /*this=*/this->map_.Value(), {});
  }

  // Map#forEach(callback)
  template <class Func>
  void for_each(Func&& callback) {
    ::Napi::Object iterator = this->map_.Get("values")
                                  .As<::Napi::Function>()
                                  .Call(/*this=*/this->map_.Value(), {})
                                  .As<::Napi::Object>();
    for (;;) {
      ::Napi::Object entry = iterator.Get("next")
                                 .As<::Napi::Function>()
                                 .Call(/*this=*/iterator, {})
                                 .As<::Napi::Object>();
      bool done = entry.Get("done").As<::Napi::Boolean>().Value();
      if (done) {
        break;
      }
      callback(entry.Get("value"));
    }
  }

 private:
  ::Napi::ObjectReference map_;
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
