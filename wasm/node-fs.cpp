// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <napi.h>

namespace {
::Napi::String hello(const ::Napi::CallbackInfo& info) {
  ::Napi::Env env = info.Env();
  return ::Napi::String::New(env, "hi");
}

::Napi::Object initialize_addon(::Napi::Env env, ::Napi::Object exports) {
  exports.Set(::Napi::String::New(env, "hello"),
              ::Napi::Function::New(env, hello));
  return exports;
}
}

NODE_API_MODULE(quick_lint_js_node_fs, initialize_addon)

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
