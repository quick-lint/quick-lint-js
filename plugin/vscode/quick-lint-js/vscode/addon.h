// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VSCODE_ADDON_H
#define QUICK_LINT_JS_VSCODE_ADDON_H

#include <memory>
#include <quick-lint-js/vscode/napi-support.h>

namespace quick_lint_js {
// State global to a specific Node.js instance/thread.
class addon_state {
 public:
  static std::unique_ptr<addon_state> create(::Napi::Env env);

  ::Napi::FunctionReference qljs_logger_class;
  ::Napi::FunctionReference qljs_workspace_class;
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
