// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/lint.h>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
const global_declared_variable_set& configuration::globals() noexcept {
  if (this->add_global_group_ecmascript_) {
    const char8* writable_global_variables[] = {
        // ECMA-262 18.1 Value Properties of the Global Object
        u8"globalThis",

        // ECMA-262 18.2 Function Properties of the Global Object
        u8"decodeURI",
        u8"decodeURIComponent",
        u8"encodeURI",
        u8"encodeURIComponent",
        u8"eval",
        u8"isFinite",
        u8"isNaN",
        u8"parseFloat",
        u8"parseInt",

        // ECMA-262 18.3 Constructor Properties of the Global Object
        u8"Array",
        u8"ArrayBuffer",
        u8"BigInt",
        u8"BigInt64Array",
        u8"BigUint64Array",
        u8"Boolean",
        u8"DataView",
        u8"Date",
        u8"Error",
        u8"EvalError",
        u8"Float32Array",
        u8"Float64Array",
        u8"Function",
        u8"Int16Array",
        u8"Int32Array",
        u8"Int8Array",
        u8"Map",
        u8"Number",
        u8"Object",
        u8"Promise",
        u8"Proxy",
        u8"RangeError",
        u8"ReferenceError",
        u8"RegExp",
        u8"Set",
        u8"SharedArrayBuffer",
        u8"String",
        u8"Symbol",
        u8"SyntaxError",
        u8"TypeError",
        u8"URIError",
        u8"Uint16Array",
        u8"Uint32Array",
        u8"Uint8Array",
        u8"Uint8ClampedArray",
        u8"WeakMap",
        u8"WeakSet",

        // ECMA-262 18.4 Other Properties of the Global Object
        u8"Atomics",
        u8"JSON",
        u8"Math",
        u8"Reflect",
    };
    for (string8_view global_variable : writable_global_variables) {
      if (!this->should_remove_global_variable(global_variable)) {
        this->globals_.add_variable(global_variable);
      }
    }

    const char8* non_writable_global_variables[] = {
        // ECMA-262 18.1 Value Properties of the Global Object
        u8"Infinity",
        u8"NaN",
        u8"undefined",
    };
    for (string8_view global_variable : non_writable_global_variables) {
      if (!this->should_remove_global_variable(global_variable)) {
        global_declared_variable* var =
            this->globals_.add_variable(global_variable);
        var->is_writable = false;
      }
    }
  }

  if (this->add_global_group_node_js_) {
    const char8* writable_global_variables[] = {
        u8"Buffer",          u8"GLOBAL",       u8"Intl",
        u8"TextDecoder",     u8"TextEncoder",  u8"URL",
        u8"URLSearchParams", u8"WebAssembly",  u8"clearImmediate",
        u8"clearInterval",   u8"clearTimeout", u8"console",
        u8"escape",          u8"global",       u8"process",
        u8"queueMicrotask",  u8"root",         u8"setImmediate",
        u8"setInterval",     u8"setTimeout",   u8"unescape",
    };
    for (string8_view global_variable : writable_global_variables) {
      if (!this->should_remove_global_variable(global_variable)) {
        this->globals_.add_variable(global_variable);
      }
    }

    const char8* writable_module_variables[] = {
        u8"__dirname", u8"__filename", u8"exports", u8"module", u8"require",
    };
    for (string8_view module_variable : writable_module_variables) {
      if (!this->should_remove_global_variable(module_variable)) {
        global_declared_variable* var =
            this->globals_.add_variable(module_variable);
        var->is_shadowable = false;
      }
    }
  }

  return this->globals_;
}

void configuration::reset_global_groups() {
  this->add_global_group_node_js_ = false;
  this->add_global_group_ecmascript_ = false;
}

bool configuration::add_global_group(string8_view group_name) {
  if (group_name == u8"ecmascript"sv) {
    this->add_global_group_ecmascript_ = true;
    return true;
  }
  if (group_name == u8"node.js"sv) {
    this->add_global_group_node_js_ = true;
    return true;
  }
  return false;
}

global_declared_variable* configuration::add_global_variable(
    string8_view name) {
  return this->globals_.add_variable(name);
}

void configuration::remove_global_variable(string8_view name) {
  this->globals_to_remove_.emplace_back(name);
}

bool configuration::should_remove_global_variable(string8_view name) {
  return std::find(this->globals_to_remove_.begin(),
                   this->globals_to_remove_.end(),
                   name) != this->globals_to_remove_.end();
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
