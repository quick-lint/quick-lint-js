// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lint.h>

namespace quick_lint_js {
linter::linter(error_reporter *error_reporter)
    : error_reporter_(error_reporter) {
  this->scopes_.emplace_back();
  scope &global_scope = this->scopes_.back();

  const char8 *writable_global_variables[] = {
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

  for (const char8 *global_variable : writable_global_variables) {
    global_scope.declared_variables.emplace_back(declared_variable{
        .name = global_variable,
        .kind = variable_kind::_function,
        .declaration = std::nullopt,
        .declaration_scope = declared_variable_scope::declared_in_current_scope,
    });
  }

  const char8 *non_writable_global_variables[] = {
      // ECMA-262 18.1 Value Properties of the Global Object
      u8"Infinity",
      u8"NaN",
      u8"undefined",
  };
  for (const char8 *global_variable : non_writable_global_variables) {
    global_scope.declared_variables.emplace_back(declared_variable{
        .name = global_variable,
        .kind = variable_kind::_const,
        .declaration = std::nullopt,
        .declaration_scope = declared_variable_scope::declared_in_current_scope,
    });
  }
}
}
