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
#include <quick-lint-js/lint.h>

namespace quick_lint_js {
linter::linter(error_reporter *error_reporter)
    : error_reporter_(error_reporter) {
  this->scopes_.emplace_back();
  scope &global_scope = this->scopes_.back();

  const char *writable_global_variables[] = {
      // ECMA-262 18.1 Value Properties of the Global Object
      "globalThis",

      // ECMA-262 18.2 Function Properties of the Global Object
      "decodeURI",
      "decodeURIComponent",
      "encodeURI",
      "encodeURIComponent",
      "eval",
      "isFinite",
      "isNaN",
      "parseFloat",
      "parseInt",

      // ECMA-262 18.3 Constructor Properties of the Global Object
      "Array",
      "ArrayBuffer",
      "BigInt",
      "BigInt64Array",
      "BigUint64Array",
      "Boolean",
      "DataView",
      "Date",
      "Error",
      "EvalError",
      "Float32Array",
      "Float64Array",
      "Function",
      "Int16Array",
      "Int32Array",
      "Int8Array",
      "Map",
      "Number",
      "Object",
      "Promise",
      "Proxy",
      "RangeError",
      "ReferenceError",
      "RegExp",
      "Set",
      "SharedArrayBuffer",
      "String",
      "Symbol",
      "SyntaxError",
      "TypeError",
      "URIError",
      "Uint16Array",
      "Uint32Array",
      "Uint8Array",
      "Uint8ClampedArray",
      "WeakMap",
      "WeakSet",

      // ECMA-262 18.4 Other Properties of the Global Object
      "Atomics",
      "JSON",
      "Math",
      "Reflect",
  };

  for (const char *global_variable : writable_global_variables) {
    global_scope.declared_variables.emplace_back(declared_variable{
        .name = global_variable,
        .kind = variable_kind::_function,
        .declaration = std::nullopt,
    });
  }

  const char *non_writable_global_variables[] = {
      // ECMA-262 18.1 Value Properties of the Global Object
      "Infinity",
      "NaN",
      "undefined",
  };
  for (const char *global_variable : non_writable_global_variables) {
    global_scope.declared_variables.emplace_back(declared_variable{
        .name = global_variable,
        .kind = variable_kind::_const,
        .declaration = std::nullopt,
    });
  }
}
}  // namespace quick_lint_js
