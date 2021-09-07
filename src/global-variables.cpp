// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/global-variables.h>

namespace quick_lint_js {
constexpr const char8* global_variables_ecmascript[] = {
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

    nullptr,
};

constexpr const char8* global_variables_ecmascript_non_writable[] = {
    // ECMA-262 18.1 Value Properties of the Global Object
    u8"Infinity",
    u8"NaN",
    u8"undefined",
    nullptr,
};

constexpr const char8* global_variables_node_js[] = {
    u8"Buffer",
    u8"GLOBAL",
    u8"Intl",
    u8"TextDecoder",
    u8"TextEncoder",
    u8"URL",
    u8"URLSearchParams",
    u8"WebAssembly",
    u8"clearImmediate",
    u8"clearInterval",
    u8"clearTimeout",
    u8"console",
    u8"escape",
    u8"global",
    u8"process",
    u8"queueMicrotask",
    u8"root",
    u8"setImmediate",
    u8"setInterval",
    u8"setTimeout",
    u8"unescape",
    nullptr,
};

constexpr const char8* global_variables_node_js_non_shadowable[] = {
    u8"__dirname", u8"__filename", u8"exports",
    u8"module",    u8"require",    nullptr,
};

const global_group global_groups[] = {
    {
        .name = u8"browser",
        .globals = global_variables_browser,
        .non_writable_globals = nullptr,
        .non_shadowable_globals = nullptr,
    },
    {
        .name = u8"ecmascript",
        .globals = global_variables_ecmascript,
        .non_writable_globals = global_variables_ecmascript_non_writable,
        .non_shadowable_globals = nullptr,
    },
    {
        .name = u8"node.js",
        .globals = global_variables_node_js,
        .non_writable_globals = nullptr,
        .non_shadowable_globals = global_variables_node_js_non_shadowable,
    },
};
static_assert(global_group_count == std::size(global_groups),
              "configuration::global_group_count should match the number of "
              "groups in global_groups");
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
