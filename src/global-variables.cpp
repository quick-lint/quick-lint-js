// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/global-variables.h>

namespace quick_lint_js {
constexpr const char8 global_variables_ecmascript[] =
    // ECMA-262 18.1 Value Properties of the Global Object
    u8"globalThis\0"

    // ECMA-262 18.2 Function Properties of the Global Object
    u8"decodeURI\0"
    u8"decodeURIComponent\0"
    u8"encodeURI\0"
    u8"encodeURIComponent\0"
    u8"eval\0"
    u8"isFinite\0"
    u8"isNaN\0"
    u8"parseFloat\0"
    u8"parseInt\0"

    // ECMA-262 18.3 Constructor Properties of the Global Object
    u8"Array\0"
    u8"ArrayBuffer\0"
    u8"BigInt\0"
    u8"BigInt64Array\0"
    u8"BigUint64Array\0"
    u8"Boolean\0"
    u8"DataView\0"
    u8"Date\0"
    u8"Error\0"
    u8"EvalError\0"
    u8"Float32Array\0"
    u8"Float64Array\0"
    u8"Function\0"
    u8"Int16Array\0"
    u8"Int32Array\0"
    u8"Int8Array\0"
    u8"Map\0"
    u8"Number\0"
    u8"Object\0"
    u8"Promise\0"
    u8"Proxy\0"
    u8"RangeError\0"
    u8"ReferenceError\0"
    u8"RegExp\0"
    u8"Set\0"
    u8"SharedArrayBuffer\0"
    u8"String\0"
    u8"Symbol\0"
    u8"SyntaxError\0"
    u8"TypeError\0"
    u8"URIError\0"
    u8"Uint16Array\0"
    u8"Uint32Array\0"
    u8"Uint8Array\0"
    u8"Uint8ClampedArray\0"
    u8"WeakMap\0"
    u8"WeakSet\0"

    // ECMA-262 18.4 Other Properties of the Global Object
    u8"Atomics\0"
    u8"JSON\0"
    u8"Math\0"
    u8"Reflect\0";

constexpr const char8 global_variables_ecmascript_non_writable[] =
    // ECMA-262 18.1 Value Properties of the Global Object
    u8"Infinity\0"
    u8"NaN\0"
    u8"undefined\0";

constexpr const char8 global_variables_jasmine[] =
    u8"afterAll\0"
    u8"afterEach\0"
    u8"beforeAll\0"
    u8"beforeEach\0"
    u8"describe\0"
    u8"expect\0"
    u8"expectAsync\0"
    u8"fail\0"
    u8"fdescribe\0"
    u8"fit\0"
    u8"it\0"
    u8"jasmine"
    u8"jsApiReporter\0"
    u8"pending\0"
    u8"setSpecProperty\0"
    u8"setSuiteProperty\0"
    u8"spyOn\0"
    u8"spyOnAllFunctions\0"
    u8"spyOnProperty\0"
    u8"xdescribe\0"
    u8"xit\0";

constexpr const char8 global_variables_jest[] =
    u8"afterAll\0"
    u8"afterEach\0"
    u8"beforeAll\0"
    u8"beforeEach\0"
    u8"describe\0"
    u8"expect\0"
    u8"fdescribe\0"
    u8"fit\0"
    u8"it\0"
    u8"test\0"
    u8"xdescribe\0"
    u8"xit\0"
    u8"xtest\0";

constexpr const char8 global_variables_jquery[] =
    u8"$\0"
    u8"jQuery\0";

constexpr const char8 global_variables_node_js[] =
    u8"Buffer\0"
    u8"GLOBAL\0"  // Removed in v14.16.1 (or earlier?).
    u8"Intl\0"
    u8"TextDecoder\0"      // Added in v12.22.1 (or earlier?).
    u8"TextEncoder\0"      // Added in v12.22.1 (or earlier?).
    u8"URLSearchParams\0"  // Added in v10.24.1 (or earlier?).
    u8"URL\0"              // Added in v10.24.1 (or earlier?).
    u8"WebAssembly\0"      // Added in v8.17.0 (or earlier?).
    u8"__dirname\0"
    u8"__filename\0"
    u8"clearImmediate\0"
    u8"clearInterval\0"
    u8"clearTimeout\0"
    u8"console\0"
    u8"escape\0"
    u8"exports\0"
    u8"global\0"
    u8"module\0"
    u8"process\0"
    u8"queueMicrotask\0"  // Added in v12.22.1 (or earlier?).
    u8"require\0"
    u8"root\0"  // Removed in v14.16.1 (or earlier?).
    u8"setImmediate\0"
    u8"setInterval\0"
    u8"setTimeout\0"
    u8"unescape\0";

constexpr const char8 global_variables_node_js_es[] =
    u8"Buffer\0"
    u8"GLOBAL\0"  // Removed in v14.16.1 (or earlier?).
    u8"Intl\0"
    u8"TextDecoder\0"      // Added in v12.22.1 (or earlier?).
    u8"TextEncoder\0"      // Added in v12.22.1 (or earlier?).
    u8"URL\0"              // Added in v10.24.1 (or earlier?).
    u8"URLSearchParams\0"  // Added in v10.24.1 (or earlier?).
    u8"WebAssembly\0"      // Added in v8.17.0 (or earlier?).
    u8"clearImmediate\0"
    u8"clearInterval\0"
    u8"clearTimeout\0"
    u8"console\0"
    u8"escape\0"
    u8"global\0"
    u8"process\0"
    u8"queueMicrotask\0"  // Added in v12.22.1 (or earlier?).
    u8"root\0"            // Removed in v14.16.1 (or earlier?).
    u8"setImmediate\0"
    u8"setInterval\0"
    u8"setTimeout\0"
    u8"unescape\0";

const global_group global_groups[] = {
    {
        .name = u8"browser",
        .globals = global_variables_browser,
        .non_writable_globals = nullptr,
        .non_shadowable_globals = nullptr,
        .globals_count = 1006,
        .non_writable_globals_count = 0,
        .non_shadowable_globals_count = 0,
    },
    {
        .name = u8"ecmascript",
        .globals = global_variables_ecmascript,
        .non_writable_globals = global_variables_ecmascript_non_writable,
        .non_shadowable_globals = nullptr,
        .globals_count = 51,
        .non_writable_globals_count = 3,
        .non_shadowable_globals_count = 0,
    },
    {
        .name = u8"jasmine",
        .globals = global_variables_jasmine,
        .non_writable_globals = nullptr,
        .non_shadowable_globals = nullptr,
        .globals_count = 20,
        .non_writable_globals_count = 0,
        .non_shadowable_globals_count = 0,
    },
    {
        .name = u8"jest",
        .globals = global_variables_jest,
        .non_writable_globals = nullptr,
        .non_shadowable_globals = nullptr,
        .globals_count = 13,
        .non_writable_globals_count = 0,
        .non_shadowable_globals_count = 0,
    },
    {
        .name = u8"jquery",
        .globals = global_variables_jquery,
        .non_writable_globals = nullptr,
        .non_shadowable_globals = nullptr,
        .globals_count = 2,
        .non_writable_globals_count = 0,
        .non_shadowable_globals_count = 0,
    },
    {
        .name = u8"node.js",
        .globals = global_variables_node_js,
        .non_writable_globals = nullptr,
        .non_shadowable_globals = nullptr,
        .globals_count = 26,
        .non_writable_globals_count = 0,
        .non_shadowable_globals_count = 0,
    },
    {
        .name = u8"node.js-es",
        .globals = global_variables_node_js_es,
        .non_writable_globals = nullptr,
        .non_shadowable_globals = nullptr,
        .globals_count = 21,
        .non_writable_globals_count = 0,
        .non_shadowable_globals_count = 0,
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
