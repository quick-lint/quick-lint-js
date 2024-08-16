// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/fe/global-variables.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")

namespace quick_lint_js {

// source: https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Build_a_cross_browser_extension#api_namespace
constexpr const Char8 global_variables_browser_extensions[] =
  u8"browser\0"
  u8"chrome\0";

// Last updated: Bun v0.1.4
// Excludes ECMAScript globals.
constexpr const Char8 global_variables_bun[] =
    u8"AbortController\0"
    u8"AbortSignal\0"
    u8"Blob\0"
    u8"Buffer\0"
    u8"BuildError\0"
    u8"Bun\0"
    u8"ByteLengthQueuingStrategy\0"
    u8"CloseEvent\0"
    u8"CountQueuingStrategy\0"
    u8"Crypto\0"
    u8"CustomEvent\0"
    u8"DOMException\0"
    u8"ErrorEvent\0"
    u8"Event\0"
    u8"EventTarget\0"
    u8"HTMLRewriter\0"
    u8"Headers\0"
    u8"Intl\0"
    u8"Loader\0"
    u8"MessageEvent\0"
    u8"ReadableByteStreamController\0"
    u8"ReadableStream\0"
    u8"ReadableStreamBYOBReader\0"
    u8"ReadableStreamBYOBRequest\0"
    u8"ReadableStreamDefaultController\0"
    u8"ReadableStreamDefaultReader\0"
    u8"Request\0"
    u8"ResolveError\0"
    u8"Response\0"
    u8"ShadowRealm\0"
    u8"TextDecoder\0"
    u8"TextEncoder\0"
    u8"TransformStream\0"
    u8"TransformStreamDefaultController\0"
    u8"URL\0"
    u8"URLSearchParams\0"
    u8"WebAssembly\0"
    u8"WebSocket\0"
    u8"WritableStream\0"
    u8"WritableStreamDefaultController\0"
    u8"WritableStreamDefaultWriter\0"
    u8"addEventListener\0"
    u8"alert\0"
    u8"atob\0"
    u8"btoa\0"
    u8"bunJSX\0"
    u8"clearInterval\0"
    u8"clearTimeout\0"
    u8"confirm\0"
    u8"console\0"
    u8"crypto\0"
    u8"escape\0"
    u8"fetch\0"
    u8"performance\0"
    u8"process\0"
    u8"prompt\0"
    u8"queueMicrotask\0"
    u8"reportError\0"
    u8"setInterval\0"
    u8"setTimeout\0"
    u8"unescape\0";

// Last updated: Deno 1.31.1
// Excludes ECMAScript globals.
constexpr const Char8 global_variables_deno[] =
    u8"AbortController\0"
    u8"AbortSignal\0"
    u8"Blob\0"
    u8"ByteLengthQueuingStrategy\0"
    u8"CacheStorage\0"
    u8"Cache\0"
    u8"CloseEvent\0"
    u8"CompressionStream\0"
    u8"CountQueuingStrategy\0"
    u8"CryptoKey\0"
    u8"Crypto\0"
    u8"CustomEvent\0"
    u8"DOMException\0"
    u8"DecompressionStream\0"
    u8"Deno\0"
    u8"ErrorEvent\0"
    u8"EventTarget\0"
    u8"Event\0"
    u8"FileReader\0"
    u8"File\0"
    u8"FormData\0"
    u8"Headers\0"
    u8"Intl\0"
    u8"Location\0"
    u8"MessageChannel\0"
    u8"MessageEvent\0"
    u8"MessagePort\0"
    u8"Navigator\0"
    u8"PerformanceEntry\0"
    u8"PerformanceMark\0"
    u8"PerformanceMeasure\0"
    u8"Performance\0"
    u8"ProgressEvent\0"
    u8"PromiseRejectionEvent\0"
    u8"ReadableByteStreamController\0"
    u8"ReadableStreamBYOBReader\0"
    u8"ReadableStreamBYOBRequest\0"
    u8"ReadableStreamDefaultController\0"
    u8"ReadableStreamDefaultReader\0"
    u8"ReadableStream\0"
    u8"Request\0"
    u8"Response\0"
    u8"Storage\0"
    u8"SubtleCrypto\0"
    u8"TextDecoderStream\0"
    u8"TextDecoder\0"
    u8"TextEncoderStream\0"
    u8"TextEncoder\0"
    u8"TransformStreamDefaultController\0"
    u8"TransformStream\0"
    u8"URLPattern\0"
    u8"URLSearchParams\0"
    u8"URL\0"
    u8"WebAssembly\0"
    u8"WebSocket\0"
    u8"Window\0"
    u8"Worker\0"
    u8"WritableStreamDefaultController\0"
    u8"WritableStreamDefaultWriter\0"
    u8"WritableStream\0"
    u8"alert\0"
    u8"atob\0"
    u8"btoa\0"
    u8"caches\0"
    u8"clearInterval\0"
    u8"clearTimeout\0"
    u8"close\0"
    u8"closed\0"
    u8"confirm\0"
    u8"console\0"
    u8"crypto\0"
    u8"escape\0"
    u8"fetch\0"
    u8"localStorage\0"
    u8"location\0"
    u8"navigator\0"
    u8"onbeforeunload\0"
    u8"onerror\0"
    u8"onload\0"
    u8"onunhandledrejection\0"
    u8"onunload\0"
    u8"performance\0"
    u8"prompt\0"
    u8"queueMicrotask\0"
    u8"reportError\0"
    u8"self\0"
    u8"sessionStorage\0"
    u8"setInterval\0"
    u8"setTimeout\0"
    u8"structuredClone\0"
    u8"unescape\0"
    u8"window\0";

constexpr const Char8 global_variables_ecmascript[] =
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
    u8"AggregateError\0"
    u8"ArrayBuffer\0"
    u8"Array\0"
    u8"BigInt64Array\0"
    u8"BigInt\0"
    u8"BigUint64Array\0"
    u8"Boolean\0"
    u8"DataView\0"
    u8"Date\0"
    u8"Error\0"
    u8"EvalError\0"
    u8"FinalizationRegistry\0"
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
    u8"WeakRef\0"
    u8"WeakSet\0"

    // ECMA-262 18.4 Other Properties of the Global Object
    u8"Atomics\0"
    u8"JSON\0"
    u8"Math\0"
    u8"Reflect\0";

constexpr const Char8 global_variables_ecmascript_non_writable[] =
    // ECMA-262 18.1 Value Properties of the Global Object
    u8"Infinity\0"
    u8"NaN\0"
    u8"undefined\0";

constexpr const Char8 global_variables_jasmine[] =
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

constexpr const Char8 global_variables_jest[] =
    u8"afterAll\0"
    u8"afterEach\0"
    u8"beforeAll\0"
    u8"beforeEach\0"
    u8"describe\0"
    u8"expect\0"
    u8"fdescribe\0"
    u8"fit\0"
    u8"it\0"
    u8"jest\0"
    u8"test\0"
    u8"xdescribe\0"
    u8"xit\0"
    u8"xtest\0";

constexpr const Char8 global_variables_jquery[] =
    u8"$\0"
    u8"jQuery\0";

constexpr const Char8 global_variables_node_js[] =
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

constexpr const Char8 global_variables_node_js_es[] =
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

constexpr const Char8 global_variables_quickjs[] =
    u8"scriptArgs\0"
    u8"console\0"
    u8"print\0";

constexpr const Char8 global_variables_typescript_types[] =
    // Source:
    // https://github.com/microsoft/TypeScript/tree/1da6d87a053746901a7eca6395a4bd15190b13c4/src/lib
    //
    // clang-format off
    //
    // sed -E -n -e 's/^(declare )?type ([a-zA-Z_$][a-zA-Z_$0-9]*).*/\2/p' src/lib/es*.d.ts >ts-types
    // sed -E -n -e 's/^interface\s*([a-zA-Z_$][a-zA-Z_$0-9]*).*/\1/p' src/lib/es*.d.ts >ts-interfaces
    // sed -E -n -e 's/^(declare )?var\s*([a-zA-Z_$][a-zA-Z_$0-9]*).*/\2/p' src/lib/es*.d.ts >ts-vars
    // comm -2 -3 <(cat ts-interfaces ts-types | sort -u) <(sort ts-vars -u)
    //
    // clang-format on
    u8"AggregateErrorConstructor\0"
    u8"ArrayBufferConstructor\0"
    u8"ArrayBufferLike\0"
    u8"ArrayBufferTypes\0"
    u8"ArrayBufferView\0"
    u8"ArrayConstructor\0"
    u8"ArrayLike\0"
    u8"AsyncGenerator\0"
    u8"AsyncGeneratorFunction\0"
    u8"AsyncGeneratorFunctionConstructor\0"
    u8"AsyncIterable\0"
    u8"AsyncIterableIterator\0"
    u8"AsyncIterator\0"
    u8"Awaited\0"
    u8"BigInt64ArrayConstructor\0"
    u8"BigIntConstructor\0"
    u8"BigIntToLocaleStringOptions\0"
    u8"BigUint64ArrayConstructor\0"
    u8"BooleanConstructor\0"
    u8"CallableFunction\0"
    u8"Capitalize\0"
    u8"ConcatArray\0"
    u8"ConstructorParameters\0"
    u8"DataViewConstructor\0"
    u8"DateConstructor\0"
    u8"ErrorConstructor\0"
    u8"ErrorOptions\0"
    u8"EvalErrorConstructor\0"
    u8"Exclude\0"
    u8"Extract\0"
    u8"FinalizationRegistryConstructor\0"
    u8"FlatArray\0"
    u8"Float32ArrayConstructor\0"
    u8"Float64ArrayConstructor\0"
    u8"FunctionConstructor\0"
    u8"Generator\0"
    u8"GeneratorFunction\0"
    u8"GeneratorFunctionConstructor\0"
    u8"IArguments\0"
    u8"ImportAssertions\0"
    u8"ImportCallOptions\0"
    u8"ImportMeta\0"
    u8"InstanceType\0"
    u8"Int16ArrayConstructor\0"
    u8"Int32ArrayConstructor\0"
    u8"Int8ArrayConstructor\0"
    u8"Iterable\0"
    u8"IterableIterator\0"
    u8"Iterator\0"
    u8"IteratorResult\0"
    u8"IteratorReturnResult\0"
    u8"IteratorYieldResult\0"
    u8"Lowercase\0"
    u8"MapConstructor\0"
    u8"NewableFunction\0"
    u8"NonNullable\0"
    u8"NumberConstructor\0"
    u8"ObjectConstructor\0"
    u8"Omit\0"
    u8"OmitThisParameter\0"
    u8"Parameters\0"
    u8"Partial\0"
    u8"Pick\0"
    u8"PromiseConstructor\0"
    u8"PromiseConstructorLike\0"
    u8"PromiseFulfilledResult\0"
    u8"PromiseLike\0"
    u8"PromiseRejectedResult\0"
    u8"PromiseSettledResult\0"
    u8"PropertyDescriptor\0"
    u8"PropertyDescriptorMap\0"
    u8"PropertyKey\0"
    u8"ProxyConstructor\0"
    u8"ProxyHandler\0"
    u8"RangeErrorConstructor\0"
    u8"Readonly\0"
    u8"ReadonlyArray\0"
    u8"ReadonlyMap\0"
    u8"ReadonlySet\0"
    u8"Record\0"
    u8"ReferenceErrorConstructor\0"
    u8"RegExpConstructor\0"
    u8"RegExpExecArray\0"
    u8"RegExpIndicesArray\0"
    u8"RegExpMatchArray\0"
    u8"Required\0"
    u8"ReturnType\0"
    u8"SetConstructor\0"
    u8"SharedArrayBufferConstructor\0"
    u8"StringConstructor\0"
    u8"SymbolConstructor\0"
    u8"SyntaxErrorConstructor\0"
    u8"TemplateStringsArray\0"
    u8"ThisParameterType\0"
    u8"ThisType\0"
    u8"TypedPropertyDescriptor\0"
    u8"TypeErrorConstructor\0"
    u8"Uint16ArrayConstructor\0"
    u8"Uint32ArrayConstructor\0"
    u8"Uint8ArrayConstructor\0"
    u8"Uint8ClampedArrayConstructor\0"
    u8"Uncapitalize\0"
    u8"Uppercase\0"
    u8"URIErrorConstructor\0"
    u8"WeakMapConstructor\0"
    u8"WeakRefConstructor\0"
    u8"WeakSetConstructor\0";

const Global_Group global_groups[] = {
    {
        .name = u8"browser",
        .globals = global_variables_browser,
        .type_only_globals = global_variables_web_types,
        .globals_count = 990,
        .type_only_globals_count = 515,
    },    
    {
        .name = u8"browser_extensions",
        .globals = global_variables_browser_extensions,
        .globals_count = 2,
    },
    {
        .name = u8"bun",
        .globals = global_variables_bun,
        .globals_count = 61,
    },
    {
        .name = u8"deno",
        .globals = global_variables_deno,
        .globals_count = 92,
    },
    {
        .name = u8"ecmascript",
        .globals = global_variables_ecmascript,
        .non_writable_globals = global_variables_ecmascript_non_writable,
        .globals_count = 54,
        .non_writable_globals_count = 3,
    },
    {
        .name = u8"jasmine",
        .globals = global_variables_jasmine,
        .globals_count = 20,
    },
    {
        .name = u8"jest",
        .globals = global_variables_jest,
        .globals_count = 14,
    },
    {
        .name = u8"jquery",
        .globals = global_variables_jquery,
        .globals_count = 2,
    },
    {
        .name = u8"node.js",
        .globals = global_variables_node_js,
        .globals_count = 26,
    },
    {
        .name = u8"node.js-es",
        .globals = global_variables_node_js_es,
        .globals_count = 21,
    },
    {
        .name = u8"quickjs",
        .globals = global_variables_quickjs,
        .globals_count = 3,
    },
    {
        .name = u8"typescript",
        .type_only_globals = global_variables_typescript_types,
        .type_only_globals_count = 107,
    },
    {
        .name = u8"web-worker",
        .globals = global_variables_web_worker,
        .type_only_globals = global_variables_web_types,
        .globals_count = 232,
        .type_only_globals_count = 515,
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
