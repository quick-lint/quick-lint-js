// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
constexpr const char8 *writable_global_variables[] = {
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
    u8"AggregateError",
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
    u8"FinalizationRegistry",
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
    u8"WeakRef",
    u8"WeakSet",

    // ECMA-262 18.4 Other Properties of the Global Object
    u8"Atomics",
    u8"JSON",
    u8"Math",
    u8"Reflect",
};

constexpr const char8 *non_writable_global_variables[] = {
    // ECMA-262 18.1 Value Properties of the Global Object
    u8"Infinity",
    u8"NaN",
    u8"undefined",
};

constexpr const char8 *type_only_global_variables[] = {
    u8"Awaited",
    u8"Capitalize",
    u8"ConstructorParameters",
    u8"Exclude",
    u8"Extract",
    u8"InstanceType",
    u8"Lowercase",
    u8"NonNullable",
    u8"Omit",
    u8"OmitThisParameter",
    u8"Parameters",
    u8"Partial",
    u8"Pick",
    u8"Readonly",
    u8"Record",
    u8"Required",
    u8"ReturnType",
    u8"ThisParameterType",
    u8"ThisType",
    u8"Uncapitalize",
    u8"Uppercase",
};

constexpr const char8 *nodejs_global_variables[] = {
    u8"Array",
    u8"ArrayBuffer",
    u8"Atomics",
    u8"BigInt",
    u8"BigInt64Array",
    u8"BigUint64Array",
    u8"Boolean",
    u8"Buffer",
    u8"DataView",
    u8"Date",
    u8"Error",
    u8"EvalError",
    u8"Float32Array",
    u8"Float64Array",
    u8"Function",
    u8"GLOBAL",
    u8"Infinity",
    u8"Int16Array",
    u8"Int32Array",
    u8"Int8Array",
    u8"Intl",
    u8"JSON",
    u8"Map",
    u8"Math",
    u8"NaN",
    u8"Number",
    u8"Object",
    u8"Promise",
    u8"Proxy",
    u8"RangeError",
    u8"ReferenceError",
    u8"Reflect",
    u8"RegExp",
    u8"Set",
    u8"SharedArrayBuffer",
    u8"String",
    u8"Symbol",
    u8"SyntaxError",
    u8"TextDecoder",
    u8"TextEncoder",
    u8"TypeError",
    u8"URIError",
    u8"URL",
    u8"URLSearchParams",
    u8"Uint16Array",
    u8"Uint32Array",
    u8"Uint8Array",
    u8"Uint8ClampedArray",
    u8"WeakMap",
    u8"WeakSet",
    u8"WebAssembly",
    u8"clearImmediate",
    u8"clearInterval",
    u8"clearTimeout",
    u8"console",
    u8"decodeURI",
    u8"decodeURIComponent",
    u8"encodeURI",
    u8"encodeURIComponent",
    u8"escape",
    u8"eval",
    u8"global",
    u8"globalThis",
    u8"isFinite",
    u8"isNaN",
    u8"parseFloat",
    u8"parseInt",
    u8"process",
    u8"queueMicrotask",
    u8"root",
    u8"setImmediate",
    u8"setInterval",
    u8"setTimeout",
    u8"undefined",
    u8"unescape",
};

TEST(test_variable_analyzer_globals, global_variables_are_usable) {
  // Array = null;
  // Array;
  for (const char8 *global_variable : writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_variable_use(identifier_of(global_variable));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }

  // NaN;
  for (const char8 *global_variable : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_use(identifier_of(global_variable));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_globals,
     immutable_global_variables_are_not_assignable) {
  for (const char8 *global_variable : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));

    // NaN = null;  // ERROR
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(diag_assignment_to_const_global_variable,
                                   assignment, span_of(global_variable)),
                }));
  }

  for (const char8 *global_variable : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));

    // (() => {
    //   NaN = null;  // ERROR
    // });
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_enter_function_scope();
    l.visit_enter_function_scope_body();
    l.visit_variable_assignment(identifier_of(global_variable));
    l.visit_exit_function_scope();
    l.visit_end_of_module();

    EXPECT_THAT(v.errors,
                ElementsAreArray({
                    DIAG_TYPE_SPAN(diag_assignment_to_const_global_variable,
                                   assignment, span_of(global_variable)),
                }));
  }
}

TEST(test_variable_analyzer_globals, nodejs_global_variables_are_usable) {
  for (const char8 *global_variable : nodejs_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    l.visit_variable_use(identifier_of(global_variable));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_globals,
     non_module_nodejs_global_variables_are_shadowable) {
  for (variable_declaration_flags flags :
       {variable_declaration_flags::none,
        variable_declaration_flags::initialized_with_equals}) {
    diag_collector v;
    variable_analyzer l(&v, &default_globals, javascript_var_options);
    // Intentionally excluded: __dirname, __filename, exports, module, require
    for (const char8 *global_variable : nodejs_global_variables) {
      l.visit_variable_declaration(identifier_of(global_variable),
                                   variable_kind::_let, flags);
    }
    l.visit_end_of_module();

    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_globals,
     type_only_global_variables_are_not_usable_in_expressions) {
  for (const char8 *global_variable : type_only_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));

    // Awaited;
    {
      diag_collector v;
      variable_analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_use(identifier_of(global_variable));
      l.visit_end_of_module();
      EXPECT_THAT(v.errors, ElementsAreArray({
                                DIAG_TYPE_SPAN(diag_use_of_undeclared_variable,
                                               name, span_of(global_variable)),
                            }));
    }

    // Awaited = null;
    {
      diag_collector v;
      variable_analyzer l(&v, &default_globals, typescript_var_options);
      l.visit_variable_assignment(identifier_of(global_variable));
      l.visit_end_of_module();
      EXPECT_THAT(v.errors,
                  ElementsAreArray({
                      DIAG_TYPE_SPAN(diag_assignment_to_undeclared_variable,
                                     assignment, span_of(global_variable)),
                  }));
    }
  }
}

TEST(test_variable_analyzer_globals,
     type_only_global_variables_are_usable_in_types) {
  // null as Awaited;
  for (const char8 *global_variable : type_only_global_variables) {
    SCOPED_TRACE(out_string8(global_variable));
    diag_collector v;
    variable_analyzer l(&v, &default_globals, typescript_var_options);
    l.visit_variable_type_use(identifier_of(global_variable));
    l.visit_end_of_module();
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_variable_analyzer_globals,
     any_variable_is_declarable_and_usable_if_opted_into) {
  // This tests the "literally-anything" global group.

  global_declared_variable_set globals;
  globals.add_literally_everything();

  const char8 builtin_1_declaration[] = u8"Object";
  const char8 builtin_2_use[] = u8"Array";
  const char8 anything_1_declaration[] = u8"thisVariableDoesNotExistInAnyList";
  const char8 anything_2_use[] = u8"iDoNotExistInAnyList";

  diag_collector v;
  variable_analyzer l(&v, &globals, javascript_var_options);
  l.visit_variable_declaration(identifier_of(builtin_1_declaration),
                               variable_kind::_let,
                               variable_declaration_flags::none);
  l.visit_variable_use(identifier_of(builtin_2_use));
  l.visit_variable_declaration(identifier_of(anything_1_declaration),
                               variable_kind::_let,
                               variable_declaration_flags::none);
  l.visit_variable_use(identifier_of(anything_2_use));
  l.visit_end_of_module();

  EXPECT_THAT(v.errors, IsEmpty());
}
}
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
