// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VARIABLE_ANALYZER_SUPPORT_H
#define QUICK_LINT_JS_VARIABLE_ANALYZER_SUPPORT_H

#include <quick-lint-js/diagnostic-assertion.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/port/source-location.h>

// TODO(strager): Remove this #include.
#include <quick-lint-js/parse-support.h>

namespace quick_lint_js {
class Global_Declared_Variable_Set;

extern Global_Declared_Variable_Set default_globals;

constexpr Variable_Analyzer_Options javascript_var_options =
    Variable_Analyzer_Options{
        .allow_deleting_typescript_variable = true,
        .eval_can_declare_variables = true,
    };

constexpr Variable_Analyzer_Options typescript_var_options =
    Variable_Analyzer_Options{
        .allow_deleting_typescript_variable = false,
        .eval_can_declare_variables = false,
    };

struct Test_Parse_And_Analyze_Options {
  Parser_Options parse_options;
  Variable_Analyzer_Options analyze_options;
};

extern const Test_Parse_And_Analyze_Options javascript_analyze_options;
extern const Test_Parse_And_Analyze_Options typescript_analyze_options;

// Create a Parser with a Variable_Analyzer and call
// Parser::parse_and_visit_module. Assert that exactly the given diagnostics
// were emitted. See NOTE[_diag-syntax] for examples.
void test_parse_and_analyze(
    String8_View input, No_Diags_Tag, const Test_Parse_And_Analyze_Options&,
    const Global_Declared_Variable_Set&,
    Source_Location caller = Source_Location::current());
void test_parse_and_analyze(
    String8_View input, Diagnostic_Assertion,
    const Test_Parse_And_Analyze_Options&, const Global_Declared_Variable_Set&,
    Source_Location caller = Source_Location::current());
void test_parse_and_analyze(
    String8_View input, Diagnostic_Assertion, Diagnostic_Assertion,
    const Test_Parse_And_Analyze_Options&, const Global_Declared_Variable_Set&,
    Source_Location caller = Source_Location::current());
void test_parse_and_analyze(
    String8_View input, Span<const Diagnostic_Assertion>,
    const Test_Parse_And_Analyze_Options&, const Global_Declared_Variable_Set&,
    Source_Location caller = Source_Location::current());
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
