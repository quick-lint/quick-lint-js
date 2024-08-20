// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diagnostic-assertion.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/constinit.h>
#include <quick-lint-js/port/source-location.h>
#include <quick-lint-js/variable-analyzer-support.h>

namespace quick_lint_js {
Global_Declared_Variable_Set default_globals = Configuration().globals();

QLJS_CONSTINIT const Test_Parse_And_Analyze_Options
    javascript_analyze_options = {
        .parse_options = javascript_options,
        .analyze_options = javascript_var_options,
};

QLJS_CONSTINIT const Test_Parse_And_Analyze_Options
    typescript_analyze_options = {
        .parse_options = typescript_options,
        .analyze_options = typescript_var_options,
};

void test_parse_and_analyze(String8_View input, No_Diags_Tag,
                            const Test_Parse_And_Analyze_Options& options,
                            const Global_Declared_Variable_Set& globals,
                            Source_Location caller) {
  test_parse_and_analyze(input, Span<const Diagnostic_Assertion>(), options,
                         globals, caller);
}

void test_parse_and_analyze(String8_View input, Diagnostic_Assertion diag0,
                            const Test_Parse_And_Analyze_Options& options,
                            const Global_Declared_Variable_Set& globals,
                            Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0};
  test_parse_and_analyze(input, Span<const Diagnostic_Assertion>(assertions),
                         options, globals, caller);
}

void test_parse_and_analyze(String8_View input, Diagnostic_Assertion diag0,
                            Diagnostic_Assertion diag1,
                            const Test_Parse_And_Analyze_Options& options,
                            const Global_Declared_Variable_Set& globals,
                            Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1};
  test_parse_and_analyze(input, Span<const Diagnostic_Assertion>(assertions),
                         options, globals, caller);
}

void test_parse_and_analyze(String8_View input,
                            Span<const Diagnostic_Assertion> diag_assertions,
                            const Test_Parse_And_Analyze_Options& options,
                            const Global_Declared_Variable_Set& globals,
                            Source_Location caller) {
  Monotonic_Allocator memory("test");
  Padded_String code(input);

  Failing_Diag_Reporter failing_diag_reporter;
  Diag_List_Diag_Reporter diags(&memory);

  Parser p(&code, &diags, options.parse_options);

  Variable_Analyzer var_analyzer(&diags, &globals, options.analyze_options);

  p.parse_and_visit_module(var_analyzer);

  assert_diagnostics(&code, diags.diags(), diag_assertions, caller);
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
