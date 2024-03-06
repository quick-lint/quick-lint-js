// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Class,
     static_property_initializer_cannot_use_lexical_before_declaration) {
  test_parse_and_analyze(
      u8"class C { static prop = f(); }  let f = () => {};"_sv,
      u8"                        ^ Diag_Variable_Used_Before_Declaration.use"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"class C { static prop = new B(); }  class B {}"_sv,
      u8"                            ^ Diag_Variable_Used_Before_Declaration.use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Class,
     static_block_cannot_use_lexical_before_declaration) {
  test_parse_and_analyze(
      u8"class C { static { f(); } }  let f = () => {};"_sv,
      u8"                   ^ Diag_Variable_Used_Before_Declaration.use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Class,
     computed_property_name_cannot_use_lexical_before_declaration) {
  test_parse_and_analyze(
      u8"class C { [f()]; } let f = () => {};"_sv,
      u8"           ^ Diag_Variable_Used_Before_Declaration.use"_diag,
      javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"class C { [f()] = null; }  let f = () => {};"_sv,
      u8"           ^ Diag_Variable_Used_Before_Declaration.use"_diag,
      javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Class,
     instance_property_initializer_can_use_lexical_before_declaration) {
  test_parse_and_analyze(u8"class C { prop = f(); }  let f = () => {};"_sv,
                         no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C { prop = new B(); }  class B {}"_sv,
                         no_diags, javascript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Class, class_decorator_can_reference_class) {
  test_parse_and_analyze(
      u8"function myDecorator() {}\n"_sv
      u8"@myDecorator(C) class C {}"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(u8"@C.decorate class C { static decorate() {} }"_sv,
                         no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function myDecorator() {}\n"_sv
      u8"export @myDecorator(C) class C {}"_sv,
      no_diags, javascript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function myDecorator() {}\n"_sv
      u8"export default @myDecorator(C) class C {}"_sv,
      no_diags, javascript_analyze_options, default_globals);
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
