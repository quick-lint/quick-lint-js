// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript, type_annotation_in_expression_is_an_error) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"x = myVar: Type;"_sv,                                            //
        u8"         ^ Diag_TypeScript_Type_Annotation_In_Expression"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // myVar
                              "visit_variable_assignment",  // x
                          }))
        << "visit_variable_type_use for Type should not happen because it "
           "might produce spurious warnings about undeclared types";
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myVar"}));
  }
}

TEST_F(Test_Parse_TypeScript, warn_on_mistyped_strict_inequality_operator) {
  {
    Test_Parser p(u8"x! == y"_sv, typescript_options, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(nonnull(var x), var y)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^ Diag_Bang_Equal_Equal_Interpreted_As_Non_Null_Assertion.unexpected_space\n"_diag
            u8" ^ .bang"_diag,
        });
  }
  test_parse_and_visit_statement(
      u8"if (length + 1! == constraints.getMaxLength()) {}"_sv,  //
      u8"               ^ Diag_Bang_Equal_Equal_Interpreted_As_Non_Null_Assertion.unexpected_space"_diag,  //
      typescript_options);
  test_parse_and_visit_statement(
      u8"if (typeof diagnostic.code! == 'undefined') {}"_sv,  //
      u8"                           ^ Diag_Bang_Equal_Equal_Interpreted_As_Non_Null_Assertion.unexpected_space"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript,
       mistyped_strict_inequality_operator_is_suppressable) {
  test_parse_and_visit_statement(u8"(x!) == y"_sv, no_diags,
                                 typescript_options);
  test_parse_and_visit_statement(u8"x! /**/ == y"_sv, no_diags,
                                 typescript_options);
  test_parse_and_visit_statement(u8"x!\n== y"_sv, no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript, unicode_next_line_is_whitespace) {
  Spy_Visitor v = test_parse_and_visit_statement(u8"x+\u0085+y;"_sv, no_diags,
                                                 typescript_options);
  EXPECT_THAT(v.variable_uses, ElementsAreArray({u8"x"_sv, u8"y"_sv}));
}

// These examples used to crash with assertion failures.
TEST_F(Test_Parse_TypeScript, no_crash) {
  for (String8_View code : {
           u8"export declare:"_sv,
           u8"export declare export"_sv,
           u8"export declare()"_sv,
       }) {
    Monotonic_Allocator memory("test");
    Diag_List_Diag_Reporter diags(&memory);
    Padded_String code_string(code);
    Parser p(&code_string, &diags, typescript_options);
    Spy_Visitor v;
    // Should not crash:
    p.parse_and_visit_module_catching_fatal_parse_errors(v);
  }
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
