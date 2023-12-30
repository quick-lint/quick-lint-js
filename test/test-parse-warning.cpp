// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
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
using ::testing::IsEmpty;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
class Test_Parse_Warning : public Test_Parse_Expression {};
// TODO(strager): Move Test_Error_Equals_Does_Not_Distribute_Over_Or tests into
// their own test file.
class Test_Error_Equals_Does_Not_Distribute_Over_Or
    : public Test_Parse_Expression {};

TEST_F(Test_Parse_Warning, condition_with_assignment_from_literal) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (x = 42) {}"_sv,  //
        u8"      ^ Diag_Assignment_Makes_Condition_Constant"_diag);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }

  test_parse_and_visit_statement(
      u8"if (o.prop = 'hello') {}"_sv,  //
      u8"           ^ Diag_Assignment_Makes_Condition_Constant"_diag);

  for (String8_View code : {
           u8"while (x = 'hello') {}"_sv,
           u8"for (; x = 'hello'; ) {}"_sv,
           u8"do {} while (x = 'hello');"_sv,
       }) {
    test_parse_and_visit_statement(
        code, u8"Diag_Assignment_Makes_Condition_Constant"_diag);
  }
}

TEST_F(Test_Parse_Warning, non_condition_with_assignment_from_literal) {
  for (String8_View code : {
           u8"with (x = 'hello') {}"_sv,
           u8"for (x = 'hello'; ; ) {}"_sv,
           u8"for (; ; x = 'hello') {}"_sv,
           u8"switch (x = 'hello') {}"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code);
    p.parse_and_visit_statement();
  }
}

TEST_F(Test_Parse_Warning,
       condition_with_assignment_from_literal_with_parentheses) {
  {
    Spy_Visitor p =
        test_parse_and_visit_statement(u8"if ((x = 42)) {}"_sv, no_diags);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }
}

TEST_F(Test_Parse_Warning, condition_with_updating_assignment_from_literal) {
  {
    Spy_Visitor p =
        test_parse_and_visit_statement(u8"if (x += 42) {}"_sv, no_diags);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }
}

TEST_F(Test_Parse_Warning, condition_with_assignment_from_non_literal) {
  {
    Spy_Visitor p =
        test_parse_and_visit_statement(u8"if (x = y) {}"_sv, no_diags);
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
  }
}

TEST_F(Test_Error_Equals_Does_Not_Distribute_Over_Or, examples) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (x === 'A' || 'B') {}"_sv,  //
        u8"              ^^ Diag_Equals_Does_Not_Distribute_Over_Or.or_operator\n"_diag
        u8"      ^^^ .equals_operator"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  test_parse_and_visit_statement(
      u8"if (x === 10 || 0) {}"_sv,  //
      u8"             ^^ Diag_Equals_Does_Not_Distribute_Over_Or.or_operator\n"_diag
      u8"      ^^^ .equals_operator"_diag);

  test_parse_and_visit_statement(
      u8"if (x == 'A' || 'B') {}"_sv,  //
      u8"             ^^ Diag_Equals_Does_Not_Distribute_Over_Or.or_operator\n"_diag
      u8"      ^^ .equals_operator"_diag);
}

TEST_F(Test_Error_Equals_Does_Not_Distribute_Over_Or, not_equals) {
  test_parse_and_visit_module(u8"if (x != 'A' || 'B') {}"_sv, no_diags);

  test_parse_and_visit_module(u8"if (x !== 'A' || 'B') {}"_sv, no_diags);
}

TEST_F(Test_Error_Equals_Does_Not_Distribute_Over_Or, null_and_undefined) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (x == 'A' || null) {}"_sv,  //
        u8"             ^^ Diag_Equals_Does_Not_Distribute_Over_Or.or_operator\n"_diag
        u8"      ^^ .equals_operator"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (x == 'A' || undefined) {}"_sv,  //
        u8"             ^^ Diag_Equals_Does_Not_Distribute_Over_Or.or_operator\n"_diag
        u8"      ^^ .equals_operator"_diag);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"undefined"}));
  }

  test_parse_and_visit_statement(
      u8"if (x === 10 || null) {}"_sv,  //
      u8"             ^^ Diag_Equals_Does_Not_Distribute_Over_Or.or_operator\n"_diag
      u8"      ^^^ .equals_operator"_diag);

  test_parse_and_visit_statement(
      u8"if (x === 10 || undefined) {}"_sv,  //
      u8"             ^^ Diag_Equals_Does_Not_Distribute_Over_Or.or_operator\n"_diag
      u8"      ^^^ .equals_operator"_diag);
}

TEST_F(Test_Error_Equals_Does_Not_Distribute_Over_Or, logical_and) {
  test_parse_and_visit_statement(u8"if (x == 'A' && 'B') {}"_sv, no_diags);
}

TEST_F(Test_Error_Equals_Does_Not_Distribute_Over_Or, non_constant) {
  test_parse_and_visit_statement(u8"if (x === 'A' || y) {}"_sv, no_diags);
}

TEST_F(Test_Parse_Warning, warn_on_pointless_string_compare) {
  test_parse_and_visit_statement(u8"s.toLowerCase() == 'banana'"_sv, no_diags);
  test_parse_and_visit_statement(
      u8"s.toLowerCase() == 'BANANA'"_sv,  //
      u8"                ^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
  test_parse_and_visit_statement(u8"s.toUpperCase() == 'BANANA'"_sv, no_diags);
  test_parse_and_visit_statement(
      u8"s.toUpperCase() == 'banana'"_sv,  //
      u8"                ^^ Diag_Pointless_String_Comp_Contains_Lower"_diag);
  test_parse_and_visit_statement(
      u8"s.toLowerCase() == \"BANANA\""_sv,  //
      u8"                ^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
}

TEST_F(Test_Parse_Warning, warn_on_pointless_string_compare_all_operators) {
  {
    for (String8_View op : {u8"=="_sv, u8"==="_sv, u8"!="_sv, u8"!=="_sv}) {
      Test_Parser p(concat(u8"s.toLowerCase() "_sv, op, u8" 'Banana'"_sv),
                    capture_diags);
      p.parse_and_visit_statement();
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Pointless_String_Comp_Contains_Upper,
                  span_operator, u8"s.toLowerCase() "_sv.size(), String8(op)),
          }));
    }
  }
  test_parse_and_visit_statement(u8"s.toLowerCase() || 'BANANA'"_sv, no_diags);
}

TEST_F(Test_Parse_Warning,
       warn_on_pointless_string_compare_function_signatures) {
  test_parse_and_visit_statement(u8"s.tolowercase() == 'BANANA'"_sv, no_diags);
  test_parse_and_visit_statement(u8"s.myToLowerCase() == 'BANANA'"_sv,
                                 no_diags);
  test_parse_and_visit_statement(
      u8"stringBuilder.build().toLowerCase() == 'BANANA'"_sv,  //
      u8"                                    ^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
  test_parse_and_visit_statement(u8"o.arr[0]() == 'BANANA'"_sv, no_diags);
  test_parse_and_visit_statement(
      u8"'BANANA' == s.toLowerCase()"_sv,  //
      u8"         ^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
}

TEST_F(Test_Parse_Warning,
       warn_on_comma_between_member_array_subscript_operators) {
  test_parse_and_visit_expression(
      u8"a[1, 2, 3]"_sv,  //
      u8"      ^ Diag_Misleading_Comma_Operator_In_Index_Operation.comma\n"_diag
      u8" ^ .left_square"_diag);

  test_parse_and_visit_statement(
      u8"a[pow(1, 2), 2]"_sv,  //
      u8"           ^ Diag_Misleading_Comma_Operator_In_Index_Operation.comma\n"_diag
      u8" ^ .left_square"_diag);

  test_parse_and_visit_statement(
      u8"a[b[1967, 1975]]"_sv,  //
      u8"        ^ Diag_Misleading_Comma_Operator_In_Index_Operation.comma\n"_diag
      u8"   ^ .left_square"_diag);

  test_parse_and_visit_expression(u8"a = [1, 2, 3]"_sv, no_diags);
}

TEST_F(Test_Parse_Warning, warn_on_comma_operator_in_conditional_statement) {
  test_parse_and_visit_statement(
      u8"if(false, true){}"_sv,  //
      u8"        ^ Diag_Misleading_Comma_Operator_In_Conditional_Statement"_diag);

  test_parse_and_visit_statement(
      u8"do{i++}while(i < 0, true)"_sv,  //
      u8"                  ^ Diag_Misleading_Comma_Operator_In_Conditional_Statement"_diag);

  test_parse_and_visit_statement(u8"do{i++}while(i < (0, true))"_sv, no_diags);

  test_parse_and_visit_statement(
      u8"for(; i < 5, i < 3; ){}"_sv,  //
      u8"           ^ Diag_Misleading_Comma_Operator_In_Conditional_Statement"_diag);

  test_parse_and_visit_statement(u8"for(let i = 0, j = 0;;){}"_sv, no_diags);

  test_parse_and_visit_statement(u8"for(i = 0, j = 0;;){}"_sv, no_diags);

  test_parse_and_visit_statement(u8"for(;; ++i, ++j){}"_sv, no_diags);

  test_parse_and_visit_statement(
      u8"switch(cond1, cond2){case 1:break;}"_sv,  //
      u8"            ^ Diag_Misleading_Comma_Operator_In_Conditional_Statement"_diag);
}

TEST_F(Test_Parse_Warning,
       warn_on_pointless_string_compare_complex_expressions) {
  test_parse_and_visit_statement(
      u8"if(s.toLowerCase() === 'BANANA') {}"_sv,  //
      u8"                   ^^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
  test_parse_and_visit_expression(
      u8"((s.toLowerCase())) === 'BANANA'"_sv,  //
      u8"                    ^^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
  test_parse_and_visit_expression(
      u8"(((s.toLowerCase())) === ((('BANANA'))))"_sv,  //
      u8"                     ^^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
  test_parse_and_visit_statement(
      u8"s.toLowerCase() == 'BANANA' && s.toUpperCase() !== 'orange'"_sv,  //
      u8"                                               ^^^ Diag_Pointless_String_Comp_Contains_Lower"_diag,  //
      u8"                ^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
  test_parse_and_visit_statement(
      u8"((s.toLowerCase() == 'BANANA') && s.toUpperCase() !== 'orange')"_sv,  //
      u8"                                                  ^^^ Diag_Pointless_String_Comp_Contains_Lower"_diag,  //
      u8"                  ^^ Diag_Pointless_String_Comp_Contains_Upper"_diag);
}

TEST_F(Test_Parse_Warning, warn_on_pointless_string_compare_literals) {
  test_parse_and_visit_statement(u8"s.toLowerCase() == 'Ba\\u006Eana'"_sv,
                                 no_diags);
  test_parse_and_visit_statement(
      u8"s.toLowerCase() == 0xeF || s.toUpperCase() == 0xeF"_sv, no_diags);
  test_parse_and_visit_statement(u8"s.toUpperCase() == 'C:\\User\\tom'"_sv,
                                 no_diags);
}

TEST_F(Test_Parse_Warning,
       warn_on_pointless_strict_compare_against_array_literals) {
  for (String8 op : {u8"===", u8"!=="}) {
    {
      Test_Parser p(concat(u8"x "_sv, op, u8" []"_sv), capture_diags);
      p.parse_and_visit_expression();
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code,
                  Diag_Pointless_Strict_Comp_Against_Empty_Array_Literal,
                  equals_operator, u8"x "_sv.size(), op),
          }));
    }
    {
      Test_Parser p(concat(u8"x "_sv, op, u8" [1, 2, 3]"_sv), capture_diags);
      p.parse_and_visit_expression();
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Pointless_Strict_Comp_Against_Array_Literal,
                  equals_operator, u8"x "_sv.size(), op),
          }));
    }
  }
}

TEST_F(Test_Parse_Warning, warn_on_pointless_compare_against_literals) {
  for (String8 op : {u8"==", u8"!=", u8"===", u8"!=="}) {
    {
      Test_Parser p(concat(u8"x "_sv, op, u8" {}"_sv), capture_diags);
      p.parse_and_visit_expression();
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, Diag_Pointless_Comp_Against_Object_Literal,
                          equals_operator, u8"x "_sv.size(), op),
                  }));
    }
    {
      Test_Parser p(concat(u8"x "_sv, op, u8" class C{}"_sv), capture_diags);
      p.parse_and_visit_expression();
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, Diag_Pointless_Comp_Against_Class_Literal,
                          equals_operator, u8"x "_sv.size(), op),
                  }));
    }
    {
      Test_Parser p(
          concat(u8"x "_sv, op,
                 u8" ((parameter) => { some_object.call(parameter); })"_sv),
          capture_diags);
      p.parse_and_visit_expression();
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, Diag_Pointless_Comp_Against_Arrow_Function,
                          equals_operator, u8"x "_sv.size(), op),
                  }));
    }
    {
      Test_Parser p(concat(u8"x "_sv, op, u8" /some_pattern/a"_sv),
                    capture_diags);
      p.parse_and_visit_expression();
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code,
                  Diag_Pointless_Comp_Against_Regular_Expression_Literal,
                  equals_operator, u8"x "_sv.size(), op),
          }));
    }
  }
}

TEST_F(Test_Parse_Warning,
       warn_on_pointless_compare_against_literals_complex_expressions) {
  test_parse_and_visit_expression(
      u8"({} == {} && (x) === [1, 2, 3]) || ((/pattern/) == y.prop)"_sv,  //
      u8"                                                ^^ Diag_Pointless_Comp_Against_Regular_Expression_Literal.equals_operator"_diag,  //
      u8"                 ^^^ Diag_Pointless_Strict_Comp_Against_Array_Literal"_diag,  //
      u8"    ^^ Diag_Pointless_Comp_Against_Object_Literal.equals_operator"_diag);
  test_parse_and_visit_expression(
      u8"x === y || ({}) != obj.prop"_sv,  //
      u8"                ^^ Diag_Pointless_Comp_Against_Object_Literal.equals_operator"_diag);
}

TEST_F(Test_Parse_Warning, warn_on_pointless_nullish_coalescing_operator) {
  test_parse_and_visit_expression(
      u8"true ?? false"_sv,  //
      u8"     ^^ Diag_Pointless_Nullish_Coalescing_Operator"_diag);
  test_parse_and_visit_expression(
      u8"(a < b) ?? false"_sv,  //
      u8"        ^^ Diag_Pointless_Nullish_Coalescing_Operator"_diag);
  test_parse_and_visit_expression(
      u8"!b ?? false"_sv,  //
      u8"   ^^ Diag_Pointless_Nullish_Coalescing_Operator"_diag);
  test_parse_and_visit_expression(
      u8"'hi' ?? true"_sv,  //
      u8"     ^^ Diag_Pointless_Nullish_Coalescing_Operator"_diag);
  for (String8_View code : {
           u8"s.toLowerCase() ?? false"_sv,
           u8"s ?? false"_sv,
           u8"null ?? false"_sv,
           u8"(foo) ?? false"_sv,
           u8"{}.missingProp ?? false"_sv,
           u8"{}['missingProp'] ?? false"_sv,
           u8"await foo ?? false"_sv,
           u8"void 42 ?? false"_sv,
           u8"bar`hello` ?? false"_sv,
           u8"this ?? false"_sv,
           u8"(2+2 && null) ?? false"_sv,
           u8"(2+2 || null) ?? false"_sv,
           u8"(2+2 , null) ?? false"_sv,
           u8"(2+2 ?? null) ?? false"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code);
    p.parse_and_visit_expression();
  }
}

TEST_F(Test_Parse_Warning, warn_on_variable_assigned_to_self_is_noop) {
  test_parse_and_visit_statement(
      u8"x = x"_sv,  //
      u8"^^^^^ Diag_Variable_Assigned_To_Self_Is_Noop"_diag);
  test_parse_and_visit_statement(
      u8"x = \\u{78}"_sv,  //
      u8"^^^^^^^^^^^ Diag_Variable_Assigned_To_Self_Is_Noop"_diag);
  test_parse_and_visit_statement(
      u8"x = ((x))"_sv,  //
      u8"^^^^^^^^^ Diag_Variable_Assigned_To_Self_Is_Noop"_diag);
  test_parse_and_visit_statement(
      u8"(x) = x"_sv,  //
      u8"^^^^^^^ Diag_Variable_Assigned_To_Self_Is_Noop"_diag);
  test_parse_and_visit_statement(u8"i.x = i.x"_sv, no_diags);
  test_parse_and_visit_statement(u8"x += x"_sv, no_diags);
  test_parse_and_visit_statement(u8"x &&= x"_sv, no_diags);
}

TEST_F(Test_Parse_Warning, warn_on_xor_operation_used_as_exponentiation) {
  test_parse_and_visit_expression(
      u8"2 ^ 8"_sv,  //
      u8"  ^ Diag_Xor_Used_As_Exponentiation.xor_operator"_diag);
  test_parse_and_visit_statement(
      u8"let a = 2 ^ 5"_sv,  //
      u8"          ^ Diag_Xor_Used_As_Exponentiation.xor_operator"_diag);
  test_parse_and_visit_expression(
      u8"10 ^ 5"_sv,  //
      u8"   ^ Diag_Xor_Used_As_Exponentiation.xor_operator"_diag);
  test_parse_and_visit_expression(u8"x ^ a"_sv, no_diags);
  test_parse_and_visit_expression(u8"10 ^ x"_sv, no_diags);
  test_parse_and_visit_expression(u8"3 ^ x"_sv, no_diags);
  test_parse_and_visit_expression(u8"4 ^ 3"_sv, no_diags);
  test_parse_and_visit_expression(u8"(x+2)^a"_sv, no_diags);
}
TEST_F(Test_Parse_Warning, warn_on_unintuitive_precedence_when_using_bitshift) {
  test_parse_and_visit_expression(
      u8"var1 & 0x01 >> 0x02"_sv,
      u8"            ^^ Diag_Unintuitive_Bitshift_Precedence.bitshift_operator\n"_diag
      u8"     ^ .and_operator"_diag);
  test_parse_and_visit_expression(
      u8"var2 & 0x1234 << 0x4321"_sv,
      u8"              ^^ Diag_Unintuitive_Bitshift_Precedence.bitshift_operator\n"_diag
      u8"     ^ .and_operator"_diag);
  test_parse_and_visit_statement(
      u8"const x = a & 10 << 12"_sv,
      u8"                 ^^ Diag_Unintuitive_Bitshift_Precedence.bitshift_operator\n"_diag
      u8"            ^ .and_operator"_diag);
  test_parse_and_visit_expression(u8"0x111 << 0x222 & var1"_sv, no_diags);
  test_parse_and_visit_expression(u8"a&b>>c"_sv, no_diags);
  test_parse_and_visit_expression(u8"x & y << z"_sv, no_diags);
  test_parse_and_visit_expression(u8"0xABCD & 0xDCBA << 0xFFFF"_sv, no_diags);
  test_parse_and_visit_expression(u8"(a & 0o12) >> 0xBEEF"_sv, no_diags);
  test_parse_and_visit_expression(u8"a & (b >> 0xBEEF)"_sv, no_diags);
  test_parse_and_visit_expression(u8"a & b >> c"_sv, no_diags);
}

TEST_F(Test_Parse_Warning, Diag_Fallthrough_Without_Comment_In_Switch) {
  test_parse_and_visit_statement(
      u8"switch(cond1){case 1:\nfoo()\ncase 2:\nbar() //fallthrough\ndefault:}"_sv,  //
      u8"                              ` Diag_Fallthrough_Without_Comment_In_Switch"_diag);
  test_parse_and_visit_statement(
      u8"switch(cond1){case 1:\nfoo()\ncase 2:\nlongBarFn()\ndefault:}"_sv,  //
      u8"                                                    ` Diag_Fallthrough_Without_Comment_In_Switch"_diag,  //
      u8"                              ` Diag_Fallthrough_Without_Comment_In_Switch"_diag);
  // check for false positive
  test_parse_and_visit_statement(
      u8R"(switch(cond1){
        case 1:
        case 2:
        default:
      })"_sv,
      no_diags);
  test_parse_and_visit_statement(
      u8R"(switch(cond1){
      case 1:
        foo()

        //fallthrough
      case 2:
        bar()//fallthrough
      default:})"_sv,
      no_diags);
}

TEST_F(Test_Parse_Warning, early_exit_does_not_trigger_fallthrough_warning) {
  for (String8_View exiting_code : {
           u8"break;"_sv,
           u8"continue;"_sv,
           u8"do { return; } while (true);"_sv,
           u8"for (;;) { return; }"_sv,
           u8"if (false) { } else { return; }"_sv,
           u8"if (true) { return; }"_sv,
           u8"return x;"_sv,
           u8"return;"_sv,
           u8"switch (c) { default: return; }"_sv,
           u8"throw err;"_sv,
           u8"try { return; } catch (e) {}"_sv,
           u8"while (true) { return; }"_sv,
           u8"with (o) { return; }"_sv,
           u8"{ return; }"_sv,
       }) {
    test_parse_and_visit_statement(
        concat(u8"for (;;) { switch (a) { case 1: "_sv, exiting_code,
               u8" default: break; } }"_sv),
        no_diags);
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
