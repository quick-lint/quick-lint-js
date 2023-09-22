// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Class_Overload : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Class_Overload, method_overload_signatures) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n"_sv
        u8"  f();\n"_sv
        u8"  f() {}\n"_sv
        u8"}",
        no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",          // C
                              "visit_enter_class_scope_body",     // {
                              "visit_enter_function_scope",       // f #0
                              "visit_exit_function_scope",        // f #0
                              "visit_enter_function_scope",       // f #1
                              "visit_enter_function_scope_body",  // {
                              "visit_exit_function_scope",        // }
                              "visit_property_declaration",       // f
                              "visit_exit_class_scope",           // }
                              "visit_variable_declaration",       // C
                          }));
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }

  {
    // ASI
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n"_sv
        u8"  f()\n"_sv
        u8"  f() {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n"_sv
        u8"  f();\n"_sv
        u8"  \\u{66}() {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n"_sv
        u8"  \\u{66}();\n"_sv
        u8"  f() {}\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }

  for (String8_View function_name : contextual_keywords) {
    Spy_Visitor p = test_parse_and_visit_statement(
        concat(u8"class C { "_sv, function_name, u8"(); "_sv, function_name,
               u8"() {} }"_sv),
        no_diags, typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({function_name}));
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       extra_semicolons_between_method_overload_signatures_are_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n  f();;\n  f() {}\n}",
        u8"                 ^ Diag_Unexpected_Semicolon_After_Overload_Signature.extra_semicolon\n"_diag
        u8"                ^ .original_semicolon"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f();;; f() {} }",
        u8"              ^ Diag_Unexpected_Semicolon_After_Overload_Signature.extra_semicolon\n"_diag
        u8"             ^ .original_semicolon"_diag,
        u8"               ^ Diag_Unexpected_Semicolon_After_Overload_Signature.extra_semicolon\n"_diag
        u8"             ^ .original_semicolon"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       semicolon_is_required_after_overload_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f()  f() {} }",
        u8"             ` Diag_Missing_Semicolon_After_TypeScript_Method_Overload_Signature"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       method_overload_signature_with_wrong_name) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(); g() {} }"_sv,
        u8"               ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.function_name\n"_diag
        u8"          ^ .overload_name"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"g"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(); g(); h() {} }"_sv,
        u8"                    ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.function_name\n"_diag
        u8"               ^ .overload_name"_diag,
        u8"                    ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.function_name\n"_diag
        u8"          ^ .overload_name"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"h"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(); g(); f() {} }"_sv,
        u8"                    ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.function_name\n"_diag
        u8"               ^ .overload_name"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { g(); f(); f() {} }"_sv,
        u8"                    ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.function_name\n"_diag
        u8"          ^ .overload_name"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }

  {
    // Missing method body is more likely, so don't report
    // Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n  f()\n  g() {} }"_sv,
        u8"                ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"f"_sv, u8"g"_sv}));
  }

  {
    // Missing method body is more likely, so don't report
    // Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n  f()\n  async\n  g() { await(myPromise); } }"_sv,
        u8"                    ^^^^^ Diag_Newline_Not_Allowed_Between_Modifier_And_Method_Name.modifier"_diag,
        u8"                ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"_sv}))
        << "'async' should be an operator, not a variable reference or field "
           "declaration";
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"f"_sv, u8"g"_sv}));
  }

  // Missing method body is more likely for a getter or setter, so don't report
  // Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.
  test_parse_and_visit_statement(
      u8"class C { get p(); get q() {} }"_sv,  //
      u8"                 ` Diag_Missing_Function_Body"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { set p(v); set q(v) {} }"_sv,  //
      u8"                  ` Diag_Missing_Function_Body"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       mixed_computed_name_and_normal_name_is_allowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { 'method'(); method() {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(); 'method'() {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { ['method'](); method() {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { ['method'](); ['method']() {} }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({std::nullopt}));
  }
}

// TODO[TypeScript-overload-signature-with-computed-property]: Report
// Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name if two
// computed properties (or string or number literal properties) have different
// names according to TypeScript's non-trivial expression equality rules.

TEST_F(Test_Parse_TypeScript_Class_Overload,
       method_overload_signature_with_wrong_computed_name) {
  // HACK(strager): The parser does not handle computed names intelligently. See
  // TODO[TypeScript-overload-signature-with-computed-property]. For now, if the
  // function's name is computed, the parser treats the first non-computed
  // overload name as the correct name.
  test_parse_and_visit_statement(
      u8"class C { f(); g(); 'f'() {} }"_sv,
      u8"               ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.overload_name\n"_diag
      u8"          ^ .function_name"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { f(); g(); 'g'() {} }"_sv,
      u8"               ^ Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name.overload_name\n"_diag
      u8"          ^ .function_name"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       method_overload_signature_with_newline_after_modifier) {
  {
    // Normally, we would treat 'async' as a field name here. However, we
    // know that there's a syntax error anyway (missing function body), so
    // Diag_Newline_Not_Allowed_Between_Async_And_Method_Name seems more
    // helpful.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n  f()\n  async\n  f() { await(myPromise); } }"_sv,
        u8"                    ^^^^^ Diag_Newline_Not_Allowed_Between_Modifier_And_Method_Name.modifier"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myPromise"_sv}))
        << "await should be treated as an operator";
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       method_overload_signature_declaration_can_be_async) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { async f(); f() { await(myValue); } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"await"_sv, u8"myValue"_sv}))
        << "f's body should be parsed as in a normal function";
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       function_with_overload_signatures_can_be_async) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(); async f() { await(myValue); } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"_sv}))
        << "f's body should be parsed as in an async function";
  }

  test_parse_and_visit_statement(
      u8"class C { async f(); async f() { await(myValue); } }"_sv, no_diags,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       method_overload_signature_declaration_cannot_have_generator_star) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { *f(); *f() { yield(myValue); } }"_sv,
        u8"          ^ Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}))
        << "f's body should be parsed as in a generator function";
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { *f(a); *f(a, b); *f(...args) { yield(myValue); } }"_sv,
        u8"                 ^ Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star"_diag,
        u8"          ^ Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { *f(a); f(a, b); f(...args) { yield(myValue); } }"_sv,
        u8"          ^ Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"yield", u8"myValue"}))
        << "'yield' should not be a keyword in the implementation";
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       method_with_overload_signatures_can_be_generator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(); *f() { yield(myValue); } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"_sv}))
        << "f's body should be parsed as in a generator function";
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       method_with_overload_signatures_can_be_async_generator) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { f(); async *f() { yield(await(myValue)); } }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"myValue"_sv}))
        << "f's body should be parsed as in an async generator function";
  }

  test_parse_and_visit_statement(
      u8"class C { async f(); async *f() { yield(await(myValue)); } }"_sv,
      no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       field_after_overload_signature_is_invalid) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(); field; }"_sv,
        u8"                  ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // C
                              "visit_enter_class_scope_body",  // {
                              "visit_enter_function_scope",    // method
                              "visit_exit_function_scope",     // method
                              "visit_property_declaration",    // method
                              "visit_property_declaration",    // field
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                          }));
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"method"_sv, u8"field"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(); field = null; }"_sv,
        u8"                  ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"method"_sv, u8"field"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(); field: T; }"_sv,
        u8"                  ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray({u8"method"_sv, u8"field"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n  method();\n  field\n  otherMethod() {} }"_sv,
        u8"                     ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(
        p.property_declarations,
        ElementsAreArray({u8"method"_sv, u8"field"_sv, u8"otherMethod"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C {\n  method();\n  field\n  ['otherMethod']() {} }"_sv,
        u8"                     ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations,
                ElementsAreArray<std::optional<String8_View>>(
                    {u8"method"_sv, u8"field"_sv, std::nullopt}));
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       overload_signature_at_end_is_invalid) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method(); }"_sv,
        u8"                  ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"_sv}));
  }

  // The extra semicolon should not lead to
  // Diag_Unexpected_Semicolon_After_Overload_Signature.
  test_parse_and_visit_statement(
      u8"class C { method();; }",
      u8"                  ` Diag_Missing_Function_Body"_diag,
      typescript_options);

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C { method() }"_sv,
        u8"                  ` Diag_Missing_Function_Body"_diag,
        typescript_options);
    EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"method"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       overload_signatures_can_be_generic) {
  test_parse_and_visit_statement(u8"class C { m<T>(); m<T>() {} }"_sv, no_diags,
                                 typescript_options);

  // Generic parameters don't need to match.
  test_parse_and_visit_statement(u8"class C { m<T>(); m<U>() {} }"_sv, no_diags,
                                 typescript_options);
  test_parse_and_visit_statement(u8"class C { m(); m<U>() {} }"_sv, no_diags,
                                 typescript_options);
  test_parse_and_visit_statement(u8"class C { m<T>(); m() {} }"_sv, no_diags,
                                 typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       overload_signatures_are_not_allowed_on_getters_or_setters) {
  test_parse_and_visit_statement(
      u8"class C { get p(); get p() {} }"_sv,  //
      u8"                   ^^^ Diag_Getter_Or_Setter_Cannot_Have_TypeScript_Overload_Signature"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { get p(); p() {} }"_sv,  //
      u8"          ^^^ Diag_Class_Modifier_Not_Allowed_On_TypeScript_Overload_Signature"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { p(); get p() {} }"_sv,  //
      u8"               ^^^ Diag_Getter_Or_Setter_Cannot_Have_TypeScript_Overload_Signature"_diag,
      typescript_options);

  test_parse_and_visit_statement(
      u8"class C { set p(v); set p(v) {} }"_sv,  //
      u8"                    ^^^ Diag_Getter_Or_Setter_Cannot_Have_TypeScript_Overload_Signature"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       overload_signatures_must_have_matching_optionality) {
  test_parse_and_visit_statement(
      u8"class C { m?(); m() {} }"_sv,  //
      u8"                 ` Diag_Class_Modifier_Missing_On_Method_With_TypeScript_Overload_Signature.missing_method_modifier\n"_diag
      u8"           ^ .signature_modifier"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { m(); m?() {} }"_sv,  //
      u8"                ^ Diag_Class_Modifier_Missing_On_TypeScript_Overload_Signature.method_modifier\n"_diag
      u8"           ` .missing_signature_modifier"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       overload_signatures_must_have_matching_static) {
  test_parse_and_visit_statement(
      u8"class C { static m(); m() {} }"_sv,  //
      u8"                      ` Diag_Class_Modifier_Missing_On_Method_With_TypeScript_Overload_Signature.missing_method_modifier\n"_diag
      u8"          ^^^^^^ .signature_modifier"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { m(); static m() {} }"_sv,  //
      u8"               ^^^^^^ Diag_Class_Modifier_Missing_On_TypeScript_Overload_Signature.method_modifier\n"_diag
      u8"          ` .missing_signature_modifier"_diag,
      typescript_options);
}

// TODO(strager): Check 'abstract' conflicts:
//
//     abstract class C { abstract m(); m(); }           // Invalid.
//     abstract class C { m(); abstract m(); }           // Invalid.
//     abstract class C { abstract m(); abstract m(); }  // OK.
//     abstract class C { abstract m(); abstract g(); }  // OK.

TEST_F(Test_Parse_TypeScript_Class_Overload,
       overload_signatures_may_omit_public) {
  test_parse_and_visit_statement(u8"class C { m(); public m() {} }"_sv,
                                 no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       public_overload_signature_allowed_on_method_without_public) {
  test_parse_and_visit_statement(u8"class C { public m(); m() {} }"_sv,
                                 no_diags, typescript_options);
}

TEST_F(
    Test_Parse_TypeScript_Class_Overload,
    overload_signatures_of_private_or_protected_method_must_have_access_specifier) {
  test_parse_and_visit_statement(
      u8"class C { m(); private m() {} }"_sv,  //
      u8"               ^^^^^^^ Diag_Class_Modifier_Missing_On_TypeScript_Overload_Signature.method_modifier\n"_diag
      u8"          ` .missing_signature_modifier"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { m(); protected m() {} }"_sv,  //
      u8"               ^^^^^^^^^ Diag_Class_Modifier_Missing_On_TypeScript_Overload_Signature.method_modifier\n"_diag
      u8"          ` .missing_signature_modifier"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       overload_signatures_must_have_matching_access_specifiers) {
  test_parse_and_visit_statement(
      u8"class C { public m(); private m() {} }"_sv,  //
      u8"                      ^^^^^^^ Diag_TypeScript_Overload_Signature_Access_Specifier_Mismatch.method_access_specifier\n"_diag
      u8"          ^^^^^^ .signature_access_specifier"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { private m(); public m() {} }"_sv,  //
      u8"                       ^^^^^^ Diag_TypeScript_Overload_Signature_Access_Specifier_Mismatch.method_access_specifier\n"_diag
      u8"          ^^^^^^^ .signature_access_specifier"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { protected m(); public m() {} }"_sv,  //
      u8"                         ^^^^^^ Diag_TypeScript_Overload_Signature_Access_Specifier_Mismatch.method_access_specifier\n"_diag
      u8"          ^^^^^^^^^ .signature_access_specifier"_diag,
      typescript_options);
  test_parse_and_visit_statement(
      u8"class C { public m(); private m(); protected m() {} }"_sv,  //
      u8"                                   ^^^^^^^^^ Diag_TypeScript_Overload_Signature_Access_Specifier_Mismatch.method_access_specifier\n"_diag
      u8"                      ^^^^^^^ .signature_access_specifier"_diag,
      u8"                                   ^^^^^^^^^ Diag_TypeScript_Overload_Signature_Access_Specifier_Mismatch.method_access_specifier\n"_diag
      u8"          ^^^^^^ .signature_access_specifier"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Class_Overload,
       interfaces_do_not_interpret_method_overload_signatures) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"interface I {\n"_sv
      u8"  f();\n"_sv
      u8"  f();\n"_sv
      u8"}",
      no_diags, typescript_options);
  EXPECT_THAT(p.property_declarations, ElementsAreArray({u8"f"_sv, u8"f"_sv}))
      << "the first f declaration should not be interpreted as an overload "
         "signature";
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
