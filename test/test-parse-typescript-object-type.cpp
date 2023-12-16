// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <iterator>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
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

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Object_Type : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Object_Type, empty_object_type) {
  Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
      u8"{}"_sv, no_diags, typescript_options);
  EXPECT_THAT(p.visits, IsEmpty());
  EXPECT_THAT(p.variable_uses, IsEmpty());
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_basic_properties) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ untypedProperty }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ property: Type }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ property: Type, }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ property: Type; }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ p1: Type1, p2: Type2 }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type1", u8"Type2"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ p1: Type1; p2: Type2 }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type1", u8"Type2"}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type,
       properties_allow_trailing_semicolon_or_comma_or_asi) {
  // ASI due to '}':
  test_parse_and_visit_typescript_type_expression(u8"{ f: Type }"_sv, no_diags,
                                                  typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ f2 }"_sv, no_diags,
                                                  typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ m(): Type }"_sv,
                                                  no_diags, typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ m2() }"_sv, no_diags,
                                                  typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ [v]: Type }"_sv,
                                                  no_diags, typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ [v2] }"_sv, no_diags,
                                                  typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ [v3](): Type }"_sv,
                                                  no_diags, typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ [v4]() }"_sv, no_diags,
                                                  typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ (): Type }"_sv, no_diags,
                                                  typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ () }"_sv, no_diags,
                                                  typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ <T>(): Type }"_sv,
                                                  no_diags, typescript_options);
  test_parse_and_visit_typescript_type_expression(u8"{ <T>() }"_sv, no_diags,
                                                  typescript_options);

  test_parse_and_visit_typescript_type_expression(
      u8"{\n"_sv
      u8"  f: Type\n"_sv       // ASI
      u8"  f2\n"_sv            // ASI
      u8"  m(): Type\n"_sv     // ASI
      u8"  m2()\n"_sv          // ASI
      u8"  [v]: Type\n"_sv     // ASI
      u8"  [v2]\n"_sv          // ASI
      u8"  [v3](): Type\n"_sv  // ASI
      u8"  [v4]()\n"_sv        // ASI
      u8"  (): Type\n"_sv      // ASI
      u8"  ()\n"_sv            // ASI
      u8"  <T>(): Type\n"_sv   // ASI
      u8"  <T>()\n"_sv         // ASI
      u8"  lastField;"_sv
      u8"}"_sv,
      no_diags, typescript_options);
  test_parse_and_visit_typescript_type_expression(
      u8"{\n"_sv
      u8"  f: Type;"_sv
      u8"  f2;"_sv
      u8"  m(): Type;"_sv
      u8"  m2();"_sv
      u8"  [v]: Type;"_sv
      u8"  [v2];"_sv
      u8"  [v3](): Type;"_sv
      u8"  [v4]();"_sv
      u8"  (): Type;"_sv
      u8"  ();"_sv
      u8"  <T>(): Type;"_sv
      u8"  <T>();"_sv
      u8"}"_sv,
      no_diags, typescript_options);
  test_parse_and_visit_typescript_type_expression(
      u8"{\n"_sv
      u8"  f: Type,"_sv
      u8"  f2,"_sv
      u8"  m(): Type,"_sv
      u8"  m2(),"_sv
      u8"  [v]: Type,"_sv
      u8"  [v2],"_sv
      u8"  [v3](): Type,"_sv
      u8"  [v4](),"_sv
      u8"  (): Type,"_sv
      u8"  (),"_sv
      u8"  <T>(): Type,"_sv
      u8"  <T>(),"_sv
      u8"}"_sv,
      no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Object_Type, properties_require_terminator) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ p1: Type1 p2: Type2 }"_sv,  //
        u8"           ` Diag_Missing_Separator_Between_Object_Type_Entries"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type1
                              "visit_variable_type_use",  // Type2
                          }));
  }

  {
    // TODO(strager): Improve this diagnostic.
    // Diag_Missing_Separator_Between_Object_Type_Entries would be better.
    test_parse_and_visit_typescript_type_expression(
        u8"{ m() n() }"_sv,                                             //
        u8"     ` Diag_Missing_Semicolon_After_Interface_Method"_diag,  //
        typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type,
       object_type_with_readonly_properties) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ readonly untypedProperty }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ readonly property: Type }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type,
       object_type_with_optional_properties) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ untypedProperty? }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ property?: Type }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ method?(): Type }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // method
                              "visit_variable_type_use",     // Type
                              "visit_exit_function_scope",   // method
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Type"}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_method) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ method() }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // method
                              "visit_exit_function_scope",   // method
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ method(param: Type) }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // method
                              "visit_variable_type_use",     // Type
                              "visit_variable_declaration",  // param
                              "visit_exit_function_scope",   // method
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ method(): ReturnType }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // method
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_function_scope",   // method
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_generic_method) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ method<T>() }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // method
                              "visit_variable_declaration",  // T
                              "visit_exit_function_scope",   // method
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_getter) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ get prop() }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // get prop
                              "visit_exit_function_scope",   // get prop
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ get prop(): ReturnType }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // get prop
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_function_scope",   // get prop
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_setter) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ set prop(v) }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // set prop
                              "visit_variable_declaration",  // v
                              "visit_exit_function_scope",   // set prop
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ set prop(value: Type) }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // set prop
                              "visit_variable_type_use",     // Type
                              "visit_variable_declaration",  // value
                              "visit_exit_function_scope",   // set prop
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_computed_property) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ ['prop'] }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ ['prop']: Type }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_type_use",  // Type
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ ['method']() }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  // method
                              "visit_exit_function_scope",   // method
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ [varName]: Type }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // varName
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"varName", u8"Type"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ [ns.varName]: Type }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // ns
                              "visit_variable_type_use",  // Type
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ns", u8"Type"}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_index_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ [key: KeyType]: PropType }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_declaration",         // key
                              "visit_variable_type_use",            // PropType
                              "visit_exit_index_signature_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({index_signature_param_decl(u8"key"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"KeyType", u8"PropType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_mapped_types) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ [Key in Keys]: PropType }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // Keys
                              "visit_variable_declaration",         // Key
                              "visit_variable_type_use",            // PropType
                              "visit_exit_index_signature_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"Key"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Keys", u8"PropType"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ [Key in Keys as KeyType]: PropType }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_index_signature_scope",  //
                              "visit_variable_type_use",            // Keys
                              "visit_variable_declaration",         // Key
                              "visit_variable_type_use",            // KeyType
                              "visit_variable_type_use",            // PropType
                              "visit_exit_index_signature_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"Key"_sv)}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"Keys", u8"KeyType", u8"PropType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_modified_optional) {
  for (String8 modifier : {u8"-?", u8"+?", u8"?"}) {
    {
      Padded_String code(
          concat(u8"{ [key: KeyType]"_sv, modifier, u8": PropType }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_index_signature_scope",  //
                                "visit_variable_type_use",            // KeyType
                                "visit_variable_declaration",         // key
                                "visit_variable_type_use",  // PropType
                                "visit_exit_index_signature_scope",
                            }));
    }

    {
      Padded_String code(
          concat(u8"{ [Key in Keys]"_sv, modifier, u8": PropType }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_index_signature_scope",  //
                                "visit_variable_type_use",            // Keys
                                "visit_variable_declaration",         // Key
                                "visit_variable_type_use",  // PropType
                                "visit_exit_index_signature_scope",
                            }));
    }

    {
      Padded_String code(concat(u8"{ [Key in Keys as KeyType]"_sv, modifier,
                                u8": PropType }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_index_signature_scope",  //
                                "visit_variable_type_use",            // Keys
                                "visit_variable_declaration",         // Key
                                "visit_variable_type_use",            // KeyType
                                "visit_variable_type_use",  // PropType
                                "visit_exit_index_signature_scope",
                            }));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_modified_readonly) {
  for (String8 modifier : {u8"-readonly", u8"+readonly", u8"readonly"}) {
    {
      Padded_String code(
          concat(u8"{ "_sv, modifier, u8" [key: KeyType]: PropType }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_index_signature_scope",  //
                                "visit_variable_type_use",            // KeyType
                                "visit_variable_declaration",         // key
                                "visit_variable_type_use",  // PropType
                                "visit_exit_index_signature_scope",
                            }));
    }

    {
      Padded_String code(
          concat(u8"{ "_sv, modifier, u8" [Key in Keys]: PropType }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_index_signature_scope",  //
                                "visit_variable_type_use",            // Keys
                                "visit_variable_declaration",         // Key
                                "visit_variable_type_use",  // PropType
                                "visit_exit_index_signature_scope",
                            }));
    }

    {
      Padded_String code(concat(u8"{ "_sv, modifier,
                                u8" [Key in Keys as KeyType]: PropType }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_index_signature_scope",  //
                                "visit_variable_type_use",            // Keys
                                "visit_variable_declaration",         // Key
                                "visit_variable_type_use",            // KeyType
                                "visit_variable_type_use",  // PropType
                                "visit_exit_index_signature_scope",
                            }));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type, object_type_with_call_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ () }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_exit_function_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ (param: ParamType): ReturnType }"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_type_use",     // ParamType
                              "visit_variable_declaration",  // param
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"param"_sv)}));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"ParamType", u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type,
       object_type_with_generic_call_signature) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
        u8"{ <T>(param): ReturnType }"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // T
                              "visit_variable_declaration",  // param
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv),
                                  func_param_decl(u8"param"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"ReturnType"}));
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type,
       object_type_with_keyword_named_properties) {
  for (String8 keyword : keywords) {
    {
      Padded_String code(concat(u8"{ "_sv, keyword, u8" }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, IsEmpty());
    }

    {
      Padded_String code(concat(u8"{ "_sv, keyword, u8"() }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",  //
                                "visit_exit_function_scope",
                            }));
    }

    {
      Padded_String code(concat(u8"{ "_sv, keyword, u8": Type }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_type_use",  // Type
                            }));
    }

    {
      Padded_String code(concat(u8"{ readonly "_sv, keyword, u8": Type }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_type_use",  // Type
                            }));
    }

    {
      Padded_String code(concat(u8"{ "_sv, keyword, u8"?: Type }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_type_use",  // Type
                            }));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Object_Type,
       object_type_with_contextual_keyword_named_index_key) {
  for (String8 keyword :
       contextual_keywords - Dirty_Set<String8>{u8"let", u8"static"}) {
    {
      Padded_String code(concat(u8"{ ["_sv, keyword, u8": T]: T }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({index_signature_param_decl(keyword)}));
    }

    {
      Padded_String code(concat(u8"{ ["_sv, keyword, u8" in T]: T }"_sv));
      SCOPED_TRACE(code);
      Spy_Visitor p = test_parse_and_visit_typescript_type_expression(
          code.string_view(), no_diags, typescript_options);
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(keyword)}));
    }
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
