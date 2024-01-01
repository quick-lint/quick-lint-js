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

namespace quick_lint_js {
namespace {
class Test_Parse_TypeScript_Generic : public Test_Parse_Expression {};

TEST_F(Test_Parse_TypeScript_Generic, single_basic_generic_parameter) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, multiple_basic_generic_parameter) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T1, T2, T3>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T1
                              "visit_variable_declaration",  // T2
                              "visit_variable_declaration",  // T3
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T1"_sv),
                                  generic_param_decl(u8"T2"_sv),
                                  generic_param_decl(u8"T3"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T1, T2, T3,>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T1
                              "visit_variable_declaration",  // T2
                              "visit_variable_declaration",  // T3
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, parameters_require_commas_between) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T1 T2>"_sv,                                               //
        u8"   ` Diag_Missing_Comma_Between_Generic_Parameters"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T1
                              "visit_variable_declaration",  // T2
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T1"_sv),
                                  generic_param_decl(u8"T2"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       parameter_list_does_not_allow_leading_comma) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<, T>"_sv,                                                       //
        u8" ^ Diag_Comma_Not_Allowed_Before_First_Generic_Parameter"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<,,, T>"_sv,                                                       //
        u8"   ^ Diag_Comma_Not_Allowed_Before_First_Generic_Parameter"_diag,  //
        u8"  ^ Diag_Comma_Not_Allowed_Before_First_Generic_Parameter"_diag,   //
        u8" ^ Diag_Comma_Not_Allowed_Before_First_Generic_Parameter"_diag,    //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       parameter_list_must_contain_at_least_one_parameter) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<>"_sv,                                                    //
        u8" ` Diag_TypeScript_Generic_Parameter_List_Is_Empty"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<,>"_sv,                                                   //
        u8" ` Diag_TypeScript_Generic_Parameter_List_Is_Empty"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<,,>"_sv,                                                  //
        u8"  ^ Diag_Multiple_Commas_In_Generic_Parameter_List"_diag,  //
        u8" ` Diag_TypeScript_Generic_Parameter_List_Is_Empty"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, IsEmpty());
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       parameter_list_does_not_allow_multiple_trailing_commas) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T,,>"_sv,                                                  //
        u8"   ^ Diag_Multiple_Commas_In_Generic_Parameter_List"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T , , ,>"_sv,                                                  //
        u8"       ^ Diag_Multiple_Commas_In_Generic_Parameter_List"_diag,  //
        u8"     ^ Diag_Multiple_Commas_In_Generic_Parameter_List"_diag,    //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       parameter_list_does_not_allow_consecutive_interior_commas) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T,,U>"_sv,                                                 //
        u8"   ^ Diag_Multiple_Commas_In_Generic_Parameter_List"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_variable_declaration",  // U
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, parameter_list_extends) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T extends U>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // extends
                              "visit_variable_type_use",     // U
                              "visit_exit_type_scope",       //
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, extends_can_be_cyclic_with_indirection) {
  // See NOTE[TypeScript-cyclic-type].
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends { k: T }>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends Array<T>>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends T[]>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends [T, number, string]>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends [T] | null>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends () => T>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends (param: T) => number>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends new () => T>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends new (param: T) => number>"_sv, no_diags,
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends <U>() => T>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends <U>(param: T) => number>"_sv, no_diags, typescript_options);

  // See NOTE[TypeScript-extends-cycle].
  // No cycle:
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends number extends string ? T : false>"_sv, no_diags,
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends number extends number ? true : T>"_sv, no_diags,
      typescript_options);
  // Cycle:
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends number extends number ? T : false>"_sv, no_diags,
      typescript_options);
  // Cycle:
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends number extends string ? true : T>"_sv, no_diags,
      typescript_options);

  // NOTE(strager): These are not a reference of type 'T', but they look like
  // they are.
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends (T) => number>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends new (T) => number>"_sv, no_diags, typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends <U>(T) => number>"_sv, no_diags, typescript_options);

  // TODO(strager): We should disallow this type. TypeScript allows it, but it's
  // almost certainly a bug to have a tuple type which only can contain itself
  // and no extra state.
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends [T]>"_sv, no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Generic, extends_cannot_be_directly_cyclic) {
  // See NOTE[TypeScript-cyclic-type].
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends T>"_sv,  //
      u8"           ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends \\u{54}>"_sv,  //
      u8"           ^^^^^^^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends (T)>"_sv,  //
      u8"            ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T: T>"_sv,  //
      u8"    ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      u8"  ^ Diag_Unexpected_Colon_After_Generic_Definition"_diag,
      typescript_options);

  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends null | T>"_sv,  //
      u8"                  ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends T | null>"_sv,
      u8"           ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends null & T>"_sv,
      u8"                  ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);

  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends `${T}`>"_sv,
      u8"              ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);

  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends T<number>>"_sv,
      u8"           ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);

  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends T extends number ? true : false>"_sv,
      u8"           ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends number extends T ? true : false>"_sv,
      u8"                          ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);

  test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends keyof T>"_sv,
      u8"                 ^ Diag_Cyclic_TypeScript_Type_Definition.use\n"_diag
      u8" ^ .declaration"_diag
      u8"{.kind=Variable_Kind::_generic_parameter}"_diag,
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Generic,
       extends_can_reference_namespace_with_same_name_as_declared_variable) {
  Spy_Visitor v = test_parse_and_visit_typescript_generic_parameters(
      u8"<T extends T.foo>"_sv, no_diags, typescript_options);
  EXPECT_THAT(v.visits, ElementsAreArray({
                            "visit_variable_declaration",    // T
                            "visit_enter_type_scope",        // extends
                            "visit_variable_namespace_use",  // T (in T.foo)
                            "visit_exit_type_scope",         //
                        }));
}

TEST_F(Test_Parse_TypeScript_Generic, unexpected_colon_in_parameter_extends) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T: U>"_sv,                                                //
        u8"  ^ Diag_Unexpected_Colon_After_Generic_Definition"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // extends
                              "visit_variable_type_use",     // U
                              "visit_exit_type_scope",       //
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, type_parameter_default) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T = U>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // =
                              "visit_variable_type_use",     // U
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"U"}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, type_parameter_default_with_extends) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<T extends U = Def>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // =
                              "visit_variable_type_use",     // Def
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // extends
                              "visit_variable_type_use",     // U
                              "visit_exit_type_scope",       //
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Def", u8"U"}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       type_parameter_extends_visits_appear_after_all_default_visits) {
  // See NOTE[generic-type-parameter-visit-order].

  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<P1 extends E1 = D1, P2 extends E2 = D2>"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // =
                              "visit_variable_type_use",     // D1
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // P1
                              "visit_enter_type_scope",      // =
                              "visit_variable_type_use",     // D2
                              "visit_exit_type_scope",       //
                              "visit_variable_declaration",  // P2
                              // All extends clauses follow:
                              "visit_enter_type_scope",   // extends
                              "visit_variable_type_use",  // E1
                              "visit_exit_type_scope",    //
                              "visit_enter_type_scope",   // extends
                              "visit_variable_type_use",  // E2
                              "visit_exit_type_scope",    //
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"D1", u8"D2", u8"E1", u8"E2"}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, variance_specifiers) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<in T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<out T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<in out T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, variance_specifiers_in_wrong_order) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<out in T>"_sv,  //
        u8"     ^^ Diag_TypeScript_Variance_Keywords_In_Wrong_Order.in_keyword\n"_diag
        u8" ^^^ .out_keyword"_diag,  //
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, duplicate_variance_specifiers) {
  test_parse_and_visit_typescript_generic_parameters(
      u8"<in in T>"_sv,  //
      u8"    ^^ Diag_TypeScript_Variance_Keyword_Repeated.second_keyword\n"_diag  //
      u8" ^^ .first_keyword"_diag,  //
      typescript_options);
  test_parse_and_visit_typescript_generic_parameters(
      u8"<out out T>"_sv,  //
      u8"     ^^^ Diag_TypeScript_Variance_Keyword_Repeated.second_keyword\n"_diag  //
      u8" ^^^ .first_keyword"_diag,  //
      typescript_options);

  test_parse_and_visit_typescript_generic_parameters(
      u8"<in out out out T>"_sv,  //
      u8"            ^^^ Diag_TypeScript_Variance_Keyword_Repeated.second_keyword\n"_diag  //
      u8"    ^^^ .first_keyword"_diag,  //
      u8"        ^^^ Diag_TypeScript_Variance_Keyword_Repeated.second_keyword\n"_diag  //
      u8"    ^^^ .first_keyword"_diag,  //
      typescript_options);
}

TEST_F(Test_Parse_TypeScript_Generic, const_parameter) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<const T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       const_parameter_can_be_mixed_with_variance_in_any_order) {
  for (String8_View test_case : {
           u8"<in const T>"_sv,
           u8"<const in T>"_sv,
           u8"<out const T>"_sv,
           u8"<const out T>"_sv,
           u8"<in out const T>"_sv,
           u8"<in const out T>"_sv,
           u8"<const in out T>"_sv,
       }) {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        test_case, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, const_parameter_can_be_repeated) {
  {
    Spy_Visitor p = test_parse_and_visit_typescript_generic_parameters(
        u8"<const const const T>"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // T
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T"_sv)}));
  }

  test_parse_and_visit_typescript_generic_parameters(
      u8"<const in const out const T>"_sv, no_diags, typescript_options);
}

TEST_F(Test_Parse_TypeScript_Generic,
       parameters_can_be_named_contextual_keywords) {
  for (String8 name :
       Dirty_Set<String8>{
           u8"await",
           u8"undefined",
       } | (contextual_keywords - typescript_builtin_type_keywords -
            typescript_special_type_keywords - typescript_type_only_keywords -
            Dirty_Set<String8>{
                u8"let",
                u8"static",
                u8"yield",
            })) {
    {
      Test_Parser p(concat(u8"<"_sv, name, u8">"_sv), typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_typescript_generic_parameters();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"<in "_sv, name, u8">"_sv), typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_typescript_generic_parameters();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"<out "_sv, name, u8">"_sv), typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_typescript_generic_parameters();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"<in out "_sv, name, u8">"_sv),
                    typescript_options);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_typescript_generic_parameters();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({generic_param_decl(name)}));
    }
  }
}

TEST_F(Test_Parse_TypeScript_Generic, parameter_can_be_named_out) {
  {
    Spy_Visitor v = test_parse_and_visit_typescript_generic_parameters(
        u8"<in out out>"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"out"_sv)}));
  }

  {
    Spy_Visitor v = test_parse_and_visit_typescript_generic_parameters(
        u8"<in out>"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"out"_sv)}));
  }

  {
    Spy_Visitor v = test_parse_and_visit_typescript_generic_parameters(
        u8"<in out out, other>"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"out"_sv),
                                  generic_param_decl(u8"other"_sv)}));
  }

  {
    Spy_Visitor v = test_parse_and_visit_typescript_generic_parameters(
        u8"<in out, other>"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"out"_sv),
                                  generic_param_decl(u8"other"_sv)}));
  }

  {
    Spy_Visitor v = test_parse_and_visit_typescript_generic_parameters(
        u8"<in out out = Def>"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"out"_sv)}));
  }

  {
    Spy_Visitor v = test_parse_and_visit_typescript_generic_parameters(
        u8"<in out = Def>"_sv, no_diags, typescript_options);
    EXPECT_THAT(v.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"out"_sv)}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, function_call_with_generic_arguments) {
  {
    Test_Parser p(u8"foo<T>(p)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    SCOPED_TRACE("'<<' should be split into two tokens");
    Test_Parser p(u8"foo<<Param>() => ReturnType>(p)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // <
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // Param
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                              "visit_exit_type_scope",  // >
                          }));
  }

  {
    Test_Parser p(u8"foo?.<T>(p)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    SCOPED_TRACE("'<<' should be split into two tokens");
    Test_Parser p(u8"foo?.<<Param>() => ReturnType>(p)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
  }

  {
    Test_Parser p(u8"foo<T>`bar`"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var foo)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Test_Parser p(u8"foo<T>`bar${baz}`"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var foo, var baz)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Test_Parser p(u8"foo<T>?.()"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo)");
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, new_with_generic_arguments) {
  {
    Test_Parser p(u8"new Foo<T>;"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Foo)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Test_Parser p(u8"new Foo<T>"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Foo)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    Test_Parser p(u8"new Foo<T>(p)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Foo, var p)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"}));
  }

  {
    SCOPED_TRACE("'<<' should be split into two tokens");
    Test_Parser p(u8"new Foo<<Param>() => ReturnType>()"_sv,
                  typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Foo)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // <
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // Param
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",
                              "visit_exit_type_scope",  // >
                          }));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       variable_reference_with_generic_arguments) {
  struct Test_Case {
    String8_View code;
    const char* expected_ast;
    const Char8* variable_type_use;
  };

  for (const Test_Case& tc : {
           // clang-format off
           Test_Case
           {u8"foo<T> /*EOF*/"_sv,     "var foo", u8"T"},
           {u8"foo<T>;"_sv,            "var foo", u8"T"},
           {u8"[foo<T>]"_sv,           "array(var foo)", u8"T"},
           {u8"(foo<T>)"_sv,           "paren(var foo)", u8"T"},
           {u8"{k: foo<T>}"_sv,        "object(literal: var foo)", u8"T"},
           {u8"foo<T>.prop"_sv,        "dot(var foo, prop)", u8"T"},
           {u8"foo<T>?.prop"_sv,       "dot(var foo, prop)", u8"T"},
           {u8"foo<T>, other"_sv,      "binary(var foo, var other)", u8"T"},
           {u8"f(foo<T>)"_sv,          "call(var f, var foo)", u8"T"},
           {u8"f(foo<T>, other)"_sv,   "call(var f, var foo, var other)", u8"T"},
           {u8"foo<T> ? t : f"_sv,     "cond(var foo, var t, var f)", u8"T"},
           {u8"cond ? foo<T> : f"_sv,  "cond(var cond, var foo, var f)", u8"T"},
           {u8"foo<T> = rhs"_sv,       "assign(var foo, var rhs)", u8"T"},

           {u8"foo<T> ||= rhs"_sv,     "condassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> &&= rhs"_sv,     "condassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> ?\x3f= rhs"_sv,  "condassign(var foo, var rhs)", u8"T"},

           {u8"foo<T> %= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> &= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> **= rhs"_sv,     "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> *= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> += rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> -= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> /= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> <<= rhs"_sv,     "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> >>= rhs"_sv,     "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> >>>= rhs"_sv,    "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> ^= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},
           {u8"foo<T> |= rhs"_sv,      "upassign(var foo, var rhs)", u8"T"},

           // In the following examples, the final keyword is part of the next
           // statement. We're only parsing the expression, and expression
           // parsing stops before the keyword.
           {u8"foo<T> break"_sv,     "var foo", u8"T"},
           {u8"foo<T> case"_sv,      "var foo", u8"T"},
           {u8"foo<T> const"_sv,     "var foo", u8"T"},
           {u8"foo<T> continue"_sv,  "var foo", u8"T"},
           {u8"foo<T> debugger"_sv,  "var foo", u8"T"},
           {u8"foo<T> default"_sv,   "var foo", u8"T"},
           {u8"foo<T> do"_sv,        "var foo", u8"T"},
           {u8"foo<T> else"_sv,      "var foo", u8"T"},
           {u8"foo<T> enum"_sv,      "var foo", u8"T"},
           {u8"foo<T> export"_sv,    "var foo", u8"T"},
           {u8"foo<T> for"_sv,       "var foo", u8"T"},
           {u8"foo<T> if"_sv,        "var foo", u8"T"},
           {u8"foo<T> import"_sv,    "var foo", u8"T"},
           {u8"foo<T> return"_sv,    "var foo", u8"T"},
           {u8"foo<T> switch"_sv,    "var foo", u8"T"},
           {u8"foo<T> throw"_sv,     "var foo", u8"T"},
           {u8"foo<T> try"_sv,       "var foo", u8"T"},
           {u8"foo<T> var"_sv,       "var foo", u8"T"},
           {u8"foo<T> while"_sv,     "var foo", u8"T"},
           {u8"foo<T> with"_sv,      "var foo", u8"T"},
           // clang-format on
       }) {
    SCOPED_TRACE(out_string8(tc.code));
    Test_Parser p(tc.code, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), tc.expected_ast);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_variable_type_use",
                              "visit_exit_type_scope",  // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({tc.variable_type_use}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic, jsx_element) {
  {
    Test_Parser p(u8"<MyComponent<Param> />"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(MyComponent)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",  // <
                              "visit_variable_type_use",
                              "visit_exit_type_scope",  // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Param"_sv}));
  }

  {
    Test_Parser p(u8"<MyComponent<<T>() => ReturnType> />"_sv,
                  typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(MyComponent)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",      // <
                              "visit_enter_function_scope",  //
                              "visit_variable_declaration",  // T
                              "visit_enter_type_scope",      // =>
                              "visit_variable_type_use",     // ReturnType
                              "visit_exit_type_scope",       //
                              "visit_exit_function_scope",   //
                              "visit_exit_type_scope",       // >
                          }));
  }

  {
    // '>>' should be split.
    Test_Parser p(u8"<MyComponent<Param>></MyComponent>"_sv,
                  typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(MyComponent)");
  }

  {
    // '>>>' should be split.
    Test_Parser p(u8"<MyComponent<C<P>>></MyComponent>"_sv,
                  typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(MyComponent)");
  }

  {
    // 'as' should be treated as an attribute name, not a keyword.
    Test_Parser p(u8"<C<T> as={value} />"_sv, typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(C, var value)");
  }

  {
    Test_Parser p(u8"<MyComponent<T> aria-label={label} />"_sv,
                  typescript_jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "jsxelement(MyComponent, var label)");
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       generic_arguments_less_and_greater_are_operators_in_javascript) {
  {
    Test_Parser p(u8"foo<T>(p)"_sv, javascript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var foo, var T, paren(var p))");
    EXPECT_THAT(p.visits, IsEmpty());
  }

  {
    Test_Parser p(u8"foo<<T>()=>{}>(p)"_sv, javascript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(var foo, var T, arrowfunc(), paren(var p))");
  }

  {
    Test_Parser p(u8"foo<T>`bar`"_sv, javascript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var foo, var T, literal)");
  }

  {
    Test_Parser p(u8"foo<T>`bar${baz}`"_sv, javascript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var foo, var T, template(var baz))");
  }

  {
    Test_Parser p(u8"foo<<T>() => number>`bar${baz}`"_sv, javascript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var foo, var T, arrowfunc())");
  }

  {
    Test_Parser p(u8"new Foo<T>;"_sv, javascript_options, capture_diags);
    Expression* ast = p.parse_expression();
    // FIXME(#557): Precedence is incorrect.
    EXPECT_EQ(summarize(ast), "new(binary(var Foo, var T, missing))");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"new Foo<T>(p);"_sv, javascript_options);
    Expression* ast = p.parse_expression();
    // FIXME(#557): Precedence is incorrect.
    EXPECT_EQ(summarize(ast), "new(binary(var Foo, var T, paren(var p)))");
  }
}

// FIXME(#690): On second thought, I think treating less-greater as operators by
// default is a bad plan. TypeScript parses foo<T>{} as < and > operations, but
// also has type errors when using > with an object literal or when mixing < and
// >.
TEST_F(Test_Parse_TypeScript_Generic,
       less_and_greater_are_operators_by_default) {
  struct Test_Case {
    String8_View code;
    const char* expected_ast;
  };

  for (const Test_Case& tc : {
           // clang-format off
           Test_Case
           {u8"foo<T> rhs"_sv,           "binary(var foo, var T, var rhs)"},
           {u8"foo<T> delete x"_sv,      "binary(var foo, var T, delete(var x))"},
           {u8"foo<T> class {}"_sv,      "binary(var foo, var T, class)"},
           {u8"foo<T> function(){}"_sv,  "binary(var foo, var T, function)"},
           {u8"foo<T> {}"_sv,            "binary(var foo, var T, object())"},
           {u8"foo<T> []"_sv,            "binary(var foo, var T, array())"},
           {u8"foo<T> /regexp/"_sv,      "binary(var foo, var T, literal)"},

           // The 'x' is part of the next statement.
           {u8"foo<T>\n let\n x"_sv,             "binary(var foo, var T, var let)"},
           {u8"foo<T>\n interface\n x\n {}"_sv,  "binary(var foo, var T, var interface)"},
           // clang-format on
       }) {
    SCOPED_TRACE(out_string8(tc.code));
    Test_Parser p(tc.code, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), tc.expected_ast);
    EXPECT_THAT(p.variable_uses, IsEmpty());
    EXPECT_THAT(p.visits,
                ::testing::AnyOf(IsEmpty(),
                                 ElementsAreArray({
                                     "visit_enter_class_scope",       //
                                     "visit_enter_class_scope_body",  //
                                     "visit_exit_class_scope",
                                 }),  //
                                 ElementsAreArray({
                                     "visit_enter_function_scope",       //
                                     "visit_enter_function_scope_body",  //
                                     "visit_exit_function_scope",
                                 })))
        << "there should be no generic arguments (visit_variable_type_use)";
  }
}

TEST_F(
    Test_Parse_TypeScript_Generic,
    greater_equal_ending_generic_argument_list_requires_space_in_expression) {
  // TypeScript does not split '>=' into '>' and '='. This will always result in
  // an error:
  //
  // * (A<B >= Z) is always a type error because booleans ('A<B' and 'Z') cannot
  //   be compared using >= in TypeScript.
  // * (A<B<C >>= Z) is always a type error because 'A<B' and 'C' cannot be
  //   compared using '<', and is always an error because you cannot assign to
  //   'A<B<C'.
  // * (A<B<C<D >>>= Z) is always an error like with (A<B<C >>= Z).
  //
  // quick-lint-js does split '>=', but it should report a helpful diagnostic
  // (instead of ugly type errors like TypeScript emits).
  //
  // See NOTE[typescript-generic-expression-token-splitting].

  {
    Test_Parser p(u8"foo<T>= rhs"_sv, typescript_options, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var foo, var rhs)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"_sv}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ^^ Diag_TypeScript_Requires_Space_Between_Greater_And_Equal"_diag,
        });
  }

  {
    Test_Parser p(u8"foo<T<U>>= rhs"_sv, typescript_options, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var foo, var rhs)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_variable_type_use",  // U
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T"_sv, u8"U"_sv}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"        ^^ Diag_TypeScript_Requires_Space_Between_Greater_And_Equal"_diag,
        });
  }

  {
    Test_Parser p(u8"foo<T<U<V>>>= rhs"_sv, typescript_options, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var foo, var rhs)");
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_type_scope",   // <
                              "visit_variable_type_use",  // T
                              "visit_variable_type_use",  // U
                              "visit_variable_type_use",  // V
                              "visit_exit_type_scope",    // >
                          }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"T"_sv, u8"U"_sv, u8"V"_sv}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"           ^^ Diag_TypeScript_Requires_Space_Between_Greater_And_Equal"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       unambiguous_generic_arguments_are_parsed_in_javascript) {
  {
    Test_Parser p(u8"foo?.<T>(p)"_sv, javascript_options, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ^ Diag_TypeScript_Generics_Not_Allowed_In_JavaScript"_diag,
        });
  }

  {
    Test_Parser p(u8"foo?.<<T>() => void>(p)"_sv, javascript_options,
                  capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var foo, var p)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ^ Diag_TypeScript_Generics_Not_Allowed_In_JavaScript"_diag,
        });
  }
}

TEST_F(Test_Parse_TypeScript_Generic, class_can_extend_generic) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C extends Base<string> {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_type_scope",        // <
                              "visit_exit_type_scope",         // >
                              "visit_variable_use",            // Base
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Base"_sv}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"class C extends Base<string> implements I {}"_sv, no_diags,
        typescript_options);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_type_scope",        // <
                              "visit_exit_type_scope",         // >
                              "visit_variable_use",            // Base
                              "visit_variable_type_use",       // I
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Base"_sv, u8"I"_sv}));
  }
}

TEST_F(
    Test_Parse_TypeScript_Generic,
    extending_or_implementing_generic_allows_newline_before_generic_arguments) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I extends A\n<B> {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"A", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C implements I\n<B> {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"I", u8"B"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C extends Base\n<T> {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"Base"}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       newline_is_allowed_before_generic_parameters_except_after_async) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f\n<T>() {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"), function_decl(u8"f")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class C\n<T> {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"), class_decl(u8"C")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"interface I\n<T> {}"_sv, no_diags, typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({interface_decl(u8"I"), generic_param_decl(u8"T")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"type Identity\n<T> = T;"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({type_alias_decl(u8"Identity"),
                                  generic_param_decl(u8"T")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let f:\n<T>() => RetType;"_sv, no_diags, typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"), let_noinit_decl(u8"f")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let f =\n<T>() => value;"_sv, no_diags, typescript_options);
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({generic_param_decl(u8"T"), let_init_decl(u8"f")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"<Component\n<T> />;"_sv, no_diags, typescript_jsx_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"T"_sv, u8"Component"_sv}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       newline_is_allowed_between_async_and_generic_arguments) {
  {
    Test_Parser p(u8"async\n<T>(param)"_sv, typescript_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var async, var param)");
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       newline_is_not_allowed_between_async_and_generic_arrow_parameters) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async\n<T>() => {}"_sv,
        u8"             ^^ Diag_Newline_Not_Allowed_Between_Async_And_Parameter_List.arrow\n"_diag
        u8"^^^^^ .async"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T")}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async\n<T>(): ReturnType => {}"_sv,
        u8"                         ^^ Diag_Newline_Not_Allowed_Between_Async_And_Parameter_List.arrow\n"_diag
        u8"^^^^^ .async"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({generic_param_decl(u8"T")}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       generic_in_expression_allows_newline_before_generic_arguments) {
  {
    Spy_Visitor p = test_parse_and_visit_expression(
        u8"new C\n<T>()"_sv, no_diags, typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"C"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"f\n<T>()"_sv, no_diags,
                                                    typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"f"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_expression(u8"f?.\n<T>()"_sv, no_diags,
                                                    typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"T", u8"f"}));
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       newline_is_not_allowed_before_generic_arguments_in_type) {
  {
    // TypeScript triggers ASI, but we shouldn't.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(x: C\n<T>) {}"_sv,  //
        u8"                 ^ Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type.less"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C", u8"T"}));
  }

  {
    // ASI should not trigger.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function f(x: C\n<<T>() => ReturnType>) {}"_sv,  //
        u8"                 ^ Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type.less"_diag,
        typescript_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"C", u8"ReturnType"}));
  }

  {
    // ASI should insert a semicolon between 'C' and '<':
    //
    // TODO(strager): Intelligently report
    // Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type instead of
    // inserting a semicolon.
    test_parse_and_visit_module(
        u8"let f = (): C\n<number> => {};"_sv,
        u8"Diag_TypeScript_Type_Annotation_In_Expression"_diag,
        u8"Diag_Missing_Arrow_Function_Parameter_List"_diag,
        u8"Diag_Missing_Expression_Between_Parentheses"_diag,
        typescript_options);
  }

  {
    // ASI should insert a semicolon between 'C' and '<':
    //
    // TODO(strager): Intelligently report
    // Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type instead of
    // inserting a semicolon.
    test_parse_and_visit_module(
        u8"let f: C\n<number>;"_sv,
        u8"Diag_Missing_Expression_After_Angle_Type_Assertion"_diag,
        typescript_options);
  }

  {
    // TypeScript triggers ASI, but we shouldn't.
    test_parse_and_visit_module(
        u8"function f(): C\n<number> {}"_sv,  //
        u8"                 ^ Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type.less"_diag,
        typescript_options);
  }

  {
    // TypeScript triggers ASI, but we shouldn't.
    test_parse_and_visit_module(
        u8"class C { f(): C\n<number> {} }"_sv,  //
        u8"                  ^ Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type.less"_diag,
        typescript_options);
  }

  {
    // ASI should insert a semicolon between 'C' and '<':
    //
    // TODO(strager): Intelligently report
    // Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type instead of
    // inserting a semicolon.
    test_parse_and_visit_module(
        u8"let f = (): C\n<number> => {};"_sv,
        u8"Diag_TypeScript_Type_Annotation_In_Expression"_diag,
        u8"Diag_Missing_Arrow_Function_Parameter_List"_diag,
        u8"Diag_Missing_Expression_Between_Parentheses"_diag,
        typescript_options);
  }

  {
    // ASI should insert a semicolon between 'C' and '<':
    //
    // TODO(strager): Intelligently report
    // Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type instead of
    // inserting a semicolon.
    test_parse_and_visit_module(
        u8"declare function f(): C\n<number>;"_sv,
        u8"Diag_Missing_Expression_After_Angle_Type_Assertion"_diag,
        typescript_options);
  }

  {
    // See TODO[declare-namespace-function-ASI].
    test_parse_and_visit_module(
        u8"declare namespace ns { function f(): C\n<number>; }"_sv,
        u8"Diag_Declare_Namespace_Cannot_Contain_Statement"_diag,
        u8"Diag_Missing_Expression_After_Angle_Type_Assertion"_diag,
        typescript_options);
  }

  {
    // ASI should insert a semicolon between 'f' and '<':
    test_parse_and_visit_module(
        u8"let g = x ? (t) : f\n<number> => {};"_sv,  //
        u8"                              ^^ Diag_Missing_Arrow_Function_Parameter_List.arrow"_diag,  //
        typescript_options);
  }

  {
    // ASI should insert a semicolon between 'C' and '<':
    test_parse_and_visit_module(
        u8"type A = C\n<T>;"_sv,  //
        u8"               ` Diag_Missing_Expression_After_Angle_Type_Assertion"_diag,  //
        typescript_options);
  }

  {
    // ASI should insert a semicolon between 'C' and '<':
    test_parse_and_visit_module(
        u8"type A = typeof C\n<T>;"_sv,  //
        u8"                      ` Diag_Missing_Expression_After_Angle_Type_Assertion"_diag,  //
        typescript_options);
  }

  {
    // ASI should insert a semicolon between 'C' and '<':
    test_parse_and_visit_module(
        u8"type A = import('mod').C\n<T>;"_sv,  //
        u8"                             ` Diag_Missing_Expression_After_Angle_Type_Assertion"_diag,  //
        typescript_options);
  }

  {
    // '<N>' is not a generic argument list. ASI should insert a semicolon
    // between 'C' and '<N>'.
    test_parse_and_visit_module(
        u8"interface C {\n"_sv
        u8"  f(): C\n"_sv  // ASI.
        u8"  <N>(): RT;\n"_sv
        u8"}"_sv,
        no_diags, typescript_options);
  }

  {
    // TypeScript triggers ASI, but we shouldn't.
    test_parse_and_visit_module(
        u8"abstract class C { abstract f(): C\n<N>; }"_sv,
        u8"                                    ^ Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type.less"_diag,
        typescript_options);
  }
}

TEST_F(Test_Parse_TypeScript_Generic,
       newline_is_not_allowed_after_in_out_const_modifiers) {
  {
    test_parse_and_visit_module(
        u8"class C<in\nT> {}"_sv,  //
        u8"        ^^ Diag_Newline_Not_Allowed_After_In_Out_Const_Modifiers.modifier"_diag,  //
        typescript_options);
    test_parse_and_visit_module(
        u8"class C<out\nT> {}"_sv,  //
        u8"        ^^^ Diag_Newline_Not_Allowed_After_In_Out_Const_Modifiers.modifier"_diag,  //
        typescript_options);
    test_parse_and_visit_module(
        u8"class C<const\nT> {}"_sv,  //
        u8"        ^^^^^ Diag_Newline_Not_Allowed_After_In_Out_Const_Modifiers.modifier"_diag,  //
        typescript_options);
  }

  {
    test_parse_and_visit_module(u8"class C<in T> {}"_sv, no_diags,
                                typescript_options);
    test_parse_and_visit_module(u8"class C<out T> {}"_sv, no_diags,
                                typescript_options);
    test_parse_and_visit_module(u8"class C<const T> {}"_sv, no_diags,
                                typescript_options);
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
