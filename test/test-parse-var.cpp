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

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAreArray;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
class Test_Parse_Var : public Test_Parse_Expression {};

TEST_F(Test_Parse_Var, parse_simple_let) {
  {
    Test_Parser p(u8"let x"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }

  {
    Test_Parser p(u8"let a, b"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {let_noinit_decl(u8"a"_sv), let_noinit_decl(u8"b"_sv)}));
  }

  {
    Test_Parser p(u8"let a, b, c, d, e, f, g"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 7);
    EXPECT_EQ(p.variable_declarations[0].name, u8"a");
    EXPECT_EQ(p.variable_declarations[1].name, u8"b");
    EXPECT_EQ(p.variable_declarations[2].name, u8"c");
    EXPECT_EQ(p.variable_declarations[3].name, u8"d");
    EXPECT_EQ(p.variable_declarations[4].name, u8"e");
    EXPECT_EQ(p.variable_declarations[5].name, u8"f");
    EXPECT_EQ(p.variable_declarations[6].name, u8"g");
    for (const auto& declaration : p.variable_declarations) {
      EXPECT_EQ(declaration.kind, Variable_Kind::_let);
    }
  }

  {
    Test_Parser p(u8"let first; let second"_sv, capture_diags);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"first");
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 2);
    EXPECT_EQ(p.variable_declarations[0].name, u8"first");
    EXPECT_EQ(p.variable_declarations[1].name, u8"second");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_Var, parse_simple_var) {
  Test_Parser p(u8"var x"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({var_noinit_decl(u8"x"_sv)}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(Test_Parse_Var, parse_simple_const) {
  Test_Parser p(u8"const x = null"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({const_init_decl(u8"x"_sv)}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(Test_Parse_Var, parse_const_with_no_initializers) {
  Spy_Visitor p = test_parse_and_visit_statement(
      u8"const x;"_sv,  //
      u8"      ^ Diag_Missing_Initializer_In_Const_Declaration"_diag);
  ASSERT_EQ(p.variable_declarations.size(), 1);
  EXPECT_THAT(p.variable_declarations,
              ElementsAreArray({const_noinit_decl(u8"x"_sv)}));
}

TEST_F(Test_Parse_Var, let_asi) {
  {
    Test_Parser p(u8"let x\ny"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // y
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }
}

TEST_F(Test_Parse_Var, parse_let_with_initializers) {
  {
    Test_Parser p(u8"let x = 2"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"x"_sv)}));
  }

  {
    Test_Parser p(u8"let x = 2, y = 3"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.variable_declarations,
        ElementsAreArray({let_init_decl(u8"x"_sv), let_init_decl(u8"y"_sv)}));
  }

  {
    Test_Parser p(u8"let x = other, y = x"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 2);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_EQ(p.variable_declarations[1].name, u8"y");
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"other", u8"x"}));
  }

  {
    Test_Parser p(u8"let x = y in z;"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y", u8"z"}));
  }
}

TEST_F(Test_Parse_Var, parse_let_with_object_destructuring) {
  {
    Test_Parser p(u8"let {x} = 2"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"x"_sv)}));
  }

  {
    Test_Parser p(u8"let {x, y, z} = 2"_sv);
    p.parse_and_visit_statement();
    ASSERT_EQ(p.variable_declarations.size(), 3);
    EXPECT_EQ(p.variable_declarations[0].name, u8"x");
    EXPECT_EQ(p.variable_declarations[1].name, u8"y");
    EXPECT_EQ(p.variable_declarations[2].name, u8"z");
  }

  {
    Test_Parser p(u8"let {key: variable} = 2"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"variable"_sv)}));
  }

  {
    Test_Parser p(u8"let {} = x;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  }

  {
    Test_Parser p(u8"let {key = defaultValue} = x;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // x
                              "visit_variable_use",          // defaultValue
                              "visit_variable_declaration",  // key
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"key"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x",  //
                                                   u8"defaultValue"}));
  }
}

TEST_F(Test_Parse_Var, parse_let_with_array_destructuring) {
  {
    Test_Parser p(u8"let [first, second] = xs;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // x
                              "visit_variable_declaration",  // first
                              "visit_variable_declaration",  // second
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"first"_sv),
                                  let_init_decl(u8"second"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs"}));
  }
}

TEST_F(Test_Parse_Var, let_does_not_insert_semicolon_after_let_keyword) {
  {
    Test_Parser p(u8"let\nx = y;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"x"_sv)}));
  }
}

TEST_F(Test_Parse_Var,
       variables_used_in_let_initializer_are_used_before_variable_declaration) {
  using namespace std::literals::string_view_literals;

  Test_Parser p(u8"let x = x"_sv, capture_diags);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",  //
                            "visit_variable_declaration",
                        }));

  ASSERT_EQ(p.variable_declarations.size(), 1);
  EXPECT_EQ(p.variable_declarations[0].name, u8"x");
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(Test_Parse_Var, parse_valid_let) {
  {
    Test_Parser p(u8"let x\nclass C{}"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // x
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_end_of_module",
                          }));

    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"let x\nnew Array()"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // Array
                              "visit_end_of_module",
                          }));

    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"let x\ntypeof Array"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_typeof_use",   // Array
                              "visit_end_of_module",
                          }));

    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"let x\nclass C{}\nx = new C();"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",    // x
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // C
                              "visit_variable_use",            // C
                              "visit_variable_assignment",     // x
                              "visit_end_of_module",
                          }));

    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_Var, parse_invalid_let) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let a,"_sv,  //
        u8"     ^ Diag_Stray_Comma_In_Let_Statement"_diag);
    EXPECT_EQ(p.variable_declarations.size(), 1);
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let a,;"_sv,  //
        u8"     ^ Diag_Stray_Comma_In_Let_Statement"_diag);
    EXPECT_EQ(p.variable_declarations.size(), 1);
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let x, 42"_sv,  //
        u8"       ^^ Diag_Unexpected_Token_In_Variable_Declaration"_diag);
    EXPECT_EQ(p.variable_declarations.size(), 1);
  }

  // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
  for (String8 keyword : disallowed_binding_identifier_keywords) {
    {
      Test_Parser p(concat(u8"var "_sv, keyword), capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations, IsEmpty());
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Cannot_Declare_Variable_With_Keyword_Name,  //
                  keyword, u8"var "_sv.size(), keyword),
          }));
    }

    {
      Test_Parser p(concat(u8"var "_sv, keyword, u8";"_sv), capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations, IsEmpty());
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Cannot_Declare_Variable_With_Keyword_Name,  //
                  keyword, u8"var "_sv.size(), keyword),
          }));
    }

    {
      Test_Parser p(concat(u8"var "_sv, keyword, u8" = x;"_sv), capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations, IsEmpty());
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // x
                            }));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(
                  p.code, Diag_Cannot_Declare_Variable_With_Keyword_Name,  //
                  keyword, u8"var "_sv.size(), keyword),
          }));
    }
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let while (x) { break; }"_sv,  //
        u8"    ^^^^^ Diag_Unexpected_Token_In_Variable_Declaration"_diag);
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",       // x
                              "visit_enter_block_scope",  //
                              "visit_exit_block_scope",   //
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let 42*69"_sv,  //
        u8"    ^^ Diag_Unexpected_Token_In_Variable_Declaration"_diag);
    EXPECT_EQ(p.variable_declarations.size(), 0);
  }

  test_parse_and_visit_module(
      u8"let x, `hello`;"_sv,  //
      u8"       ^^^^^^^ Diag_Unexpected_Token_In_Variable_Declaration"_diag);

  {
    Test_Parser p(u8"let x, `hello${world}`;"_sv, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // world
                              "visit_end_of_module",
                          }));
    // TODO(strager): Improve the span.
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ^^^^^^^^ Diag_Unexpected_Token_In_Variable_Declaration"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let {debugger}"_sv,  //
        u8"     ^^^^^^^^ Diag_Missing_Value_For_Object_Literal_Entry"_diag);
    EXPECT_EQ(p.variable_declarations.size(), 0);
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let {42}"_sv,  //
        u8"     ^^ Diag_Invalid_Lone_Literal_In_Object_Literal"_diag);
    EXPECT_EQ(p.variable_declarations.size(), 0);
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let true, true, y\nlet x;"_sv,  //
        u8"    ^^^^ Diag_Unexpected_Token_In_Variable_Declaration"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"y"}));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
  }

  for (String8 prefix_operator : {u8"--", u8"++"}) {
    Test_Parser p(concat(u8"var "_sv, prefix_operator, u8"x;"_sv),
                  capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // x
                              "visit_variable_assignment",  // x
                              "visit_end_of_module",
                          }));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"   ` Diag_Missing_Semicolon_After_Statement"_diag,  //
            u8"^^^ Diag_Let_With_No_Bindings"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"const = y, z = w, = x;"_sv,  //
        u8"                  ^ Diag_Missing_Variable_Name_In_Declaration"_diag,  //
        u8"      ^ Diag_Missing_Variable_Name_In_Declaration"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_use",          // w
                              "visit_variable_declaration",  // z
                              "visit_variable_use",          // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x y = z w"_sv,  //
        u8"           ` Diag_Missing_Comma_Between_Variable_Declarations"_diag,  //
        u8"     ` Diag_Missing_Comma_Between_Variable_Declarations"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // z
                              "visit_variable_declaration",  // y
                              "visit_variable_declaration",  // z
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x [y]=ys {z}=zs"_sv,  //
        u8"            ` Diag_Missing_Comma_Between_Variable_Declarations"_diag,  //
        u8"     ` Diag_Missing_Comma_Between_Variable_Declarations"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // ys
                              "visit_variable_declaration",  // y
                              "visit_variable_use",          // zs
                              "visit_variable_declaration",  // z
                              "visit_end_of_module",
                          }));
  }

  for (String8 compound_assignment_operator : {
           u8"%=",
           u8"&=",
           u8"**=",
           u8"*=",
           u8"+=",
           u8"-=",
           u8"/=",
           u8"<<=",
           u8">>=",
           u8">>>=",
           u8"^=",
           u8"|=",
       }) {
    {
      Test_Parser p(
          concat(u8"let x "_sv, compound_assignment_operator, u8" y, z"_sv),
          capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",          // y
                                "visit_variable_declaration",  // x
                                "visit_variable_declaration",  // z
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {let_init_decl(u8"x"_sv), let_noinit_decl(u8"z"_sv)}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_2_OFFSETS(
                  p.code, Diag_Cannot_Update_Variable_During_Declaration,  //
                  updating_operator, u8"let x "_sv.size(),
                  compound_assignment_operator,  //
                  declaring_token, 0, u8"let"_sv),
          }));
    }

    {
      Test_Parser p(concat(u8"const [x, y] "_sv, compound_assignment_operator,
                           u8" init;"_sv),
                    capture_diags);
      SCOPED_TRACE(p.code);
      p.parse_and_visit_module();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",          // init
                                "visit_variable_declaration",  // x
                                "visit_variable_declaration",  // y
                                "visit_end_of_module",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray(
                      {const_init_decl(u8"x"_sv), const_init_decl(u8"y"_sv)}));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_2_OFFSETS(
                  p.code, Diag_Cannot_Update_Variable_During_Declaration,  //
                  updating_operator, u8"const [x, y] "_sv.size(),
                  compound_assignment_operator,  //
                  declaring_token, 0, u8"const"_sv),
          }));
    }
  }

  {
    Test_Parser p(u8"let [42] = x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_EQ(p.variable_declarations.size(), 0);
    // TODO(strager): Report a better message. We should say 'let statement',
    // not 'parameter'.
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ^^ Diag_Unexpected_Literal_In_Parameter_List"_diag,
        });
  }

  {
    Test_Parser p(u8"let [this] = x;"_sv, capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations, IsEmpty());
    // TODO(strager): Report a better message. We should say 'let statement',
    // not 'parameter'.
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ^^^^ Diag_This_Parameter_Not_Allowed_When_Destructuring"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let [y?] = x;"_sv,  //
        u8"      ^ Diag_Unexpected_Question_When_Destructuring"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"y"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"let {p: y?} = x;"_sv,  //
        u8"         ^ Diag_Unexpected_Question_When_Destructuring"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"y"_sv)}));
  }
}

TEST_F(Test_Parse_Var, parse_let_with_missing_equal) {
  {
    Test_Parser p(u8"async function f() {return 1;}\nlet x await f()"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // f
                              "visit_variable_declaration",       // x
                              "visit_end_of_module",
                          }));

    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                                     ` Diag_Missing_Equal_After_Variable"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x class C{}"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x function f() {}"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_enter_function_scope_body",   //
                              "visit_exit_function_scope",         //
                              "visit_variable_declaration",        // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x null"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x new Array()"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // Array
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x this"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x typeof Array"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",   // Array
                              "visit_variable_declaration",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"async function f() {return 1;}\nlet x await f(), y = x"_sv,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",       // f
                              "visit_enter_function_scope",       //
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",        //
                              "visit_variable_use",               // f
                              "visit_variable_declaration",       // x
                              "visit_variable_use",               // x
                              "visit_variable_declaration",       // y
                              "visit_end_of_module",
                          }));

    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                                     ` Diag_Missing_Equal_After_Variable"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x class C{}, y = x"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_class_scope",       // {
                              "visit_enter_class_scope_body",  // C
                              "visit_exit_class_scope",        // }
                              "visit_variable_declaration",    // x
                              "visit_variable_use",            // x
                              "visit_variable_declaration",    // y
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x function f() {}, y = x"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_named_function_scope",  // f
                              "visit_enter_function_scope_body",   //
                              "visit_exit_function_scope",         //
                              "visit_variable_declaration",        // x
                              "visit_variable_use",                // x
                              "visit_variable_declaration",        // y
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x null, y = x"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // x
                              "visit_variable_declaration",  // y
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x new Array(), y = x;"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // Array
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // x
                              "visit_variable_declaration",  // y
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x this, y = x"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // x
                              "visit_variable_declaration",  // y
                              "visit_end_of_module",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"let x typeof Array, y = x;"_sv,  //
        u8"     ` Diag_Missing_Equal_After_Variable"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_typeof_use",   // Array
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // x
                              "visit_variable_declaration",  // y
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Var, parse_invalid_var) {
  {
    Spy_Visitor p =
        test_parse_and_visit_statement(u8"var"_sv,  //
                                       u8"^^^ Diag_Let_With_No_Bindings"_diag);
    EXPECT_THAT(p.variable_declarations, IsEmpty());
  }
}

TEST_F(Test_Parse_Var, parse_invalid_const) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"const"_sv,  //
        u8"^^^^^ Diag_Let_With_No_Bindings"_diag);
    EXPECT_THAT(p.variable_declarations, IsEmpty());
  }
}

TEST_F(Test_Parse_Var, report_missing_semicolon_for_declarations) {
  {
    Test_Parser p(u8"let x = 2 for (;;) { console.log(); }"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"x"_sv)}));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"console"}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ` Diag_Missing_Semicolon_After_Statement"_diag,
        });
  }
  {
    Test_Parser p(u8"let x debugger"_sv, capture_diags);
    p.parse_and_visit_statement();
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_noinit_decl(u8"x"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ` Diag_Missing_Semicolon_After_Statement"_diag,
        });
  }
}

TEST_F(Test_Parse_Var, old_style_variables_can_be_named_let) {
  {
    Test_Parser p(u8"var let = initial;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // initial
                              "visit_variable_declaration",  // let
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_init_decl(u8"let"_sv)}));
  }

  {
    Test_Parser p(u8"function let(let) {}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",  // let (function)
                              "visit_enter_function_scope",
                              "visit_variable_declaration",  // let (parameter)
                              "visit_enter_function_scope_body",
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray(
                    {function_decl(u8"let"_sv), func_param_decl(u8"let"_sv)}));
  }

  {
    Test_Parser p(u8"(function let() {})"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits,
                ElementsAreArray({
                    "visit_enter_named_function_scope",  // let (function)
                    "visit_enter_function_scope_body",
                    "visit_exit_function_scope",
                }));
    EXPECT_THAT(p.enter_named_function_scopes, ElementsAreArray({u8"let"}));
  }

  {
    Test_Parser p(u8"try { } catch (let) { }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_block_scope",     //
                              "visit_exit_block_scope",      //
                              "visit_enter_block_scope",     //
                              "visit_variable_declaration",  // let
                              "visit_exit_block_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({catch_decl(u8"let"_sv)}));
  }

  {
    Test_Parser p(u8"let {x = let} = o;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // o
                              "visit_variable_use",          // let
                              "visit_variable_declaration",  // x
                          }));
    EXPECT_THAT(p.variable_uses, ::testing::Contains(u8"let"));
  }

  {
    Test_Parser p(u8"console.log(let);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // console
                              "visit_variable_use",  // let
                          }));
    EXPECT_THAT(p.variable_uses, ::testing::Contains(u8"let"));
  }

  {
    Test_Parser p(u8"let.method();"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // let
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let"}));
  }

  for (String8 code : {
           u8"(async let => null)",
           u8"(async (let) => null)",
           u8"(let => null)",
           u8"((let) => null)",
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",       //
                              "visit_variable_declaration",       // let
                              "visit_enter_function_scope_body",  //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({arrow_param_decl(u8"let"_sv)}));
  }

  {
    Test_Parser p(u8"for (let in xs) ;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_for_scope",      //
                              "visit_variable_use",         // xs
                              "visit_variable_assignment",  // let
                              "visit_exit_for_scope",
                          }));
    EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"let"}));
  }

  {
    Test_Parser p(u8"for (let.prop in xs) ;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs", u8"let"}));
  }

  {
    Test_Parser p(u8"let"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let"}));
  }

  {
    Test_Parser p(u8"let;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let"}));
  }

  {
    Test_Parser p(u8"let in other;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let", u8"other"}));
  }

  {
    Test_Parser p(u8"let instanceof MyClass;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"let", u8"MyClass"}));
  }
}

TEST_F(Test_Parse_Var, new_style_variables_cannot_be_named_let) {
  for (String8 declaration_kind : {u8"const", u8"let"}) {
    Test_Parser p(concat(declaration_kind, u8" let = null;"_sv), capture_diags);
    p.parse_and_visit_statement();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, Diag_Cannot_Declare_Variable_Named_Let_With_Let,  //
                name, declaration_kind.size() + 1, u8"let"_sv),
        }));

    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_declaration",
                          }));
    ASSERT_EQ(p.variable_declarations.size(), 1);
    EXPECT_EQ(p.variable_declarations[0].name, u8"let");
  }

  test_parse_and_visit_statement(
      u8"let {other, let} = stuff;"_sv,  //
      u8"            ^^^ Diag_Cannot_Declare_Variable_Named_Let_With_Let"_diag);

  // import implies strict mode (because modules imply strict mode).
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import let from 'weird';"_sv,  //
        u8"       ^^^ Diag_Cannot_Import_Let"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"let"_sv)}));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import * as let from 'weird';"_sv,  //
        u8"            ^^^ Diag_Cannot_Import_Let"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"let"_sv)}));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import { let } from 'weird';"_sv,  //
        u8"         ^^^ Diag_Cannot_Import_Let"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"let"_sv)}));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import { someName as let } from 'weird';"_sv,  //
        u8"                     ^^^ Diag_Cannot_Import_Let"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"let"_sv)}));
  }

  // import implies strict mode (because modules imply strict mode).
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"import { 'someName' as let } from 'weird';"_sv,  //
        u8"                       ^^^ Diag_Cannot_Import_Let"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({import_decl(u8"let"_sv)}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"export function let() {}"_sv,  //
        u8"                ^^^ Diag_Cannot_Export_Let"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"let"_sv)}));
  }

  // class implies strict mode.
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"class let {}"_sv,  //
        u8"      ^^^ Diag_Cannot_Declare_Class_Named_Let"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({class_decl(u8"let"_sv)}));
  }
}

TEST_F(Test_Parse_Var, use_await_in_non_async_function) {
  {
    Test_Parser p(u8"await(x);"_sv);
    auto guard = p.enter_function(Function_Attributes::normal);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await",  //
                                                   u8"x"}));
  }

  {
    Test_Parser p(
        u8"async function f() {\n"
        u8"  function g() { await(x); }\n"
        u8"}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await",  //
                                                   u8"x"}));
  }

  {
    Test_Parser p(
        u8"function f() {\n"
        u8"  async function g() {}\n"
        u8"  await();\n"
        u8"}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await"}));
  }

  {
    Test_Parser p(
        u8"(() => {\n"
        u8"  async () => {};\n"
        u8"  await();\n"
        u8"})"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await"}));
  }

  {
    Test_Parser p(u8"(async => { await(); })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await"}));
  }

  {
    Test_Parser p(u8"({ async() { await(); } })"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await"}));
  }

  {
    Test_Parser p(u8"class C { async() { await(); } }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await"}));
  }
}

TEST_F(Test_Parse_Var, declare_await_in_non_async_function) {
  {
    Test_Parser p(u8"function await() { }"_sv);
    auto guard = p.enter_function(Function_Attributes::normal);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"await"_sv)}));
  }

  {
    Test_Parser p(u8"let await = 42;"_sv);
    auto guard = p.enter_function(Function_Attributes::normal);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"await"_sv)}));
  }

  {
    Test_Parser p(
        u8"(async function() {\n"
        u8"  (function(await) { })\n"
        u8"})"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"await"_sv)}));
  }

  {
    Test_Parser p(
        u8"(function() {\n"
        u8"  async function await() { }\n"
        u8"})"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"await"_sv)}));
  }
}

TEST_F(Test_Parse_Var, declare_await_in_async_function) {
  {
    Test_Parser p(u8"function await() { }"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::async);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"await"_sv)}));
    // TODO(strager): Include a note referencing the origin of the async
    // function.
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag,
        });
  }

  {
    Test_Parser p(u8"var await;"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::async);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"await"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"    ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag,
        });
  }

  {
    Test_Parser p(u8"try {} catch (await) {}"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::async);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({catch_decl(u8"await"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"              ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag,
        });
  }

  {
    // TODO(strager): Drop the
    // Diag_Missing_Operand_For_Operator error.
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"async function f(await) {}"_sv,  //
        u8"                 ^^^^^ Diag_Cannot_Declare_Await_In_Async_Function"_diag,  //
        u8"Diag_Missing_Operand_For_Operator"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv),  //
                                  func_param_decl(u8"await"_sv)}));
  }
}

TEST_F(Test_Parse_Var, declare_await_at_top_level) {
  {
    Test_Parser p(u8"function await() { }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"await"_sv)}));
  }

  {
    Test_Parser p(u8"let await = 42;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"await"_sv)}));
  }
}

TEST_F(Test_Parse_Var, use_await_at_top_level_as_operator) {
  {
    Test_Parser p(u8"await x;"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await(x);"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await +x;"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await -x;"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await[x]"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await`template`"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await`template${x}`"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Var, use_await_at_top_level_as_variable) {
  {
    Test_Parser p(u8"await;"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // await
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // await
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"(await)"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // await
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await = x"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // x
                              "visit_variable_assignment",  // await
                              "visit_end_of_module",
                          }));
  }

  {
    Test_Parser p(u8"await.prop"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await"}));
  }

  {
    Test_Parser p(u8"await?.prop"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"await"}));
  }

  {
    Test_Parser p(u8"await ? x : y"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // await
                              "visit_variable_use",  // x
                              "visit_variable_use",  // y
                              "visit_end_of_module",
                          }));
  }

  for (String8 op : {
           u8"!=",  u8"!==", u8"%",          u8"&",  u8"&&",  u8"*",
           u8"**",  u8",",   u8"<",          u8"<<", u8"<=",  u8"==",
           u8"===", u8">",   u8">=",         u8">>", u8">>>", u8"??",
           u8"^",   u8"in",  u8"instanceof", u8"|",  u8"||",
       }) {
    Padded_String code(concat(u8"await "_sv, op, u8" x;"_sv));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view());
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // await
                              "visit_variable_use",  // x
                              "visit_end_of_module",
                          }));
  }

  for (String8 op : {
           u8"%=",
           u8"&&=",
           u8"&=",
           u8"**=",
           u8"*=",
           u8"+=",
           u8"-=",
           u8"/=",
           u8"<<=",
           u8">>=",
           u8">>>=",
           u8"?\x3f=",
           u8"^=",
           u8"|=",
           u8"||=",
       }) {
    Padded_String code(concat(u8"await "_sv, op, u8" x;"_sv));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view());
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",         // await
                              "visit_variable_use",         // x
                              "visit_variable_assignment",  // await
                              "visit_end_of_module",
                          }));
  }

  // TODO(#464): Interpret / as divide, not a regular expression.
  if ((false)) {
    Test_Parser p(u8"await / await / await / await"_sv);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // await
                              "visit_variable_use",  // await
                              "visit_variable_use",  // await
                              "visit_variable_use",  // await
                              "visit_end_of_module",
                          }));
  }
}

TEST_F(Test_Parse_Var, forced_top_level_await_operator) {
  {
    Test_Parser p(
        u8"await p;"_sv,
        Parser_Options{
            .top_level_await_mode = Parser_Top_Level_Await_Mode::await_operator,
        },
        capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",  // p
                              "visit_end_of_module",
                          }));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(
        u8"await;"_sv,
        Parser_Options{
            .top_level_await_mode = Parser_Top_Level_Await_Mode::await_operator,
        },
        capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_end_of_module",
                          }));
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^^^^^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }
}

TEST_F(
    Test_Parse_Var,
    declare_await_in_async_function_is_allowed_for_named_function_expressions) {
  {
    Test_Parser p(
        u8"(async function() {\n"
        u8"  (function await() { await; })(); \n"
        u8"})();"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_enter_function_scope",        //
                              "visit_enter_function_scope_body",   //
                              "visit_enter_named_function_scope",  // await
                              "visit_enter_function_scope_body",   //
                              "visit_variable_use",                // await
                              "visit_exit_function_scope",         //
                              "visit_exit_function_scope",
                          }));
    EXPECT_THAT(p.enter_named_function_scopes, ElementsAreArray({u8"await"}));
  }
}

TEST_F(Test_Parse_Var, use_yield_in_non_generator_function) {
  {
    Test_Parser p(u8"yield(x);"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"yield", u8"x"}));
  }

  {
    Test_Parser p(
        u8"function* f() {\n"
        u8"  function g() { yield(x); }\n"
        u8"}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"yield", u8"x"}));
  }

  {
    Test_Parser p(
        u8"function f() {\n"
        u8"  function* g() {}\n"
        u8"  yield();\n"
        u8"}"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"yield"}));
  }
}

TEST_F(Test_Parse_Var, declare_yield_in_non_generator_function) {
  {
    Test_Parser p(u8"function yield() { }"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"yield"_sv)}));
  }

  {
    Test_Parser p(u8"let yield = 42;"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({let_init_decl(u8"yield"_sv)}));
  }

  {
    Test_Parser p(
        u8"(async function() {\n"
        u8"  (function(yield) { })\n"
        u8"})"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({func_param_decl(u8"yield"_sv)}));
  }

  {
    Test_Parser p(
        u8"(function() {\n"
        u8"  function* yield() { }\n"
        u8"})"_sv);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"yield"_sv)}));
  }
}

TEST_F(Test_Parse_Var, declare_yield_in_generator_function) {
  {
    Test_Parser p(u8"function yield() { }"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::generator);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"yield"_sv)}));
    // TODO(strager): Include a note referencing the origin of the generator
    // function.
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ^^^^^ Diag_Cannot_Declare_Yield_In_Generator_Function"_diag,
        });
  }

  {
    Test_Parser p(u8"var yield;"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::generator);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({var_noinit_decl(u8"yield"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"    ^^^^^ Diag_Cannot_Declare_Yield_In_Generator_Function"_diag,
        });
  }

  {
    Test_Parser p(u8"try {} catch (yield) {}"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::generator);
    p.parse_and_visit_statement();
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({catch_decl(u8"yield"_sv)}));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"              ^^^^^ Diag_Cannot_Declare_Yield_In_Generator_Function"_diag,
        });
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"function* f(yield) {}"_sv,  //
        u8"            ^^^^^ Diag_Cannot_Declare_Yield_In_Generator_Function"_diag);
    EXPECT_THAT(p.variable_declarations,
                ElementsAreArray({function_decl(u8"f"_sv),  //
                                  func_param_decl(u8"yield"_sv)}));
  }
}

TEST_F(Test_Parse_Var, variables_can_be_named_contextual_keywords) {
  Dirty_Set<String8> variable_names =
      (contextual_keywords - Dirty_Set<String8>{u8"let"}) |
      Dirty_Set<String8>{u8"await", u8"yield"} |
      // TODO(#73): Disallow these ('protected', 'implements', etc.) in strict
      // mode.
      strict_only_reserved_keywords;

  for (String8 name : variable_names) {
    SCOPED_TRACE(out_string8(name));

    {
      Test_Parser p(concat(u8"var "_sv, name, u8" = initial;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",          // initial
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({var_init_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"let "_sv, name, u8" = initial;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",          // initial
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({let_init_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"let {"_sv, name, u8" = 10 } = initial;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",          // initial
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({let_init_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"const "_sv, name, u8" = initial;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",          // initial
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({const_init_decl(name)}));
    }

    {
      Test_Parser p(
          concat(u8"function "_sv, name, u8"("_sv, name, u8") {}"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAreArray({
                      "visit_variable_declaration",       // (name) (function)
                      "visit_enter_function_scope",       //
                      "visit_variable_declaration",       // (name) (parameter)
                      "visit_enter_function_scope_body",  //
                      "visit_exit_function_scope",
                  }));
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAreArray({function_decl(name), func_param_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"function f("_sv, name, u8": ParamType) {}"_sv),
                    typescript_options);
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAreArray({
                      "visit_variable_declaration",       // f
                      "visit_enter_function_scope",       //
                      "visit_variable_type_use",          // ParamType
                      "visit_variable_declaration",       // (name)
                      "visit_enter_function_scope_body",  // {
                      "visit_exit_function_scope",        // }
                  }));
      EXPECT_THAT(
          p.variable_declarations,
          ElementsAreArray({function_decl(u8"f"_sv), func_param_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"(function "_sv, name, u8"() {})"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits,
                  ElementsAreArray({
                      "visit_enter_named_function_scope",  // (name) (function)
                      "visit_enter_function_scope_body",   //
                      "visit_exit_function_scope",
                  }));
      EXPECT_THAT(p.enter_named_function_scopes, ElementsAreArray({name}));
    }

    {
      Test_Parser p(concat(u8"class "_sv, name, u8" {}"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_class_scope",       //
                                "visit_enter_class_scope_body",  //
                                "visit_exit_class_scope",
                                "visit_variable_declaration",  // (name)
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({class_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"(class "_sv, name, u8" {})"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_class_scope",       // {
                                "visit_enter_class_scope_body",  // (name)
                                "visit_exit_class_scope",        // }
                            }));
    }

    {
      Test_Parser p(concat(u8"try { } catch ("_sv, name, u8") { }"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_block_scope",     //
                                "visit_exit_block_scope",      //
                                "visit_enter_block_scope",     //
                                "visit_variable_declaration",  // (name)
                                "visit_exit_block_scope",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({catch_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"let {x = "_sv, name, u8"} = o;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",          // o
                                "visit_variable_use",          // (name)
                                "visit_variable_declaration",  // x
                            }));
      EXPECT_THAT(p.variable_uses, ::testing::Contains(name));
    }

    {
      Test_Parser p(concat(u8"console.log("_sv, name, u8");"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // console
                                "visit_variable_use",  // (name)
                            }));
      EXPECT_THAT(p.variable_uses, ::testing::Contains(name));
    }

    {
      Test_Parser p(name);
      SCOPED_TRACE(p.code);
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (name)
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      Test_Parser p(concat(name, u8";"_sv));
      SCOPED_TRACE(p.code);
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (name)
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      Test_Parser p(concat(u8"{ "_sv, name, u8" }"_sv));
      SCOPED_TRACE(p.code);
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_block_scope",  // {
                                "visit_variable_use",       // (name)
                                "visit_exit_block_scope",   // }
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      Test_Parser p(concat(u8"class A extends "_sv, name, u8" { }"_sv));
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_class_scope",       // { A
                                "visit_variable_use",            // (name)
                                "visit_enter_class_scope_body",  // { {
                                "visit_exit_class_scope",        // }
                                "visit_variable_declaration",    // A
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      // NOTE[extends-await-paren]: 'await() {}' used to trigger E0176 (missing
      // arrow operator for arrow function).
      Test_Parser p(concat(u8"class A extends "_sv, name, u8"() { }"_sv));
      SCOPED_TRACE(p.code);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_class_scope",       // { A
                                "visit_variable_use",            // (name)
                                "visit_enter_class_scope_body",  // { {
                                "visit_exit_class_scope",        // }
                                "visit_variable_declaration",    // A
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      Test_Parser p(concat(name, u8".method();"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (name)
                            }));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({name}));
    }

    {
      Test_Parser p(concat(name, u8"[x];"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",  // (name)
                                "visit_variable_use",  // x
                            }));
      EXPECT_THAT(p.variable_uses,
                  ElementsAreArray({String8_View(name), u8"x"_sv}));
    }

    for (String8 code : {
             u8"(async " + name + u8" => null)",
             u8"(async (" + name + u8") => null)",
             u8"(" + name + u8" => null)",
             u8"((" + name + u8") => null)",
         }) {
      if (name == u8"await"_sv &&
          quick_lint_js::starts_with(String8_View(code), u8"(async"_sv)) {
        // NOTE(erlliam): await parameter isn't allowed in async functions. See
        // test_parse.disallow_await_parameter_in_async_arrow_function.
        continue;
      }
      SCOPED_TRACE(out_string8(code));
      Test_Parser p(code);
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_variable_declaration",       // (name)
                                "visit_enter_function_scope_body",  //
                                "visit_exit_function_scope",
                            }));
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({arrow_param_decl(name)}));
    }

    {
      Test_Parser p(concat(u8"for ("_sv, name, u8" in xs) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_variable_use",         // xs
                                "visit_variable_assignment",  // (name)
                            }));
      EXPECT_THAT(p.variable_assignments, ElementsAreArray({name}));
    }

    {
      Test_Parser p(concat(u8"for ("_sv, name, u8".prop in xs) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"xs"));
    }

    if (name != u8"async") {
      // NOTE(strager): async isn't allowed here. See
      // test_parse.cannot_assign_to_variable_named_async_in_for_of.
      Test_Parser p(concat(u8"for ("_sv, name, u8" of xs) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, ElementsAreArray({name}));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs"}));
    }

    {
      Test_Parser p(concat(u8"for (("_sv, name, u8") of xs) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, ElementsAreArray({name}));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs"}));
    }

    {
      Test_Parser p(concat(u8"for ("_sv, name, u8".prop of xs) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, IsEmpty());
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"xs"));
    }

    {
      Test_Parser p(concat(u8"for (let "_sv, name, u8" of xs) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({let_noinit_decl(name)}));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs"}));
    }

    {
      Test_Parser p(concat(u8"for (var "_sv, name, u8" of xs) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_declarations,
                  ElementsAreArray({var_noinit_decl(name)}));
      EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"xs"}));
    }

    {
      Test_Parser p(concat(u8"for ("_sv, name, u8"; cond;) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, IsEmpty());
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"cond"));
    }

    {
      Test_Parser p(concat(u8"for ("_sv, name, u8".prop; cond;) ;"_sv));
      auto guard = p.enter_function(Function_Attributes::normal);
      p.parse_and_visit_statement();
      EXPECT_THAT(p.variable_assignments, IsEmpty());
      EXPECT_THAT(p.variable_uses, ElementsAre(name, u8"cond"));
    }
  }
}

TEST_F(Test_Parse_Var,
       lexical_declaration_as_do_while_loop_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"do const x = y; while (cond);"_sv,  //
        u8"  ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"   ^^^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::do_while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // cond
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"do let x = y; while (cond);"_sv,  //
        u8"  ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"   ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::do_while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_variable_use",          // cond
                          }));
  }
}

TEST_F(Test_Parse_Var, lexical_declaration_as_for_loop_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"for (;cond;) const x = y;"_sv,  //
        u8"            ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"             ^^^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::for_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"for (;cond;) let x = y;"_sv,  //
        u8"            ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"             ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::for_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
  }
}

TEST_F(Test_Parse_Var, lexical_declaration_as_if_statement_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) const x = y;"_sv,  //
        u8"         ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"          ^^^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) let x = y;"_sv,  //
        u8"         ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"          ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) const x = y; else {}"_sv,  //
        u8"         ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"          ^^^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     // else
                              "visit_exit_block_scope",      // else
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) let x = y; else {}"_sv,  //
        u8"         ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"          ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_enter_block_scope",     // else
                              "visit_exit_block_scope",      // else
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) {} else const x = y;"_sv,  //
        u8"                 ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"                  ^^^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_enter_block_scope",     // if
                              "visit_exit_block_scope",      // if
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) {} else let x = y;"_sv,  //
        u8"                 ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"                  ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_enter_block_scope",     // if
                              "visit_exit_block_scope",      // if
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
  }
}

TEST_F(Test_Parse_Var, lexical_declaration_as_while_loop_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"while (cond) const x = y;"_sv,  //
        u8"            ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"             ^^^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"while (cond) let x = y;"_sv,  //
        u8"            ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"             ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::while_loop}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                          }));
  }
}

TEST_F(Test_Parse_Var,
       lexical_declaration_as_with_statement_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with (obj) const x = y;"_sv,  //
        u8"          ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"           ^^^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::with_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // obj
                              "visit_enter_with_scope",      // with
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_exit_with_scope",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"with (obj) let x = y;"_sv,  //
        u8"          ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"           ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::with_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // obj
                              "visit_enter_with_scope",      // with
                              "visit_variable_use",          // y
                              "visit_variable_declaration",  // x
                              "visit_exit_with_scope",
                          }));
  }
}

TEST_F(Test_Parse_Var, lexical_declaration_as_label_body_is_disallowed) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"l: const x = y;"_sv,  //
        u8"  ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"   ^^^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::labelled_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_declaration",
                          }));
  }

  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"l: let x = y;"_sv,  //
        u8"  ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"   ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::labelled_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",
                              "visit_variable_declaration",
                          }));
  }
}

TEST_F(Test_Parse_Var, var_declaration_as_label_body_is_allowed) {
  Test_Parser p(u8"l: var x = y;"_sv);
  p.parse_and_visit_statement();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",
                            "visit_variable_declaration",
                        }));
}

TEST_F(Test_Parse_Var, spread_must_precede_variable_name) {
  test_parse_and_visit_statement(
      u8"const [a, b, ...] = z;"_sv,  //
      u8"Diag_Spread_Must_Precede_Variable_Name"_diag);
}

TEST_F(Test_Parse_Var,
       let_as_statement_body_does_not_allow_asi_before_left_square) {
  {
    Spy_Visitor p = test_parse_and_visit_statement(
        u8"if (cond) let\n[x] = xs;"_sv,  //
        u8"         ` Diag_Lexical_Declaration_Not_Allowed_In_Body.expected_body\n"_diag
        u8"          ^^^ .declaring_keyword"_diag
        u8"{.kind_of_statement=Statement_Kind::if_statement}"_diag);
    EXPECT_THAT(p.visits, ElementsAreArray({
                              "visit_variable_use",          // cond
                              "visit_variable_use",          // xs
                              "visit_variable_declaration",  // x
                          }));
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
