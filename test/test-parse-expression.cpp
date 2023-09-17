// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAreArray;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
TEST_F(Test_Parse_Expression, parse_single_token_expression) {
  {
    Test_Parser p(u8"x"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Variable);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"x"_sv);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 1));
  }

  {
    Test_Parser p(u8"42"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 2));
  }

  {
    Test_Parser p(u8"'hello'"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
  }

  {
    Test_Parser p(u8"null"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 4));
  }

  {
    Test_Parser p(u8"true"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 4));
  }

  {
    Test_Parser p(u8"false"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
  }

  {
    Test_Parser p(u8"this"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::This_Variable);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 4));
  }
}

TEST_F(Test_Parse_Expression, keyword_variable_reference) {
  {
    Test_Parser p(u8"async"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Variable);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"async"_sv);
  }

  {
    Test_Parser p(u8"async()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Call);
    EXPECT_EQ(ast->child_0()->kind(), Expression_Kind::Variable);
    EXPECT_EQ(ast->child_0()->variable_identifier().normalized_name(),
              u8"async"_sv);
  }

  {
    Test_Parser p(u8"async(a, b).c"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(call(var async, var a, var b), c)");
  }
}

TEST_F(Test_Parse_Expression, private_identifiers_are_not_valid_expressions) {
  {
    Test_Parser p(u8"#myPrivateField"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Private_Variable);
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^^^^^^^^^^^^^^ Diag_Cannot_Refer_To_Private_Variable_Without_Object"_diag,
        });
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"#myPrivateField"_sv));
  }

  {
    Test_Parser p(u8"#myPrivateField = 10"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Assignment);
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^^^^^^^^^^^^^^ Diag_Cannot_Refer_To_Private_Variable_Without_Object"_diag,
        });
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"#myPrivateField = 10"_sv));
  }
}

TEST_F(Test_Parse_Expression, private_identifier_with_in_operator) {
  Test_Parser p(u8"#private in obj"_sv);
  Expression* ast = p.parse_expression();
  EXPECT_EQ(summarize(ast), "binary(privatevar #private, var obj)");
}

TEST_F(Test_Parse_Expression, parse_regular_expression) {
  {
    Test_Parser p(u8"/regexp/"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 8));
  }

  {
    Test_Parser p(u8"/=regexp/"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 9));
  }
}

TEST_F(Test_Parse_Expression, parse_math_expression) {
  {
    Test_Parser p(u8"-x"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Unary_Operator);
    EXPECT_EQ(ast->child_0()->kind(), Expression_Kind::Variable);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 2));
  }

  {
    Test_Parser p(u8"+x"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }

  {
    Test_Parser p(u8"~x"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }

  {
    Test_Parser p(u8"x+y"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y)");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
  }

  {
    Test_Parser p(u8"x+y-z"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y, var z)");
  }

  {
    Test_Parser p(u8"2-4+1"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal, literal)");
  }

  {
    Test_Parser p(u8"-x+y"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(unary(var x), var y)");
  }

  for (String8_View input : {
           u8"2+2"_sv,
           u8"2-2"_sv,
           u8"2*2"_sv,
           u8"2/2"_sv,
           u8"2%2"_sv,
           u8"2**2"_sv,
           u8"2^2"_sv,
           u8"2&2"_sv,
           u8"2|2"_sv,
           u8"2<<2"_sv,
           u8"2>>2"_sv,
           u8"2>>>2"_sv,
       }) {
    SCOPED_TRACE(out_string8(u8"input = " + String8(input)));
    Test_Parser p(input);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
  }
}

TEST_F(Test_Parse_Expression, parse_broken_math_expression) {
  {
    Test_Parser p(u8"2+"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, missing)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8" ^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"^2"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(missing, literal)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  // NOTE(strager): "/=" is not tested here because "/=/" is a regular
  // expression literal.
  for (String8_View op :
       {u8"*="_sv, u8"%="_sv, u8"+="_sv, u8"-="_sv, u8"<<="_sv, u8">>="_sv,
        u8">>>="_sv, u8"&="_sv, u8"^="_sv, u8"|="_sv, u8"**="_sv}) {
    Test_Parser p(concat(op, u8" 2"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "upassign(missing, literal)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Operand_For_Operator,  //
                              where, 0, op),
        }));
  }

  {
    Test_Parser p(u8"2 * * 2"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, missing, literal)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"  ^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"2 & & & 2"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, missing, missing, literal)");

    assert_diagnostics(p.code, p.errors,
                       {
                           u8"    ^ Diag_Missing_Operand_For_Operator"_diag,  //
                           u8"  ^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"(2*)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(binary(literal, missing))");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"  ^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"2 * (3 + 4"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(literal, paren(binary(literal, literal)))");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"    ^ Diag_Unmatched_Parenthesis"_diag,
                       });
  }

  {
    Test_Parser p(u8"2 * (3 + (4"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(literal, paren(binary(literal, paren(literal))))");

    assert_diagnostics(p.code, p.errors,
                       {
                           u8"         ^ Diag_Unmatched_Parenthesis"_diag,  //
                           u8"    ^ Diag_Unmatched_Parenthesis"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression, comma_expression_with_trailing_comma) {
  {
    // Arrow expressions allow trailing commas in their parenthesized parameter
    // lists, but comma expressions do not allow trailing commas.
    // A trailing comma expression emits no errors; errors are emitted depending
    // on the context.
    Test_Parser p(u8"(a, b, c,)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(trailingcomma(var a, var b, var c))");
  }
}

TEST_F(Test_Parse_Expression, parse_logical_expression) {
  for (String8_View input :
       {u8"2==2"_sv, u8"2===2"_sv, u8"2!=2"_sv, u8"2!==2"_sv, u8"2>2"_sv,
        u8"2<2"_sv, u8"2>=2"_sv, u8"2<=2"_sv, u8"2&&2"_sv, u8"2??2"_sv,
        u8"2||2"_sv}) {
    SCOPED_TRACE(out_string8(u8"input = " + String8(input)));
    Test_Parser p(input);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
  }

  {
    Test_Parser p(u8"!x"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }
}

TEST_F(Test_Parse_Expression, parse_keyword_binary_operators) {
  {
    Test_Parser p(u8"prop in object"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var prop, var object)");
  }

  {
    Test_Parser p(u8"object instanceof Class"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var object, var Class)");
  }
}

TEST_F(Test_Parse_Expression, parse_typeof_unary_operator) {
  {
    Test_Parser p(u8"typeof o"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "typeof(var o)");
  }

  {
    Test_Parser p(u8"typeof o === 'number'"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(typeof(var o), literal)");
  }

  {
    Test_Parser p(u8"typeof o.p"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "typeof(dot(var o, p))");
  }
}

TEST_F(Test_Parse_Expression, parse_typeof_conditional_operator) {
  {
    Test_Parser p(u8"typeof o ? 10 : 20"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(typeof(var o), literal, literal)");
  }
}

TEST_F(Test_Parse_Expression, parse_await_conditional_operator) {
  {
    Test_Parser p(u8"await a ? b : c"_sv);
    auto guard = p.enter_function(Function_Attributes::async);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(await(var a), var b, var c)");
  }
}

TEST_F(Test_Parse_Expression, delete_unary_operator) {
  {
    Test_Parser p(u8"delete variable"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "delete(var variable)");
  }

  {
    Test_Parser p(u8"delete variable.property"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "delete(dot(var variable, property))");
  }
}

TEST_F(Test_Parse_Expression, void_unary_operator) {
  {
    Test_Parser p(u8"void 0"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(literal)");
  }
}

TEST_F(Test_Parse_Expression, spread) {
  {
    Test_Parser p(u8"...args"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "spread(var args)");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
  }
}

TEST_F(Test_Parse_Expression, parse_function_call) {
  {
    Test_Parser p(u8"f()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
    Expression::Call* call = expression_cast<Expression::Call>(ast);
    EXPECT_THAT(call->left_paren_span(), p.matches_offsets(1, 2));
  }

  {
    Test_Parser p(u8"f(x)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(1)), "var x");
  }

  {
    Test_Parser p(u8"f(x,y)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 3);
    EXPECT_EQ(summarize(ast->child(1)), "var x");
    EXPECT_EQ(summarize(ast->child(2)), "var y");
  }
}

TEST_F(Test_Parse_Expression, function_call_with_invalid_extra_commas) {
  {
    Test_Parser p(u8"f(,)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^ Diag_Extra_Comma_Not_Allowed_Between_Arguments"_diag,
        });
  }

  {
    Test_Parser p(u8"f(a,,b)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f, var a, var b)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"    ^ Diag_Extra_Comma_Not_Allowed_Between_Arguments"_diag,
        });
  }

  {
    // A function named 'async' in a special case because of lookahead:
    // 'async()' is a function call, but 'async()=>{}' is an arrow function.
    Test_Parser p(u8"async(a,,b)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var async, var a, var b)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"        ^ Diag_Extra_Comma_Not_Allowed_Between_Arguments"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, parse_optional_function_call) {
  {
    Test_Parser p(u8"f?.(x,y)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f, var x, var y)");
  }
}

TEST_F(Test_Parse_Expression, parse_dot_expressions) {
  {
    Test_Parser p(u8"x.prop"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Dot);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"prop"_sv);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 6));
  }

  {
    Test_Parser p(u8"x.p1.p2"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(dot(var x, p1), p2)");
  }

  for (String8_View keyword : keywords) {
    SCOPED_TRACE(out_string8(keyword));
    Test_Parser p(concat(u8"promise."_sv, keyword));
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var promise, " + to_string(keyword) + ")");
  }

  {
    Test_Parser p(u8"x.#private"_sv);
    auto class_guard = p.enter_class();  // Allow to call private identifiers.
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, #private)");
  }
}

TEST_F(Test_Parse_Expression, invalid_dot_expression) {
  {
    Test_Parser p(u8"x.''"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, literal)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8" ^ Diag_Invalid_Rhs_For_Dot_Operator"_diag,
                       });
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x.''"_sv));
  }

  {
    Test_Parser p(u8"x. "_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, )");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^ Diag_Missing_Property_Name_For_Dot_Operator"_diag,
        });
  }

  {
    Test_Parser p(u8"(x.)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(dot(var x, ))");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^ Diag_Missing_Property_Name_For_Dot_Operator"_diag,
        });
  }

  for (String8_View op : {
           u8"!="_sv,   u8"!=="_sv, u8"%"_sv,      u8"%="_sv,  u8"&"_sv,
           u8"&&"_sv,   u8"&&="_sv, u8"&="_sv,     u8"*"_sv,   u8"**"_sv,
           u8"**="_sv,  u8"*="_sv,  u8"+"_sv,      u8"+="_sv,  u8","_sv,
           u8"-"_sv,    u8"-="_sv,  u8"/="_sv,     u8"<"_sv,   u8"<<"_sv,
           u8"<<="_sv,  u8"<="_sv,  u8"="_sv,      u8"=="_sv,  u8"==="_sv,
           u8">"_sv,    u8">="_sv,  u8">>"_sv,     u8">>="_sv, u8">>>"_sv,
           u8">>>="_sv, u8"??"_sv,  u8"?\x3f="_sv, u8"^"_sv,   u8"^="_sv,
           u8"|"_sv,    u8"|="_sv,  u8"||"_sv,     u8"||="_sv,
       }) {
    Test_Parser p(concat(u8"x. "_sv, op, u8" y"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast),
                ::testing::AnyOf("assign(dot(var x, ), var y)",      //
                                 "binary(dot(var x, ), var y)",      //
                                 "condassign(dot(var x, ), var y)",  //
                                 "upassign(dot(var x, ), var y)"));
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^ Diag_Missing_Property_Name_For_Dot_Operator"_diag,
        });
  }

  {
    Test_Parser p(u8"x. ? y. : z"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(dot(var x, ), dot(var y, ), var z)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"      ^ Diag_Missing_Property_Name_For_Dot_Operator"_diag,  //
            u8" ^ Diag_Missing_Property_Name_For_Dot_Operator"_diag,
        });
  }

  {
    Test_Parser p(u8"x.;"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, )");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^ Diag_Missing_Property_Name_For_Dot_Operator"_diag,
        });
  }

  {
    Test_Parser p(u8".;"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(missing, )");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^ Diag_Missing_Operand_For_Operator"_diag,  //
            u8"^ Diag_Missing_Property_Name_For_Dot_Operator"_diag,
        });
  }

  {
    Test_Parser p(u8"console.('hello');"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(dot(var console, ), literal)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ^ Diag_Missing_Property_Name_For_Dot_Operator"_diag,
        });
  }

  {
    Test_Parser p(u8"'hello' .. 'world'"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"        ^^ Diag_Dot_Dot_Is_Not_An_Operator"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression, parse_optional_dot_expressions) {
  {
    Test_Parser p(u8"x?.prop"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, prop)");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x?.prop"_sv));
  }

  for (String8 keyword : keywords) {
    Padded_String code(u8"obj?." + keyword);
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view());
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Dot);
    EXPECT_EQ(summarize(ast->child_0()), "var obj");
    EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
  }

  {
    Test_Parser p(u8"x?.#private"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, #private)");
  }
}

TEST_F(Test_Parse_Expression, parse_indexing_expression) {
  {
    Test_Parser p(u8"xs[i]"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Index);
    EXPECT_EQ(summarize(ast->child_0()), "var xs");
    EXPECT_EQ(summarize(ast->child_1()), "var i");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
  }
}

TEST_F(Test_Parse_Expression, parse_optional_indexing_expression) {
  {
    Test_Parser p(u8"xs?.[i]"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, var i)");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"xs?.[i]"_sv));
  }
}

TEST_F(Test_Parse_Expression, parse_unclosed_indexing_expression) {
  {
    Test_Parser p(u8"xs[i"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, var i)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"  ^ Diag_Unmatched_Indexing_Bracket"_diag,
                       });
  }

  {
    Test_Parser p(u8"(xs[i)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(index(var xs, var i))");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"   ^ Diag_Unmatched_Indexing_Bracket"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression, empty_indexing_expression) {
  {
    Test_Parser p(u8"xs[]"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, missing)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"  ^^ Diag_Indexing_Requires_Expression"_diag,
                       });
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"xs[]"_sv));
  }
}

TEST_F(Test_Parse_Expression, parse_parenthesized_expression) {
  {
    Test_Parser p(u8"(x)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(var x)");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"(x)"_sv));
    EXPECT_THAT(ast->child_0()->span(), p.matches_offsets(1, 2));
  }

  {
    Test_Parser p(u8"x+(y)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, paren(var y))");
  }

  {
    Test_Parser p(u8"x+(y+z)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, paren(binary(var y, var z)))");
  }

  {
    Test_Parser p(u8"(x+y)+z"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(paren(binary(var x, var y)), var z)");
  }

  {
    Test_Parser p(u8"x+(y+z)+w"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(var x, paren(binary(var y, var z)), var w)");
  }
}

TEST_F(Test_Parse_Expression, await_unary_operator_inside_async_functions) {
  {
    Test_Parser p(u8"await myPromise"_sv);
    auto guard = p.enter_function(Function_Attributes::async);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(var myPromise)");
    EXPECT_EQ(ast->kind(), Expression_Kind::Await);
    EXPECT_EQ(summarize(ast->child_0()), "var myPromise");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 15));
  }

  {
    Test_Parser p(u8"await(x)"_sv);
    auto guard = p.enter_function(Function_Attributes::async);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(paren(var x))");
  }
}

TEST_F(Test_Parse_Expression, redundant_await) {
  {
    Test_Parser p(u8"await await p"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(await(var p))");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^^^^^ Diag_Redundant_Await"_diag,
                       });
  }

  {
    Test_Parser p(u8"await await await p"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(await(await(var p)))");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"      ^^^^^ Diag_Redundant_Await"_diag,  //
                           u8"^^^^^ Diag_Redundant_Await"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression, await_followed_by_arrow_function) {
  auto test = [](auto&& make_guard) -> void {
    {
      Test_Parser p(u8"await x => {}"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "asyncarrowfunc(var x)");
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"^^^^^ Diag_Await_Followed_By_Arrow_Function"_diag,
          });
    }

    {
      Test_Parser p(u8"await () => {}"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "asyncarrowfunc()");
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"^^^^^ Diag_Await_Followed_By_Arrow_Function"_diag,
          });
    }

    {
      Test_Parser p(u8"await (param) => {}"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "asyncarrowfunc(var param)");
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"^^^^^ Diag_Await_Followed_By_Arrow_Function"_diag,
          });
    }

    {
      Test_Parser p(u8"await (param) => { await param; }"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "asyncarrowfunc(var param)");
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"^^^^^ Diag_Await_Followed_By_Arrow_Function"_diag,
          });
    }

    {
      Test_Parser p(u8"await (param) => body"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      Expression* ast = p.parse_expression();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_variable_declaration",       // param
                                "visit_enter_function_scope_body",  //
                                "visit_variable_use",               // body
                                "visit_exit_function_scope",
                            }));
      EXPECT_EQ(summarize(ast), "asyncarrowfunc(var param)");
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"^^^^^ Diag_Await_Followed_By_Arrow_Function"_diag,
          });
    }

    // TODO(strager): Test an arrow function with TypeScript a return type
    // annotation.
  };

  {
    SCOPED_TRACE("in async function");
    test([](Test_Parser& p) {
      return p.enter_function(Function_Attributes::async);
    });
  }

  {
    SCOPED_TRACE("in non-async function");
    test([](Test_Parser& p) {
      return p.enter_function(Function_Attributes::normal);
    });
  }

  {
    SCOPED_TRACE("top-level");
    test([](Test_Parser&) -> int {
      return 0;  // No guard.
    });
  }

  {
    Test_Parser p(u8"await?.() => c"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(call(var await), var c)")
        << "'await' should not be treated as if it was 'async' in an arrow "
           "function.";
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"          ^^ Diag_Unexpected_Arrow_After_Expression.arrow\n"_diag
            u8"^^^^^^^^^ .expression"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression,
       await_in_normal_function_vs_async_function_vs_top_level) {
  static Parser_Options default_parser_options;

  Parser_Options no_jsx;
  no_jsx.jsx = false;

  Parser_Options jsx;
  jsx.jsx = true;

  struct Test_Case {
    String8_View code;
    const char* expected_normal_function;
    const char* expected_async_function;

    const Parser_Options& options = default_parser_options;
  };

  for (const Test_Case& test : {
         // clang-format off
         Test_Case

         // 'await' is either an identifier or a unary operator:
         {u8"await!+x"_sv,         "binary(nonnull(var await), var x)",          "await(unary(unary(var x)))", typescript_options},
         {u8"await/re/g"_sv,       "binary(var await, var re, var g)",           "await(literal)"},
         {u8"await+x"_sv,          "binary(var await, var x)",                   "await(unary(var x))"},
         {u8"await-x"_sv,          "binary(var await, var x)",                   "await(unary(var x))"},
         {u8"await<x>y</x>/g"_sv,  "binary(var await, var x, var y, literal)",   "binary(await(jsxelement(x)), var g)", jsx},
         {u8"await(x)"_sv,         "call(var await, var x)",                     "await(paren(var x))"},
         {u8"await(x,)"_sv,        "call(var await, var x)",                     "await(paren(trailingcomma(var x)))"},
         {u8"await(x, y)"_sv,      "call(var await, var x, var y)",              "await(paren(binary(var x, var y)))"},
         {u8"await[x]"_sv,         "index(var await, var x)",                    "await(array(var x))"},
         {u8"await++\nx"_sv,       "rwunarysuffix(var await)",                   "await(rwunary(var x))"},
         {u8"await--\nx"_sv,       "rwunarysuffix(var await)",                   "await(rwunary(var x))"},
         {u8"await`some${x}`"_sv,  "taggedtemplate(var await, var x)",           "await(template(var x))"},
         {u8"await`something`"_sv, "taggedtemplate(var await)",                  "await(literal)"},
         {u8"await/=re/g"_sv,      "upassign(var await, binary(var re, var g))", "await(literal)"},

         // 'await' must be a unary operator:
         {u8"await async () => {}"_sv, nullptr, "await(asyncarrowfunc())"},
         {u8"await await x"_sv,        nullptr, "await(await(var x))"},
         {u8"await class{}"_sv,        nullptr, "await(class)"},
         {u8"await delete x.p"_sv,     nullptr, "await(delete(dot(var x, p)))"},
         {u8"await function() {}"_sv,  nullptr, "await(function)"},
         {u8"await /regexp/"_sv,       nullptr, "await(literal)"},
         {u8"await /=regexp/"_sv,      nullptr, "await(literal)"},
         {u8"await 42"_sv,             nullptr, "await(literal)"},
         {u8"await false"_sv,          nullptr, "await(literal)"},
         {u8"await null"_sv,           nullptr, "await(literal)"},
         {u8"await this"_sv,           nullptr, "await(this)"},
         {u8"await true"_sv,           nullptr, "await(literal)"},
         {u8"await new C()"_sv,        nullptr, "await(new(var C))"},
         {u8"await++x"_sv,             nullptr, "await(rwunary(var x))"},
         {u8"await--x"_sv,             nullptr, "await(rwunary(var x))"},
         {u8"await super"_sv,          nullptr, "await(super)"},
         {u8"await typeof x"_sv,       nullptr, "await(typeof(var x))"},
         {u8"await !x"_sv,             nullptr, "await(unary(var x))"},
         {u8"await void x"_sv,         nullptr, "await(unary(var x))"},
         {u8"await ~x"_sv,             nullptr, "await(unary(var x))"},
         {u8"await as"_sv,             nullptr, "await(var as)"},
         {u8"await async"_sv,          nullptr, "await(var async)"},
         {u8"await from"_sv,           nullptr, "await(var from)"},
         {u8"await get"_sv,            nullptr, "await(var get)"},
         {u8"await let"_sv,            nullptr, "await(var let)"},
         {u8"await of"_sv,             nullptr, "await(var of)"},
         {u8"await set"_sv,            nullptr, "await(var set)"},
         {u8"await static"_sv,         nullptr, "await(var static)"},
         {u8"await x"_sv,              nullptr, "await(var x)"},
         {u8"await yield"_sv,          nullptr, "await(var yield)"},
         // TODO(strager): Fix these test cases:
#if 0
         {u8"await! x"_sv,             nullptr, "await(unary(var x))", typescript_options},
#endif

         // 'await' must be an identifier:
         {u8"[await!]"_sv,            "array(nonnull(var await))",                nullptr, typescript_options},
         {u8"[await]"_sv,             "array(var await)",                         nullptr},
         {u8"await => x"_sv,          "arrowfunc(var await)",                     nullptr},
         {u8"await! = x"_sv,          "assign(nonnull(var await), var x)",        nullptr, typescript_options},
         {u8"await = x"_sv,           "assign(var await, var x)",                 nullptr},
         {u8"await! * x"_sv,          "binary(nonnull(var await), var x)",        nullptr, typescript_options},
         {u8"await!, x"_sv,           "binary(nonnull(var await), var x)",        nullptr, typescript_options},
         {u8"await != x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await !== x"_sv,         "binary(var await, var x)",                 nullptr},
         {u8"await % x"_sv,           "binary(var await, var x)",                 nullptr},
         {u8"await & x"_sv,           "binary(var await, var x)",                 nullptr},
         {u8"await && x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await ** x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await / x"_sv,           "binary(var await, var x)",                 nullptr},
         {u8"await < x"_sv,           "binary(var await, var x)",                 nullptr},
         {u8"await << x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await <= x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await == x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await === x"_sv,         "binary(var await, var x)",                 nullptr},
         {u8"await > x"_sv,           "binary(var await, var x)",                 nullptr},
         {u8"await >= x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await >> x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await >>> x"_sv,         "binary(var await, var x)",                 nullptr},
         {u8"await ?? x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await ^ x"_sv,           "binary(var await, var x)",                 nullptr},
         {u8"await in xs"_sv,         "binary(var await, var xs)",                nullptr},
         {u8"await instanceof X"_sv,  "binary(var await, var X)",                 nullptr},
         {u8"await | x"_sv,           "binary(var await, var x)",                 nullptr},
         {u8"await || x"_sv,          "binary(var await, var x)",                 nullptr},
         {u8"await, x"_sv,            "binary(var await, var x)",                 nullptr},
         {u8"await<x>y</x>/g"_sv,     "binary(var await, var x, var y, literal)", nullptr, no_jsx},
         {u8"await ? x : y"_sv,       "cond(var await, var x, var y)",            nullptr},
         {u8"x ? await : y"_sv,       "cond(var x, var await, var y)",            nullptr},
         {u8"await &&= x"_sv,         "condassign(var await, var x)",             nullptr},
         {u8"await ?\x3f= x"_sv,      "condassign(var await, var x)",             nullptr},
         {u8"await ||= x"_sv,         "condassign(var await, var x)",             nullptr},
         {u8"await.prop"_sv,          "dot(var await, prop)",                     nullptr},
         {u8"await?.prop"_sv,         "dot(var await, prop)",                     nullptr},
         {u8"{key: await}"_sv,        "object(literal: var await)",               nullptr},
         {u8"await %= x"_sv,          "upassign(var await, var x)",               nullptr},
         {u8"await &= x"_sv,          "upassign(var await, var x)",               nullptr},
         {u8"await **= x"_sv,         "upassign(var await, var x)",               nullptr},
         {u8"await *= x"_sv,          "upassign(var await, var x)",               nullptr},
         {u8"await += x"_sv,          "upassign(var await, var x)",               nullptr},
         {u8"await -= x"_sv,          "upassign(var await, var x)",               nullptr},
         {u8"await /= x"_sv,          "upassign(var await, var x)",               nullptr},
         {u8"await <<= x"_sv,         "upassign(var await, var x)",               nullptr},
         {u8"await >>= x"_sv,         "upassign(var await, var x)",               nullptr},
         {u8"await >>>= x"_sv,        "upassign(var await, var x)",               nullptr},
         {u8"await ^= x"_sv,          "upassign(var await, var x)",               nullptr},
         {u8"await |= x"_sv,          "upassign(var await, var x)",               nullptr},
         {u8"await"_sv,               "var await",                                nullptr},
         {u8"(await)"_sv,             "paren(var await)",                         nullptr},
         {u8"await;"_sv,              "var await",                                nullptr},

         // TODO(strager): Fix these test cases:
#if 0
         {u8"await++;"_sv,            "rwunarysuffix(var await)",      nullptr},
         {u8"await--;"_sv,            "rwunarysuffix(var await)",      nullptr},
#endif
         // clang-format on
       }) {
    SCOPED_TRACE(out_string8(test.code));

    {
      // Normal function:
      Test_Parser p(test.code, test.options, capture_diags);
      auto guard = p.enter_function(Function_Attributes::normal);
      Expression* ast = p.parse_expression();

      if (test.code == u8"await--x"_sv || test.code == u8"await++x"_sv ||
          test.code == u8"await of"_sv) {
        // TODO(strager): Make these test cases pass.
      } else if (test.expected_normal_function) {
        // 'await' should look like an identifier.
        EXPECT_EQ(summarize(ast), test.expected_normal_function);
        EXPECT_THAT(p.errors, IsEmpty());
      } else {
        // 'await' doesn't look like an identifier. We should report an error
        // and recover as if 'await' was an operator.
        EXPECT_EQ(summarize(ast), test.expected_async_function);
        if (test.code == u8"await await x"_sv) {
          EXPECT_THAT(
              p.errors,
              ::testing::IsSupersetOf(
                  {DIAG_TYPE_OFFSETS(p.code, Diag_Await_Operator_Outside_Async,
                                     await_operator, 0, u8"await"_sv),
                   DIAG_TYPE_OFFSETS(p.code, Diag_Await_Operator_Outside_Async,
                                     await_operator, u8"await "_sv.size(),
                                     u8"await"_sv)}));
        } else {
          std::size_t await_offset = test.code.find(u8"await"_sv);
          EXPECT_THAT(p.errors,
                      ElementsAreArray({
                          DIAG_TYPE_OFFSETS(
                              p.code, Diag_Await_Operator_Outside_Async,  //
                              await_operator, await_offset, u8"await"_sv),
                      }));
        }
      }
    }

    if (test.expected_async_function) {
      // Async function:
      Test_Parser p(test.code, test.options, capture_diags);
      auto guard = p.enter_function(Function_Attributes::async);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), test.expected_async_function);
      if (test.code == u8"await await x"_sv) {
        assert_diagnostics(p.code, p.errors,
                           {
                               u8"^^^^^ Diag_Redundant_Await"_diag,
                           });
      } else {
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    {
      // Top level:
      Test_Parser p(test.code, test.options, capture_diags);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), test.expected_async_function
                                    ? test.expected_async_function
                                    : test.expected_normal_function);
      if (test.code == u8"await await x"_sv) {
        assert_diagnostics(p.code, p.errors,
                           {
                               u8"^^^^^ Diag_Redundant_Await"_diag,
                           });
      } else {
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }
  }
}

TEST_F(Test_Parse_Expression, await_variable_name_outside_async_functions) {
  {
    Test_Parser p(u8"await(x)"_sv);
    auto guard = p.enter_function(Function_Attributes::normal);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var await, var x)");
  }

  {
    Test_Parser p(u8"await < rhs"_sv, jsx_options);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var await, var rhs)");
  }
}

TEST_F(Test_Parse_Expression, await_unary_operator_outside_async_functions) {
  {
    Test_Parser p(u8"await myPromise"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::normal);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(var myPromise)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^^^^^ Diag_Await_Operator_Outside_Async"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression,
       yield_nullary_operator_inside_generator_functions) {
  auto parse_expression_in_generator = [](String8_View code) -> std::string {
    Test_Parser p(code);
    auto guard = p.enter_function(Function_Attributes::generator);
    Expression* ast = p.parse_expression();
    return summarize(ast);
  };

  {
    Test_Parser p(u8"yield"_sv);
    auto guard = p.enter_function(Function_Attributes::generator);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yieldnone");
    EXPECT_EQ(ast->kind(), Expression_Kind::Yield_None);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
  }

  EXPECT_EQ(parse_expression_in_generator(u8"(yield)"_sv), "paren(yieldnone)");
  EXPECT_EQ(parse_expression_in_generator(u8"[yield]"_sv), "array(yieldnone)");
  EXPECT_EQ(parse_expression_in_generator(u8"f(yield, 42)"_sv),
            "call(var f, yieldnone, literal)");
  EXPECT_EQ(parse_expression_in_generator(u8"yield ? a : b"_sv),
            "cond(yieldnone, var a, var b)");
  EXPECT_EQ(parse_expression_in_generator(u8"yield in stuff"_sv),
            "binary(yieldnone, var stuff)");
  EXPECT_EQ(parse_expression_in_generator(u8"yield;"_sv), "yieldnone");
  EXPECT_EQ(parse_expression_in_generator(u8"yield }"_sv), "yieldnone")
      << "'}' is the end of a function's body, for example";
  EXPECT_EQ(parse_expression_in_generator(u8"a ? yield : b"_sv),
            "cond(var a, yieldnone, var b)");
  EXPECT_EQ(parse_expression_in_generator(u8"yield, yield"_sv),
            "binary(yieldnone, yieldnone)");
  EXPECT_EQ(parse_expression_in_generator(u8"[yield, yield, yield]"_sv),
            "array(yieldnone, yieldnone, yieldnone)");
}

TEST_F(Test_Parse_Expression, yield_unary_operator_inside_generator_functions) {
  {
    Test_Parser p(u8"yield v"_sv);
    auto guard = p.enter_function(Function_Attributes::generator);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yield(var v)");
    EXPECT_EQ(ast->kind(), Expression_Kind::Yield_One);
    EXPECT_EQ(summarize(ast->child_0()), "var v");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
  }

  {
    Test_Parser p(u8"yield(x)"_sv);
    auto guard = p.enter_function(Function_Attributes::generator);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yield(paren(var x))");
  }

  {
    Test_Parser p(u8"f(yield a, yield b, c)}"_sv);
    auto generator_guard = p.enter_function(Function_Attributes::generator);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f, yield(var a), yield(var b), var c)");
  }
}

TEST_F(Test_Parse_Expression,
       yield_many_unary_operator_inside_generator_functions) {
  {
    Test_Parser p(u8"yield *other"_sv);
    auto guard = p.enter_function(Function_Attributes::generator);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yieldmany(var other)");
    EXPECT_EQ(ast->kind(), Expression_Kind::Yield_Many);
    EXPECT_EQ(summarize(ast->child_0()), "var other");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 12));
  }

  {
    Test_Parser p(u8"f(yield *a, yield* b, c)}"_sv);
    auto generator_guard = p.enter_function(Function_Attributes::generator);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "call(var f, yieldmany(var a), yieldmany(var b), var c)");
  }
}

TEST_F(Test_Parse_Expression, yield_variable_name_outside_generator_functions) {
  {
    Test_Parser p(u8"yield(x)"_sv);
    auto guard = p.enter_function(Function_Attributes::normal);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var yield, var x)");
  }

  {
    Test_Parser p(u8"yield*other"_sv);
    auto guard = p.enter_function(Function_Attributes::normal);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var yield, var other)");
  }
}

TEST_F(Test_Parse_Expression, parse_new_expression) {
  {
    Test_Parser p(u8"new Date"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::New);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var Date");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 8));
  }

  {
    Test_Parser p(u8"new Date()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::New);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var Date");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 10));
  }

  {
    Test_Parser p(u8"new Date(y,m,d)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Date, var y, var m, var d)");
  }
}

TEST_F(Test_Parse_Expression, new_target) {
  {
    Test_Parser p(u8"new.target"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "newtarget");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 10));
  }

  {
    Test_Parser p(u8"new.target()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(newtarget)");
  }
}

TEST_F(Test_Parse_Expression, super) {
  {
    Test_Parser p(u8"super()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(super)");
  }

  {
    Test_Parser p(u8"super.method()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(dot(super, method))");
  }
}

TEST_F(Test_Parse_Expression, import) {
  {
    Test_Parser p(u8"import(url)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(import, var url)");
  }

  {
    Test_Parser p(u8"import.meta"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(import, meta)");
  }
}

TEST_F(Test_Parse_Expression, parse_assignment) {
  {
    Test_Parser p(u8"x=y"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
  }

  {
    Test_Parser p(u8"x.p=z"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Assignment);
    EXPECT_EQ(summarize(ast->child_0()), "dot(var x, p)");
    EXPECT_EQ(summarize(ast->child_1()), "var z");
  }

  {
    Test_Parser p(u8"f().p=x"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(dot(call(var f), p), var x)");
  }

  {
    Test_Parser p(u8"x=y=z"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var x, assign(var y, var z))");
  }

  {
    Test_Parser p(u8"x,y=z,w"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, assign(var y, var z), var w)");
  }
}

TEST_F(Test_Parse_Expression, parse_destructuring_assignment) {
  {
    Test_Parser p(u8"[x,y]=[z,w]"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "assign(array(var x, var y), array(var z, var w))");
  }

  {
    Test_Parser p(u8"{k} = o"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(object(literal: var k), var o)");
  }

  {
    Test_Parser p(u8"{k: k2} = o"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(object(literal: var k2), var o)");
  }

  {
    Test_Parser p(u8"{k = defaultValue} = o"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "assign(object(literal: var k = var defaultValue), var o)");
  }
}

TEST_F(Test_Parse_Expression, parse_compound_assignment) {
  for (String8_View op :
       {u8"*="_sv, u8"/="_sv, u8"%="_sv, u8"+="_sv, u8"-="_sv, u8"<<="_sv,
        u8">>="_sv, u8">>>="_sv, u8"&="_sv, u8"^="_sv, u8"|="_sv, u8"**="_sv}) {
    SCOPED_TRACE(out_string8(op));
    Test_Parser p(concat(u8"x "_sv, op, u8" y"_sv));
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Compound_Assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(
            0, narrow_cast<CLI_Source_Position::Offset_Type>(p.code.size())));
  }
}

TEST_F(Test_Parse_Expression, parse_conditional_assignment) {
  for (String8_View op : {u8"&&="_sv, u8"?\x3f="_sv, u8"||="_sv}) {
    SCOPED_TRACE(out_string8(op));
    Test_Parser p(concat(u8"x "_sv, op, u8" y"_sv));
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Conditional_Assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(
            0, narrow_cast<CLI_Source_Position::Offset_Type>(p.code.size())));
  }
}

TEST_F(Test_Parse_Expression, parse_invalid_assignment) {
  {
    Test_Parser p(u8"x+y=z"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(binary(var x, var y), var z)");

    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^^ Diag_Invalid_Expression_Left_Of_Assignment"_diag,
        });
  }

  for (String8_View code : {
           u8"f()=x"_sv,
           u8"-x=y"_sv,
           u8"42=y"_sv,
           u8"(x=y)=z"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code, capture_diags);
    p.parse_expression();

    assert_diagnostics(p.code, p.errors,
                       {
                           u8"Diag_Invalid_Expression_Left_Of_Assignment"_diag,
                       });
  }

  for (String8_View code : {
           u8"f()! = x"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    Test_Parser p(code, typescript_options, capture_diags);
    p.parse_expression();

    assert_diagnostics(p.code, p.errors,
                       {
                           u8"Diag_Invalid_Expression_Left_Of_Assignment"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression, parse_prefix_plusplus_minusminus) {
  {
    Test_Parser p(u8"++x"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::RW_Unary_Prefix);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
  }

  {
    Test_Parser p(u8"--y"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::RW_Unary_Prefix);
    EXPECT_EQ(summarize(ast->child_0()), "var y");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
  }
}

TEST_F(Test_Parse_Expression, parse_prefix_plusplus_plus_operand) {
  {
    Test_Parser p(u8"++x\n+\ny"_sv);

    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(rwunary(var x), var y)");
  }

  {
    Test_Parser p(u8"--x\n+\ny"_sv);

    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(rwunary(var x), var y)");
  }

  {
    Test_Parser p(u8"++x.y"_sv);

    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(dot(var x, y))");
  }

  {
    Test_Parser p(u8"++x[y]"_sv);

    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(index(var x, var y))");
  }
}

TEST_F(Test_Parse_Expression, parse_unary_prefix_operator_with_no_operand) {
  {
    Test_Parser p(u8"--"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(missing)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"++;"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(missing)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"(-)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(unary(missing))");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8" ^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"!;"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(missing)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }

  {
    Test_Parser p(u8"await}"_sv, capture_diags);
    auto guard = p.enter_function(Function_Attributes::async);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(missing)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^^^^^ Diag_Missing_Operand_For_Operator"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression, parse_suffix_plusplus_minusminus) {
  {
    Test_Parser p(u8"x++"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::RW_Unary_Suffix);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
  }
}

TEST_F(Test_Parse_Expression, suffix_plusplus_minusminus_disallows_line_break) {
  {
    Test_Parser p(u8"x\n++\ny"_sv);

    Expression* ast_1 = p.parse_expression();
    EXPECT_EQ(summarize(ast_1), "var x");

    Expression* ast_2 = p.parse_expression();
    EXPECT_EQ(summarize(ast_2), "rwunary(var y)");
  }
}

TEST_F(Test_Parse_Expression, prefix_plusplus_minusminus_cannot_nest) {
  {
    Test_Parser p(u8"++ ++ x"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(rwunary(var x))");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
    // TODO(strager): Report an error. ++ takes a LeftHandExpression, but ++x is
    // not a LeftHandExpression.
  }
}

TEST_F(Test_Parse_Expression, parse_template) {
  {
    Test_Parser p(u8"`hello`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
  }

  {
    Test_Parser p(u8"`hello${world}`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Template);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var world");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 15));
  }

  {
    Test_Parser p(u8"`${one}${two}${three}`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var one, var two, var three)");
  }

  {
    Test_Parser p(u8"`hello${world}` + rhs"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(template(var world), var rhs)");
  }
}

TEST_F(Test_Parse_Expression, tagged_template_literal) {
  {
    Test_Parser p(u8"hello`world`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Tagged_Template_Literal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var hello");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 12));
  }

  {
    Test_Parser p(u8"hello`template ${literal} thingy`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Tagged_Template_Literal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var hello");
    EXPECT_EQ(summarize(ast->child(1)), "var literal");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 33));
  }

  {
    Test_Parser p(u8"a.b()`c`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(call(dot(var a, b)))");
  }

  {
    Test_Parser p(u8"tag`template`.property"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(taggedtemplate(var tag), property)");
  }

  {
    Test_Parser p(u8"x + tag`template`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, taggedtemplate(var tag))");
  }
}

TEST_F(Test_Parse_Expression, optional_tagged_template_literal) {
  {
    Test_Parser p(u8"hello?.`world`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var hello)");
  }

  {
    Test_Parser p(u8"hello?.`template ${literal} thingy`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var hello, var literal)");
  }
}

TEST_F(Test_Parse_Expression, Diag_Expected_Expression_In_Template_Literal) {
  test_parse_and_visit_expression(
      u8"`${}`"_sv,  //
      u8"   ` Diag_Expected_Expression_In_Template_Literal"_diag);
  test_parse_and_visit_expression(
      u8"`${ }`"_sv,  //
      u8"   ^ Diag_Expected_Expression_In_Template_Literal"_diag);
  test_parse_and_visit_expression(
      u8"`${  }`"_sv,  //
      u8"   ^^ Diag_Expected_Expression_In_Template_Literal"_diag);
  test_parse_and_visit_expression(
      u8"`a${}b${c}`"_sv,  //
      u8"    ` Diag_Expected_Expression_In_Template_Literal"_diag);
  test_parse_and_visit_expression(u8R"(`\${}`)"_sv,  //
                                  no_diags);
}

TEST_F(Test_Parse_Expression, untagged_template_with_invalid_escape) {
  {
    Test_Parser p(u8R"(`invalid\uescape`)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "literal");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
                       });
  }

  {
    Test_Parser p(u8R"(`invalid\u${expr}escape`)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var expr)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
                       });
  }

  {
    Test_Parser p(u8R"(`invalid${expr}\uescape`)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var expr)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
                       });
  }

  {
    Test_Parser p(u8R"(`invalid${expr1}\u${expr2}escape`)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var expr1, var expr2)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"Diag_Expected_Hex_Digits_In_Unicode_Escape"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression,
       tagged_template_with_invalid_escape_reports_no_error) {
  {
    Test_Parser p(u8R"(tag`invalid\uescape`)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var tag)");
  }

  {
    Test_Parser p(u8R"(tag`invalid\uescape${expr}`)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var tag, var expr)");
  }

  {
    Test_Parser p(u8R"(tag?.`invalid\uescape`)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var tag)");
  }

  {
    Test_Parser p(u8R"(tag?.`invalid\uescape${expr}`)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var tag, var expr)");
  }
}

TEST_F(Test_Parse_Expression, array_literal) {
  {
    Test_Parser p(u8"[]"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Array);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 2));
  }

  {
    Test_Parser p(u8"[x]"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Array);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
  }

  {
    Test_Parser p(u8"[x, y]"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Array);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_EQ(summarize(ast->child(1)), "var y");
  }

  {
    Test_Parser p(u8"[,,x,,y,,]"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "array(var x, var y)");
  }

  {
    // Comma should be parsed as an array separator, not as a comma operator.
    Test_Parser p(u8"[await myPromise,]"_sv);
    auto guard = p.enter_function(Function_Attributes::async);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "array(await(var myPromise))");
  }
}

TEST_F(Test_Parse_Expression, malformed_array_literal) {
  {
    Test_Parser p(u8"[ "_sv, capture_diags);
    Expression* ast = p.parse_expression();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Missing_Array_Close.left_square\n"_diag
                           u8" ` .expected_right_square"_diag,
                       });
    EXPECT_EQ(summarize(ast), "array()");
  }

  {
    Test_Parser p(u8"[ x"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Missing_Array_Close.left_square\n"_diag
                           u8"   ` .expected_right_square"_diag,
                       });
    EXPECT_EQ(summarize(ast), "array(var x)");
  }

  {
    Test_Parser p(u8"[\nif (true) {}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Missing_Array_Close.left_square\n"_diag
                           u8" ` .expected_right_square"_diag,
                       });
    EXPECT_EQ(summarize(ast), "array()");
  }

  {
    Test_Parser p(u8"[a b]"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    assert_diagnostics(p.code, p.errors,
        {
            u8"Diag_Missing_Comma_Between_Array_Elements"_diag,
        });
    EXPECT_EQ(summarize(ast), "array(var a, var b)");
  }
}

TEST_F(Test_Parse_Expression, object_literal) {
  {
    Test_Parser p(u8"{}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 2));
  }

  {
    Test_Parser p(u8"{key: value}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
    EXPECT_FALSE(ast->object_entry(0).init);
  }

  {
    Test_Parser p(u8"{key1: value1, key2: value2}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 2);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value1");
    EXPECT_EQ(summarize(ast->object_entry(1).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(1).value), "var value2");
  }

  {
    Test_Parser p(u8"{'key': value}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
  }

  {
    Test_Parser p(u8"{[key]: value}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var key");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
  }

  {
    Test_Parser p(u8"{thing}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    auto entry = ast->object_entry(0);
    EXPECT_EQ(summarize(entry.property), "literal");
    EXPECT_THAT(entry.property->span(), p.matches_offsets(1, 6));
    EXPECT_EQ(summarize(entry.value), "var thing");
    EXPECT_THAT(entry.value->span(), p.matches_offsets(1, 6));
    EXPECT_FALSE(entry.init);
  }

  {
    Test_Parser p(u8"{key1: value1, thing2, key3: value3}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: var value1, literal: var thing2, literal: var "
              "value3)");
  }

  {
    Test_Parser p(u8"{key: variable = value}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    const Object_Property_Value_Pair& entry = ast->object_entry(0);
    EXPECT_EQ(summarize(entry.property), "literal");
    EXPECT_EQ(summarize(entry.value), "var variable");
    EXPECT_EQ(summarize(entry.init), "var value");
    EXPECT_THAT(entry.init_equals_span(),
                p.matches_offsets(u8"{key: variable "_sv.size(), u8"="_sv));
  }

  {
    Test_Parser p(u8"{key = value}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    const Object_Property_Value_Pair& entry = ast->object_entry(0);
    EXPECT_EQ(summarize(entry.property), "literal");
    EXPECT_EQ(summarize(entry.value), "var key");
    EXPECT_EQ(summarize(entry.init), "var value");
    EXPECT_THAT(entry.init_equals_span(),
                p.matches_offsets(u8"{key "_sv.size(), u8"="_sv));
  }

  {
    Test_Parser p(u8"{...other, k: v}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 2);
    EXPECT_FALSE(ast->object_entry(0).property);
    EXPECT_EQ(summarize(ast->object_entry(0).value), "spread(var other)");
    EXPECT_EQ(summarize(ast->object_entry(1).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(1).value), "var v");
    EXPECT_EQ(summarize(ast), "object(spread(var other), literal: var v)");
  }

  {
    Test_Parser p(u8"{...other = init}"_sv);
    EXPECT_EQ(summarize(p.parse_expression()),
              "object(spread(assign(var other, var init)))");
  }
}

TEST_F(Test_Parse_Expression, object_literal_with_method_key) {
  {
    Test_Parser p(u8"{ func(a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 16));
  }

  {
    Test_Parser p(u8"{ 'func'(a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 18));
  }

  {
    Test_Parser p(u8"{ [func](a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 18));
  }

  {
    Test_Parser p(u8"{ async func(a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 22));
  }

  {
    Test_Parser p(u8"{ async 'func'(a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    Test_Parser p(u8"{ async [func](a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    Test_Parser p(u8"{ *func(a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    Test_Parser p(u8"{ *'func'(a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    Test_Parser p(u8"{ *[func](a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    Test_Parser p(u8"{ async *func(a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    Test_Parser p(u8"{ async *'func'(a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    Test_Parser p(u8"{ async *[func](a, b) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }
}

TEST_F(Test_Parse_Expression, object_literal_with_getter_setter_key) {
  {
    Test_Parser p(u8"{ get prop() { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 16));
  }

  {
    Test_Parser p(u8"{ set prop(v) { } }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 17));
  }

  {
    Test_Parser p(u8"{get 1234() { }}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
  }

  {
    Test_Parser p(u8"{get 'string key'() { }}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
  }

  {
    Test_Parser p(u8"{get [expression + key]() { }}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(binary(var expression, var key): function)");
  }
}

TEST_F(Test_Parse_Expression, object_literal_with_keyword_key) {
  for (String8_View keyword :
       {u8"async"_sv, u8"catch"_sv, u8"class"_sv, u8"default"_sv,
        u8"function"_sv, u8"get"_sv, u8"set"_sv, u8"try"_sv}) {
    SCOPED_TRACE(out_string8(keyword));

    {
      Test_Parser p(concat(u8"{"_sv, keyword, u8": null}"_sv));
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: literal)");
    }

    {
      Test_Parser p(concat(u8"{"_sv, keyword, u8"() { }}"_sv));
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{get "_sv, keyword, u8"() {}}"_sv));
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{set "_sv, keyword, u8"() {}}"_sv));
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{async "_sv, keyword, u8"() {}}"_sv));
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{*"_sv, keyword, u8"() {}}"_sv));
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }
  }
}

TEST_F(Test_Parse_Expression, object_literal_with_contextual_keyword_keyvalue) {
  for (String8_View keyword :
       {u8"as"_sv, u8"async"_sv, u8"await"_sv, u8"from"_sv, u8"get"_sv,
        u8"let"_sv, u8"of"_sv, u8"private"_sv, u8"protected"_sv, u8"public"_sv,
        u8"set"_sv, u8"static"_sv, u8"yield"_sv}) {
    SCOPED_TRACE(out_string8(keyword));

    {
      Test_Parser p(concat(u8"{"_sv, keyword, u8"}"_sv));
      Expression* ast = p.parse_expression();
      EXPECT_EQ(ast->kind(), Expression_Kind::Object);
      EXPECT_EQ(ast->object_entry_count(), 1);
      EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
      EXPECT_EQ(ast->object_entry(0).value->kind(), Expression_Kind::Variable);
      EXPECT_EQ(
          ast->object_entry(0).value->variable_identifier().normalized_name(),
          keyword);
    }

    {
      Test_Parser p(concat(u8"{"_sv, keyword, u8", other}"_sv));
      Expression* ast = p.parse_expression();
      EXPECT_EQ(ast->kind(), Expression_Kind::Object);
      EXPECT_EQ(ast->object_entry_count(), 2);
      EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
      EXPECT_EQ(ast->object_entry(0).value->kind(), Expression_Kind::Variable);
      EXPECT_EQ(
          ast->object_entry(0).value->variable_identifier().normalized_name(),
          keyword);
      EXPECT_EQ(summarize(ast->object_entry(1).value), "var other");
    }
  }
}

TEST_F(Test_Parse_Expression,
       object_literal_with_reserved_keyword_keyvalue_is_an_error) {
  // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
  for (String8_View keyword : disallowed_binding_identifier_keywords) {
    SCOPED_TRACE(out_string8(keyword));

    {
      Test_Parser p(concat(u8"{"_sv, keyword, u8"}"_sv), capture_diags);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: missing)");
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code,
                                Diag_Missing_Value_For_Object_Literal_Entry,  //
                                key, u8"{"_sv.size(), keyword),
          }));
    }

    {
      Test_Parser p(concat(u8"{"_sv, keyword, u8", other}"_sv), capture_diags);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: missing, literal: var other)");
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code,
                                Diag_Missing_Value_For_Object_Literal_Entry,  //
                                key, u8"{"_sv.size(), keyword),
          }));
    }
  }
}

TEST_F(
    Test_Parse_Expression,
    object_literal_with_reserved_keyword_keyvalue_with_unicode_escapes_is_an_error) {
  {
    Test_Parser p(u8"{ \\u{69}f }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var if)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^^^^^^^ Diag_Keywords_Cannot_Contain_Escape_Sequences"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, object_literal_with_number_key) {
  {
    Test_Parser p(u8"{1234: null}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: literal)");
  }

  {
    Test_Parser p(u8"{async 42() {}}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
  }

  {
    Test_Parser p(u8"{*42() {}}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
  }
}

TEST_F(Test_Parse_Expression, incomplete_object_literal) {
  {
    Test_Parser p(u8"{ p1 "_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var p1)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Unclosed_Object_Literal.object_open\n"_diag
                           u8"    ` .expected_object_close"_diag,
                       });
  }

  {
    Test_Parser p(u8"{ p1, "_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var p1)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_Unclosed_Object_Literal.object_open\n"_diag
                           u8"     ` .expected_object_close"_diag,
                       });
  }

  {
    Test_Parser p(u8"({ p1, )"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(object(literal: var p1))");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^ Diag_Unclosed_Object_Literal.object_open\n"_diag
            u8"      ` .expected_object_close"_diag,
        });
  }

  {
    Test_Parser p(u8"[{ p1, ]"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "array(object(literal: var p1))");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^ Diag_Unclosed_Object_Literal.object_open\n"_diag
            u8"      ` .expected_object_close"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, malformed_object_literal) {
  {
    Test_Parser p(u8"{p1: v1 p2}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var v1, literal: var p2)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ` Diag_Missing_Comma_Between_Object_Literal_Entries"_diag,
        });
  }

  {
    Test_Parser p(u8"{1234}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: missing)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^^^^ Diag_Invalid_Lone_Literal_In_Object_Literal"_diag,
        });
  }

  {
    Test_Parser p(u8"{'x'}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: missing)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^^^ Diag_Invalid_Lone_Literal_In_Object_Literal"_diag,
        });
  }

  {
    Test_Parser p(u8"{a b: c}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var a, literal: var c)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ` Diag_Missing_Comma_Between_Object_Literal_Entries"_diag,
        });
  }

  {
    Test_Parser p(u8"{a *generator() {}}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var a, literal: function)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ` Diag_Missing_Comma_Between_Object_Literal_Entries"_diag,
        });
  }

  {
    Test_Parser p(u8"{async f}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"        ` Diag_Missing_Function_Parameter_List"_diag,
        });
  }

  {
    Test_Parser p(u8"{*f}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"   ` Diag_Missing_Function_Parameter_List"_diag,
                       });
  }

  {
    Test_Parser p(u8"{function a(){}}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^^^^^^^^ Diag_Methods_Should_Not_Use_Function_Keyword"_diag,
        });
  }

  {
    Test_Parser p(u8"{async function a(){}}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ^^^^^^^^ Diag_Methods_Should_Not_Use_Function_Keyword"_diag,
        });
  }

  {
    Test_Parser p(u8"{function *a(){}}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^^^^^^^^ Diag_Methods_Should_Not_Use_Function_Keyword"_diag,
        });
  }

  {
    Test_Parser p(u8"{ [x] }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var x: missing)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^^^ Diag_Missing_Value_For_Object_Literal_Entry"_diag,
        });
  }

  {
    Test_Parser p(u8"{ [x], other }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var x: missing, literal: var other)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^^^ Diag_Missing_Value_For_Object_Literal_Entry"_diag,
        });
  }

  for (String8_View op : {
           u8"!="_sv,  u8"!=="_sv,    u8"%"_sv,   u8"%="_sv,   u8"&"_sv,
           u8"&&"_sv,  u8"&&="_sv,    u8"&="_sv,  u8"*"_sv,    u8"**"_sv,
           u8"**="_sv, u8"*="_sv,     u8"+"_sv,   u8"+="_sv,   u8"-"_sv,
           u8"-="_sv,  u8"."_sv,      u8"/="_sv,  u8"<<"_sv,   u8"<<="_sv,
           u8"<="_sv,  u8"=="_sv,     u8"==="_sv, u8">"_sv,    u8">="_sv,
           u8">>"_sv,  u8">>="_sv,    u8">>>"_sv, u8">>>="_sv, u8"?."_sv,
           u8"??"_sv,  u8"?\x3f="_sv, u8"^"_sv,   u8"^="_sv,   u8"|"_sv,
           u8"|="_sv,  u8"||"_sv,     u8"||="_sv,
       }) {
    Test_Parser p(concat(u8"{one "_sv, op, u8" two}"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    Expression* ast = p.parse_expression();
    EXPECT_THAT(
        summarize(ast),
        ::testing::AnyOf("object(literal: binary(var one, var two))",
                         "object(literal: condassign(var one, var two))",
                         "object(literal: dot(var one, two))",
                         "object(literal: upassign(var one, var two))"));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Key_For_Object_Entry,  //
                              expression, u8"{"_sv.size(),
                              concat(u8"one "_sv, op, u8" two"_sv)),
        }));
  }

  for (String8_View op : {
           u8"!="_sv, u8"!=="_sv, u8"%"_sv, u8"&"_sv,  u8"&&"_sv, u8"*"_sv,
           u8"**"_sv, u8"+"_sv,   u8"-"_sv, u8"."_sv,  u8"<<"_sv, u8"<="_sv,
           u8"=="_sv, u8"==="_sv, u8">"_sv, u8">="_sv, u8">>"_sv, u8">>>"_sv,
           u8"?."_sv, u8"??"_sv,  u8"^"_sv, u8"|"_sv,  u8"||"_sv,
       }) {
    {
      Test_Parser p(concat(u8"{'one' "_sv, op, u8" two}"_sv), capture_diags);
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_THAT(summarize(ast),
                  ::testing::AnyOf("object(literal: assign(literal, var two))",
                                   "object(literal: binary(literal, var two))",
                                   "object(literal: dot(literal, two))",
                                   "object(literal: literal = var two)"));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Key_For_Object_Entry,  //
                                expression, u8"{"_sv.size(),
                                concat(u8"'one' "_sv, op, u8" two"_sv)),
          }));
    }

    {
      Test_Parser p(concat(u8"{1234 "_sv, op, u8" two}"_sv), capture_diags);
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_THAT(summarize(ast),
                  ::testing::AnyOf("object(literal: assign(literal, var two))",
                                   "object(literal: binary(literal, var two))",
                                   "object(literal: dot(literal, two))",
                                   "object(literal: literal = var two)"));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Key_For_Object_Entry,  //
                                expression, u8"{"_sv.size(),
                                concat(u8"1234 "_sv, op, u8" two"_sv)),
          }));
    }
  }

  for (String8_View op : {u8"++"_sv, u8"--"_sv}) {
    Test_Parser p(concat(u8"{one "_sv, op, u8" two}"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: rwunarysuffix(var one), literal: var two)");
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Missing_Key_For_Object_Entry,  //
                              expression, u8"{"_sv.size(),
                              concat(u8"one "_sv, op)),
            // TODO(strager): Don't report
            // Diag_Missing_Comma_Between_Object_Literal_Entries.
            DIAG_TYPE(Diag_Missing_Comma_Between_Object_Literal_Entries),
        }));
  }

  {
    Test_Parser p(u8"{#key: value}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var value)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^^^^ Diag_Private_Properties_Are_Not_Allowed_In_Object_Literals"_diag,
        });
  }

  {
    Test_Parser p(u8"{#key, [other]: value}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var other: var value)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^^^^ Diag_Private_Properties_Are_Not_Allowed_In_Object_Literals"_diag,
        });
  }

  for (String8_View prefix : {u8""_sv, u8"async "_sv, u8"get "_sv, u8"set "_sv,
                              u8"*"_sv, u8"async *"_sv}) {
    Padded_String code(concat(u8"{ "_sv, prefix, u8"#method() { } }"_sv));
    SCOPED_TRACE(code);
    Test_Parser p(code.string_view(), capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                Diag_Private_Properties_Are_Not_Allowed_In_Object_Literals,  //
                private_identifier, concat(u8"{ "_sv, prefix).size(),
                u8"#method"_sv),
        }));
  }
}

// In some other languages, ';' separates entries similar to how ',' does in
// JavaScript.
TEST_F(Test_Parse_Expression,
       object_literal_entries_are_not_separated_by_semicolon) {
  {
    Test_Parser p(u8"{ key: value; other: second }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: var value, literal: var second)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"            ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,
        });
  }

  {
    Test_Parser p(u8"{ first; get; set; async; }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: var first, literal: var get, "
              "literal: var set, literal: var async)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                        ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,  //
            u8"                 ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,  //
            u8"            ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,  //
            u8"       ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,
        });
  }

  {
    Test_Parser p(u8"{ [key]; other }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var key: missing, literal: var other)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,  //
            u8"  ^^^^^ Diag_Missing_Value_For_Object_Literal_Entry"_diag,
        });
  }
}

// On some keyboards, '<' is input by pressing ',' while holding the SHIFT key.
TEST_F(Test_Parse_Expression,
       object_literal_entries_are_not_separated_by_less_than_symbol) {
  {
    Test_Parser p(u8"{ first< get< set< async< }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: var first, literal: var get, "
              "literal: var set, literal: var async)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"                        ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,  //
            u8"                 ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,  //
            u8"            ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,  //
            u8"       ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,
        });
  }

  {
    Test_Parser p(u8"{ [key]< other }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var key: missing, literal: var other)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"       ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,  //
            u8"  ^^^^^ Diag_Missing_Value_For_Object_Literal_Entry"_diag,
        });
  }

  {
    Test_Parser p(u8"{ method() {}< other }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function, literal: var other)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"             ^ Diag_Expected_Comma_To_Separate_Object_Literal_Entries"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression,
       object_literal_generator_method_with_misplaced_star) {
  {
    Test_Parser p(u8"{method*() { yield 42; }}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^^^^^^ Diag_Generator_Function_Star_Belongs_Before_Name.function_name\n"_diag
            u8"       ^ .star"_diag,
        });
  }

  {
    Test_Parser p(u8"{ [computed] *() { yield 42; }}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var computed: function)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^^^^^^^^^^ Diag_Generator_Function_Star_Belongs_Before_Name.function_name\n"_diag
            u8"             ^ .star"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, parse_comma_expression) {
  {
    Test_Parser p(u8"x,y,z"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Binary_Operator);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_EQ(summarize(ast->child(1)), "var y");
    EXPECT_EQ(summarize(ast->child(2)), "var z");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
  }

  {
    Test_Parser p(u8"(x+(y,z)+w)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "paren(binary(var x, paren(binary(var y, var z)), var w))");
  }

  {
    Test_Parser p(u8"`${2+2, four}`"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(binary(literal, literal, var four))");
  }

  {
    Test_Parser p(u8"i = 0, j = 0"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(assign(var i, literal), assign(var j, literal))");
  }
}

TEST_F(Test_Parse_Expression, binary_operator_span) {
  for (String8_View op : {
           u8"!="_sv, u8"!=="_sv, u8"%"_sv,  u8"&"_sv,   u8"&&"_sv, u8"*"_sv,
           u8"**"_sv, u8"+"_sv,   u8","_sv,  u8"-"_sv,   u8"/"_sv,  u8"<"_sv,
           u8"<<"_sv, u8"<="_sv,  u8"=="_sv, u8"==="_sv, u8">"_sv,  u8">="_sv,
           u8">>"_sv, u8">>>"_sv, u8"??"_sv, u8"^"_sv,   u8"|"_sv,  u8"||"_sv,
       }) {
    Test_Parser p(concat(u8"x"_sv, op, u8"y"_sv));
    SCOPED_TRACE(p.code);
    Expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), Expression_Kind::Binary_Operator);
    auto* binary = static_cast<Expression::Binary_Operator*>(ast);
    EXPECT_THAT(
        binary->operator_spans_[0],
        p.matches_offsets(u8"x"_sv.size(), concat(u8"x"_sv, op).size()));
  }

  {
    Test_Parser p(u8"x + y * z"_sv);
    auto* ast = static_cast<Expression::Binary_Operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0],
                p.matches_offsets(u8"x "_sv.size(), u8"+"_sv));
    EXPECT_THAT(ast->operator_spans_[1],
                p.matches_offsets(u8"x + y "_sv.size(), u8"*"_sv));
  }

  {
    Test_Parser p(u8"x.'foo'"_sv, capture_diags);
    auto* ast = static_cast<Expression::Binary_Operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0], p.matches_offsets(1, 2));
    // Ignore p.errors.
  }

  {
    Test_Parser p(u8"x .. y"_sv, capture_diags);
    auto* ast = static_cast<Expression::Binary_Operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0],
                p.matches_offsets(u8"x ."_sv.size(), u8"."_sv));
    // Ignore p.errors.
  }

  {
    Test_Parser p(u8"x in y"_sv);
    auto* ast = static_cast<Expression::Binary_Operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0],
                p.matches_offsets(u8"x "_sv.size(), u8"in"_sv));
  }

  {
    Test_Parser p(u8"f(x y)"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    auto* binary = static_cast<Expression::Binary_Operator*>(ast->child_1());
    EXPECT_THAT(binary->operator_spans_[0],
                p.matches_offsets(u8"f(x"_sv.size(), u8""_sv));
    // Ignore p.errors.
  }

  {
    Test_Parser p(u8"x.y => z"_sv, capture_diags);
    auto* ast = static_cast<Expression::Binary_Operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0],
                p.matches_offsets(u8"x.y "_sv.size(), u8"=>"_sv));
    // Ignore p.errors.
  }

  {
    Test_Parser p(u8"f() => {}"_sv, capture_diags);
    auto* ast = static_cast<Expression::Binary_Operator*>(p.parse_expression());
    // FIXME(strager): These spans look weird.
    EXPECT_THAT(ast->operator_spans_[0], p.matches_offsets(0, u8"f("_sv));
    // Ignore p.errors.
  }
}

TEST_F(Test_Parse_Expression, parse_function_expression) {
  {
    Test_Parser p(u8"function(){} /* */"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 12));
  }

  {
    Test_Parser p(u8"function(x, y){}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Function);
  }

  {
    Test_Parser p(u8"function(){}()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Call);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(ast->child_0()->kind(), Expression_Kind::Function);
  }

  {
    Test_Parser p(u8"function f(){}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Named_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"f"_sv);
  }
}

TEST_F(Test_Parse_Expression, function_with_destructuring_parameters) {
  {
    Test_Parser p(u8"function({a, b}) { c }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function");
  }

  {
    Test_Parser p(u8"function([a, b]) { c }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function");
  }

  {
    Test_Parser p(u8"function({ a ) { c }"_sv, capture_diags);
    p.parse_expression();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"         ^ Diag_Unclosed_Object_Literal.object_open\n"_diag
            u8"            ` .expected_object_close"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, function_with_spread_and_comma) {
  {
    Test_Parser p(u8"function(...a, ) { b; }"_sv, capture_diags);
    p.parse_expression();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"             ^ Diag_Comma_Not_Allowed_After_Spread_Parameter.comma\n"_diag
            u8"         ^^^^ .spread"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, async_function_expression) {
  {
    Test_Parser p(u8"async function(){}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::async);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 18));
  }

  {
    Test_Parser p(u8"async function f(){}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Named_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::async);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 20));
  }
}

TEST_F(Test_Parse_Expression, generator_function_expression) {
  {
    Test_Parser p(u8"function*(){}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::generator);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 13));
  }

  {
    Test_Parser p(u8"function* f(){}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function f");
  }
}

TEST_F(Test_Parse_Expression, async_generator_function_expression) {
  {
    Test_Parser p(u8"async function*(){}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::async_generator);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 19));
  }

  {
    Test_Parser p(u8"async function* f(){}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function f");
  }
}

TEST_F(Test_Parse_Expression, arrow_function) {
  {
    Test_Parser p(u8"() => a"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
  }

  {
    Test_Parser p(u8"a => b"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 6));
  }

  {
    Test_Parser p(u8"(a) => b"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 8));
  }

  {
    Test_Parser p(u8"(a, b) => c"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_EQ(summarize(ast->child(1)), "var b");
  }

  {
    Test_Parser p(u8"() => a, b"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(arrowfunc(), var b)");
  }

  {
    Test_Parser p(u8"a => b, c"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(arrowfunc(var a), var c)");
  }

  {
    Test_Parser p(u8"(a,) => b"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc(var a)");
  }

  {
    Test_Parser p(u8"async => value"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc(var async)");
  }

  {
    Test_Parser p(u8"() => { a; }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 12));
  }

  {
    Test_Parser p(u8"a => { b; } /* */"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 11));
  }
}

TEST_F(Test_Parse_Expression, arrow_function_with_spread_and_comma) {
  {
    Test_Parser p(u8"(...b, ) => { c; }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ^ Diag_Comma_Not_Allowed_After_Spread_Parameter.comma\n"_diag
            u8" ^^^^ .spread"_diag,
        });
    EXPECT_EQ(summarize(ast), "arrowfunc(spread(var b))");
  }
}

TEST_F(Test_Parse_Expression, arrow_function_with_destructuring_parameters) {
  {
    Test_Parser p(u8"({a, b}) => c"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)),
              "object(literal: var a, literal: var b)");
  }

  {
    Test_Parser p(u8"([a, b]) => c"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "array(var a, var b)");
  }

  {
    Test_Parser p(u8"(...args) => null"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "spread(var args)");
  }

  {
    Test_Parser p(u8"({ a, b ) => c"_sv, capture_diags);
    p.parse_expression();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8" ^ Diag_Unclosed_Object_Literal.object_open\n"_diag
            u8"       ` .expected_object_close"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, async_arrow_function) {
  {
    Test_Parser p(u8"async () => { a; }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::async);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 18));
  }

  {
    Test_Parser p(u8"async x => { y; }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::async);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
  }

  {
    Test_Parser p(u8"async (x, y, z) => { w; }"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "asyncarrowfunc(var x, var y, var z)");
  }

  {
    Test_Parser p(u8"async () => a"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::async);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 13));
  }

  {
    Test_Parser p(u8"async x => y"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Arrow_Function);
    EXPECT_EQ(ast->attributes(), Function_Attributes::async);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
  }

  {
    Test_Parser p(u8"async (x, y, z) => w"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "asyncarrowfunc(var x, var y, var z)");
  }

  {
    Test_Parser p(u8"async (a,) => b"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "asyncarrowfunc(var a)");
  }
}

TEST_F(Test_Parse_Expression, invalid_arrow_function) {
  {
    Test_Parser p(u8"a() => b"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"    ^^ Diag_Unexpected_Arrow_After_Expression.arrow\n"_diag
            u8"^^^ .expression"_diag,
        });
    EXPECT_EQ(summarize(ast), "binary(call(var a), var b)");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 0 + u8"a() => b"_sv.size()));
  }

  {
    Test_Parser p(u8"a(b) => c"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(call(var a, var b), var c)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"     ^^ Diag_Unexpected_Arrow_After_Expression.arrow\n"_diag
            u8"^^^^ .expression"_diag,
        });
  }

  {
    Test_Parser p(u8"a() => {}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, arrowfunc())");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^ Diag_Missing_Operator_Between_Expression_And_Arrow_Function"_diag,
        });
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 0 + u8"a() => {}"_sv.size()));
  }

  {
    Test_Parser p(u8"a(b) => {}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, arrowfunc(var b))");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^ Diag_Missing_Operator_Between_Expression_And_Arrow_Function"_diag,
        });
  }

  {
    Test_Parser p(u8"=> a"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^ Diag_Missing_Arrow_Function_Parameter_List"_diag,
        });
  }

  {
    Test_Parser p(u8"=> { body; }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^ Diag_Missing_Arrow_Function_Parameter_List"_diag,
        });
  }

  {
    Test_Parser p(u8"=> { body; }, other"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(arrowfunc(), var other)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"^^ Diag_Missing_Arrow_Function_Parameter_List"_diag,
        });
  }

  {
    Test_Parser p(u8"42 => body"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, var body)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"   ^^ Diag_Unexpected_Arrow_After_Literal.arrow\n"_diag
            u8"^^ .literal_parameter"_diag,
        });
  }

  {
    Test_Parser p(u8"42 => {body();}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"   ^^ Diag_Unexpected_Arrow_After_Literal.arrow\n"_diag
            u8"^^ .literal_parameter"_diag,
        });
  }

  {
    Test_Parser p(u8"x.p => rhs"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(dot(var x, p), var rhs)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"    ^^ Diag_Unexpected_Arrow_After_Expression.arrow\n"_diag
            u8"^^^ .expression"_diag,
        });
  }

  {
    Test_Parser p(u8"x.p => {body();}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"    ^^ Diag_Unexpected_Arrow_After_Expression.arrow\n"_diag
            u8"^^^ .expression"_diag,
        });
  }

  {
    Test_Parser p(u8"(x, 42, y) => {body();}"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc(var x, literal, var y)");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"    ^^ Diag_Unexpected_Literal_In_Parameter_List"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, function_without_parameter_list) {
  {
    Test_Parser p(u8"function { return 42; }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"        ` Diag_Missing_Function_Parameter_List"_diag,
        });
  }

  {
    // e.g. if (x) { function }
    Test_Parser p(u8"function }"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function");
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"        ` Diag_Missing_Function_Parameter_List"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, invalid_parentheses) {
  {
    // Errors should be reported during visitation.
    Test_Parser p(u8"()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "parenempty");
  }

  {
    // Errors should be reported during visitation.
    Test_Parser p(u8"x = ()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var x, parenempty)");
  }

  {
    // Errors should be reported during visitation.
    Test_Parser p(u8"() = x"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(parenempty, var x)");
  }
}

TEST_F(Test_Parse_Expression, invalid_keyword_in_expression) {
  {
    Test_Parser p(u8"debugger"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "invalid");
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^^^^^^^^ Diag_Unexpected_Token"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression, anonymous_class) {
  {
    Test_Parser p(u8"class {}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Class);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 8));
  }

  {
    Test_Parser p(u8"class C {}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), Expression_Kind::Class);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 10));
  }
}

TEST_F(Test_Parse_Expression, class_requires_a_body) {
  {
    Test_Parser p(u8"class C "_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "class");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"class C"_sv));
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^^^^^^^ Diag_Missing_Body_For_Class"_diag,
                       });
  }
}

TEST_F(Test_Parse_Expression, parse_mixed_expression) {
  {
    Test_Parser p(u8"a+f()"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, call(var f))");
  }

  {
    Test_Parser p(u8"a+f(x+y,-z-w)+b"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(var a, call(var f, binary(var x, var y), "
              "binary(unary(var z), var w)), var b)");
  }

  {
    Test_Parser p(u8"(x+y).z"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(paren(binary(var x, var y)), z)");
  }

  {
    Test_Parser p(u8"/hello/.test(string)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(dot(literal, test), var string)");
  }

  {
    Test_Parser p(u8"!/hello/.test(string)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(call(dot(literal, test), var string))");
  }

  {
    Test_Parser p(u8"{a: new A(), b: new B()}"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: new(var A), literal: new(var B))");
  }

  {
    Test_Parser p(u8"o && typeof o === 'object' ? o[k] : null"_sv);
    Expression* ast = p.parse_expression();
    if (false) {  // TODO(strager): Check AST.
      EXPECT_EQ(summarize(ast),
                "cond(binary(var o, binary(typeof(var o), literal)), "
                "index(var o, var k), literal)");
    }
  }

  {
    Test_Parser p(u8"!!o && k in o"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(unary(unary(var o)), var k, var o)");
  }

  {
    Test_Parser p(u8"x --> 0"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(rwunarysuffix(var x), literal)");
  }

  {
    Test_Parser p(u8"class {} + 42"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(class, literal)");
  }

  {
    Test_Parser p(u8"other + async(a)"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var other, call(var async, var a))");
  }

  {
    Test_Parser p(u8"left + async() + right"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var left, call(var async), var right)");
  }

  {
    Test_Parser p(u8"left + async + right"_sv);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var left, var async, var right)");
  }
}

TEST_F(Test_Parse_Expression,
       reserved_keywords_for_object_properties_can_contain_escape_sequences) {
  for (String8_View keyword : strict_reserved_keywords) {
    String8 property = escape_first_character_in_keyword(keyword);

    {
      Test_Parser p(concat(u8"obj."_sv, property));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(ast->kind(), Expression_Kind::Dot);
      EXPECT_EQ(summarize(ast->child_0()), "var obj");
      EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
    }

    {
      Test_Parser p(concat(u8"obj?."_sv, property));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(ast->kind(), Expression_Kind::Dot);
      EXPECT_EQ(summarize(ast->child_0()), "var obj");
      EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
    }

    {
      Test_Parser p(concat(u8"{ "_sv, property, u8": value }"_sv));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: var value)");
    }

    {
      Test_Parser p(concat(u8"{ "_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{ get "_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{ set "_sv, property, u8"(v) {} }"_sv));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{ async "_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{ *"_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{ async *"_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      Test_Parser p(concat(u8"{ function *"_sv, property, u8"() {} }"_sv),
                    capture_diags);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
      assert_diagnostics(
          p.code, p.errors,
          {
              u8"Diag_Methods_Should_Not_Use_Function_Keyword"_diag,
          });
    }
  }
}

TEST_F(Test_Parse_Expression, generator_misplaced_star) {
  Test_Parser p(u8"(*function f(){})"_sv, capture_diags);
  Expression* ast = p.parse_expression();
  EXPECT_THAT(ast->child_0()->span(), p.matches_offsets(1, 16));
  EXPECT_THAT(p.errors,
              ElementsAreArray({DIAG_TYPE(
                  Diag_Generator_Function_Star_Belongs_Before_Name)}));
}

TEST_F(Test_Parse_Expression, unary_cannot_mix_with_star_star) {
  for (Char8 op : u8"~!-+"_sv) {
    Test_Parser p(op + u8"a ** b"s, capture_diags);
    SCOPED_TRACE(p.code);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(unary(var a), var b)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Missing_Parentheses_Around_Unary_Lhs_Of_Exponent,  //
                unary_expression, 0, op + u8"a"s,                       //
                exponent_operator, (op + u8"a "s).size(), u8"**"_sv),
        }));
  }

  for (String8_View op : {u8"delete"s, u8"typeof"s, u8"void"s}) {
    Test_Parser p(concat(op, u8" a ** b"s), capture_diags);
    SCOPED_TRACE(p.code);
    Expression* ast = p.parse_expression();
    if ((false)) {
      // TODO(strager): Rewrite the AST into something like the following:
      EXPECT_EQ(summarize(ast), "typeof(binary(var a, var b))");
    }
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                Diag_Missing_Parentheses_Around_Exponent_With_Unary_Lhs,  //
                exponent_expression, concat(op, u8" "s).size(), u8"a ** b"_sv,
                unary_operator, 0, op),
        }));
  }
}

TEST_F(Test_Parse_Expression, whitespace_between_bang_and_equal) {
  {
    Test_Parser p(u8"x! == y"_sv, capture_diags);
    Expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y)")
        << "'x! == y' should be parsed as 'x !== y'";
    assert_diagnostics(
        p.code, p.errors,
        {
            u8"  ^ Diag_Unexpected_Space_Between_Bang_And_Equal_Equal"_diag,
        });
  }
}

TEST_F(Test_Parse_Expression, Diag_Spread_Must_Precede_Expression) {
  Test_Parser p(u8"a = ...;"_sv, capture_diags);
  p.parse_and_visit_expression();
  assert_diagnostics(p.code, p.errors,
                     {
                         u8"Diag_Spread_Must_Precede_Expression"_diag,
                     });
}

TEST_F(Test_Parse_Expression, precedence) {
  enum class Level_Type {
    // Left-associative binary operator.
    left,
    // Right-associative binary operator.
    right,
    // Binary operator. We don't track associativity of many binary expressions,
    // but if we do, we should remove this and use 'left' or 'right' instead.
    binary,
    // Right-associative ternary operator.
    ternary_right,
    // Right-associative prefix operator.
    prefix,
  };

  static auto is_binary_level = [](Level_Type type) -> bool {
    switch (type) {
    case Level_Type::left:
    case Level_Type::right:
    case Level_Type::binary:
      return true;
    case Level_Type::prefix:
    case Level_Type::ternary_right:
      return false;
    }
    QLJS_UNREACHABLE();
  };

  struct Operator_Type {
    String8_View op;
    const char* raw_kind;

    std::string_view kind() const {
      if (this->raw_kind) {
        return std::string_view(this->raw_kind);
      } else if (this->op.empty()) {
        return "cond"sv;
      } else {
        return "binary"sv;
      }
    }
  };
  struct Precedence_Level {
    Level_Type type;
    std::vector<Operator_Type> ops;
  };

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_CLANG("-Wmissing-field-initializers")
  QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
  // In our table, lower index items have lower precedence.
  static const Precedence_Level precedence_levels[] = {
      // TODO(strager): Fix failures when testing e.g. "a,b+c".
      // {level_type::binary, {{u8","_sv}}},
      {Level_Type::right,
       {
           {u8"="_sv, "assign"},
           {u8"+="_sv, "upassign"},
           {u8"-="_sv, "upassign"},
           {u8"**="_sv, "upassign"},
           {u8"*="_sv, "upassign"},
           {u8"/="_sv, "upassign"},
           {u8"%="_sv, "upassign"},
           {u8"<<="_sv, "upassign"},
           {u8">>="_sv, "upassign"},
           {u8">>>="_sv, "upassign"},
           {u8"&="_sv, "upassign"},
           {u8"^="_sv, "upassign"},
           {u8"|="_sv, "upassign"},
           {u8"&&="_sv, "condassign"},
           {u8"||="_sv, "condassign"},
           {u8"?\x3f="_sv, "condassign"},
           // TODO(strager): yield and yield*
       }},
      {Level_Type::ternary_right, {{/* special-cased */}}},
      {Level_Type::binary, {{u8"||"_sv}, {u8"??"_sv}}},
      {Level_Type::binary, {{u8"&&"_sv}}},
      {Level_Type::binary, {{u8"|"_sv}}},
      {Level_Type::binary, {{u8"^"_sv}}},
      {Level_Type::binary, {{u8"&"_sv}}},
      {Level_Type::binary,
       {
           {u8"=="_sv},
           {u8"!="_sv},
           {u8"==="_sv},
           {u8"!=="_sv},
       }},
      {Level_Type::binary,
       {
           {u8"<"_sv},
           {u8"<="_sv},
           {u8">"_sv},
           {u8">="_sv},
           // TODO(strager): Fix failures when testing e.g. "a in b+c".
           // {u8" in "_sv},
           {u8" instanceof "_sv},
       }},
      {Level_Type::binary, {{u8"<<"_sv}, {u8">>"_sv}, {u8">>>"_sv}}},
      {Level_Type::binary, {{u8"+"_sv}, {u8"-"_sv}}},
      {Level_Type::binary, {{u8"*"_sv}, {u8"/"_sv}, {u8"%"_sv}}},
      {Level_Type::binary, {{u8"**"_sv}}},
      {Level_Type::prefix,
       {
           {u8"!"_sv, "unary"},
           {u8"+"_sv, "unary"},
           {u8"-"_sv, "unary"},
           {u8"++"_sv, "rwunary"},
           {u8"--"_sv, "rwunary"},
           {u8"typeof "_sv, "typeof"},
           {u8"void "_sv, "unary"},
           {u8"delete "_sv, "delete"},
           // TODO(strager): await
       }},
      // TODO(strager): Unary suffix operators: ++ --
      // TODO(strager): Unary prefix operator: new
      // TODO(strager): Operators: x.y, x[y], new x(y), x(y), x?.y
  };
  QLJS_WARNING_POP

  // Sanity check the table.
  for (const Precedence_Level& level : precedence_levels) {
    for (const Operator_Type& op : level.ops) {
      switch (level.type) {
      case Level_Type::left:
      case Level_Type::right:
        ASSERT_NE(op.kind(), "binary"sv);
        break;
      case Level_Type::binary:
        ASSERT_EQ(op.kind(), "binary"sv);
        break;
      case Level_Type::prefix:
        break;
      case Level_Type::ternary_right:
        ASSERT_EQ(op.op, u8""_sv);
        ASSERT_EQ(op.kind(), "cond"sv);
        break;
      }
    }
  }

  static auto check_expression =
      [](String8_View code, std::string_view expected_ast_summary) -> void {
    SCOPED_TRACE(out_string8(code));
    for (const Parser_Options& options :
         {javascript_options, typescript_options}) {
      if (options.typescript &&
          (code == u8"a<b>=c"_sv || code == u8"a<b>>=c"_sv ||
           code == u8"a<b>>>=c"_sv)) {
        // HACK(strager): For TypeScript, "a<b>=c" is parsed as "a<b> =c", not
        // as "a < b >= c". Don't test those here. See
        // NOTE[typescript-generic-expression-token-splitting].
        continue;
      }

      Test_Parser p(code, options, capture_diags);
      Expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), expected_ast_summary);
      // Ignore p.errors.
    }
  };

  static auto test = [](Level_Type lo_type, Operator_Type lo_op,
                        Level_Type hi_type, Operator_Type hi_op,
                        bool is_same_level) -> void {
    if (lo_type == Level_Type::binary && hi_type == Level_Type::binary) {
      // Associativity is not tracked.
      // a*b+c
      check_expression(concat(u8"a"_sv, hi_op.op, u8"b"_sv, lo_op.op, u8"c"_sv),
                       "binary(var a, var b, var c)"sv);
      // a+b*c
      check_expression(concat(u8"a"_sv, lo_op.op, u8"b"_sv, hi_op.op, u8"c"_sv),
                       "binary(var a, var b, var c)"sv);
    } else if (is_binary_level(lo_type) && is_binary_level(hi_type)) {
      if (!is_same_level || hi_type == Level_Type::right) {
        // a=b+c
        check_expression(
            concat(u8"a"_sv, lo_op.op, u8"b"_sv, hi_op.op, u8"c"_sv),
            concat(lo_op.kind(), "(var a, "sv, hi_op.kind(),
                   "(var b, var c))"sv));
      }
      if (!is_same_level || hi_type == Level_Type::left) {
        // a=b,c
        check_expression(
            concat(u8"a"_sv, hi_op.op, u8"b"_sv, lo_op.op, u8"c"_sv),
            concat(lo_op.kind(), "("sv, hi_op.kind(),
                   "(var a, var b), var c)"sv));
      }
    } else if (is_binary_level(lo_type) && hi_type == Level_Type::prefix) {
      // -a*b
      check_expression(
          concat(hi_op.op, u8"a"_sv, lo_op.op, u8"b"_sv),
          concat(lo_op.kind(), "("sv, hi_op.kind(), "(var a), var b)"sv));
    } else if (is_binary_level(lo_type) &&
               hi_type == Level_Type::ternary_right) {
      ASSERT_EQ(hi_op.kind(), "cond"sv);
      // a+b?c:d
      check_expression(
          concat(u8"a"_sv, lo_op.op, u8"b?c:d"_sv),
          concat(lo_op.kind(), "(var a, cond(var b, var c, var d))"s));
      // a?b+c:d
      check_expression(
          concat(u8"a?b"_sv, lo_op.op, u8"c:d"_sv),
          concat("cond(var a, "sv, lo_op.kind(), "(var b, var c), var d)"sv));
      // a?b:c+d
      check_expression(
          concat(u8"a?b:c"_sv, lo_op.op, u8"d"_sv),
          concat("cond(var a, var b, "sv, lo_op.kind(), "(var c, var d))"sv));
    } else if (is_binary_level(hi_type) &&
               lo_type == Level_Type::ternary_right) {
      ASSERT_EQ(lo_op.kind(), "cond"sv);
      // a=b?c:d
      check_expression(
          concat(u8"a"_sv, hi_op.op, u8"b?c:d"_sv),
          concat("cond("sv, hi_op.kind(), "(var a, var b), var c, var d)"sv));
      // a?b=c:d
      check_expression(
          concat(u8"a?b"_sv, hi_op.op, u8"c:d"_sv),
          concat("cond(var a, "sv, hi_op.kind(), "(var b, var c), var d)"sv));
      // a?b:c=d
      check_expression(
          concat(u8"a?b:c"_sv, hi_op.op, u8"d"_sv),
          concat("cond(var a, var b, "sv, hi_op.kind(), "(var c, var d))"sv));
    } else if (hi_type == Level_Type::prefix &&
               lo_type == Level_Type::ternary_right) {
      ASSERT_EQ(lo_op.kind(), "cond"sv);
      // -a?b:c
      check_expression(
          concat(hi_op.op, u8"a?b:c"s),
          concat("cond("sv, hi_op.kind(), "(var a), var b, var c)"sv));
    } else {
      QLJS_UNREACHABLE();
    }
  };

  for (std::size_t hi_index = 0; hi_index < std::size(precedence_levels);
       ++hi_index) {
    const Precedence_Level& hi = precedence_levels[hi_index];
    for (const Operator_Type& hi_op : hi.ops) {
      for (std::size_t lo_index = 0; lo_index < hi_index; ++lo_index) {
        bool is_same_level = hi_index == lo_index;
        const Precedence_Level& lo = precedence_levels[lo_index];
        for (const Operator_Type& lo_op : lo.ops) {
          test(lo.type, lo_op, hi.type, hi_op, is_same_level);
        }
      }
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
