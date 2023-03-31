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

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using ::testing::VariantWith;
using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
TEST_F(test_parse_expression, parse_single_token_expression) {
  {
    test_parser p(u8"x"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::variable);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"x"_sv);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 1));
  }

  {
    test_parser p(u8"42"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 2));
  }

  {
    test_parser p(u8"'hello'"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
  }

  {
    test_parser p(u8"null"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 4));
  }

  {
    test_parser p(u8"true"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 4));
  }

  {
    test_parser p(u8"false"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
  }

  {
    test_parser p(u8"this"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::this_variable);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 4));
  }
}

TEST_F(test_parse_expression, keyword_variable_reference) {
  {
    test_parser p(u8"async"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::variable);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"async"_sv);
  }

  {
    test_parser p(u8"async()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(ast->child_0()->kind(), expression_kind::variable);
    EXPECT_EQ(ast->child_0()->variable_identifier().normalized_name(),
              u8"async"_sv);
  }

  {
    test_parser p(u8"async(a, b).c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(call(var async, var a, var b), c)");
  }
}

TEST_F(test_parse_expression, private_identifiers_are_not_valid_expressions) {
  {
    test_parser p(u8"#myPrivateField"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::private_variable);
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_cannot_refer_to_private_variable_without_object,  //
                private_identifier, 0, u8"#myPrivateField"_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"#myPrivateField"_sv));
  }

  {
    test_parser p(u8"#myPrivateField = 10"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::assignment);
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_cannot_refer_to_private_variable_without_object,  //
                private_identifier, 0, u8"#myPrivateField"_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"#myPrivateField = 10"_sv));
  }
}

TEST_F(test_parse_expression, parse_regular_expression) {
  {
    test_parser p(u8"/regexp/"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 8));
  }

  {
    test_parser p(u8"/=regexp/"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 9));
  }
}

TEST_F(test_parse_expression, parse_math_expression) {
  {
    test_parser p(u8"-x"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::unary_operator);
    EXPECT_EQ(ast->child_0()->kind(), expression_kind::variable);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 2));
  }

  {
    test_parser p(u8"+x"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }

  {
    test_parser p(u8"~x"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }

  {
    test_parser p(u8"x+y"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y)");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
  }

  {
    test_parser p(u8"x+y-z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y, var z)");
  }

  {
    test_parser p(u8"2-4+1"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal, literal)");
  }

  {
    test_parser p(u8"-x+y"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(unary(var x), var y)");
  }

  for (string8_view input : {
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
    SCOPED_TRACE(out_string8(u8"input = " + string8(input)));
    test_parser p(input);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
  }
}

TEST_F(test_parse_expression, parse_broken_math_expression) {
  {
    test_parser p(u8"2+"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"2"), u8"+"_sv),
        }));
  }

  {
    test_parser p(u8"^2"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(missing, literal)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"^"_sv),
        }));
  }

  // NOTE(strager): "/=" is not tested here because "/=/" is a regular
  // expression literal.
  for (string8_view op :
       {u8"*="_sv, u8"%="_sv, u8"+="_sv, u8"-="_sv, u8"<<="_sv, u8">>="_sv,
        u8">>>="_sv, u8"&="_sv, u8"^="_sv, u8"|="_sv, u8"**="_sv}) {
    test_parser p(concat(op, u8" 2"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "upassign(missing, literal)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, op),
        }));
  }

  {
    test_parser p(u8"2 * * 2"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, missing, literal)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"2 "), u8"*"_sv),
        }));
  }

  {
    test_parser p(u8"2 & & & 2"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, missing, missing, literal)");

    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"2 "), u8"&"_sv),
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"2 & "), u8"&"_sv),
        }));
  }

  {
    test_parser p(u8"(2*)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(binary(literal, missing))");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 2, u8"*"_sv),
        }));
  }

  {
    test_parser p(u8"2 * (3 + 4"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(literal, paren(binary(literal, literal)))");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"2 * "), u8"("_sv),
                }));
  }

  {
    test_parser p(u8"2 * (3 + (4"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(literal, paren(binary(literal, paren(literal))))");

    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"2 * (3 + "), u8"("_sv),
                    DIAG_TYPE_OFFSETS(p.code, diag_unmatched_parenthesis,  //
                                      where, strlen(u8"2 * "), u8"("_sv),
                }));
  }
}

TEST_F(test_parse_expression, comma_expression_with_trailing_comma) {
  {
    // Arrow expressions allow trailing commas in their parenthesized parameter
    // lists, but comma expressions do not allow trailing commas.
    test_parser p(u8"(a, b, c,)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(trailingcomma(var a, var b, var c))");
    EXPECT_THAT(p.errors, IsEmpty())
        << "trailing comma expression emits no errors; errors are emitted "
           "depending on the context";
  }
}

TEST_F(test_parse_expression, parse_logical_expression) {
  for (string8_view input :
       {u8"2==2"_sv, u8"2===2"_sv, u8"2!=2"_sv, u8"2!==2"_sv, u8"2>2"_sv,
        u8"2<2"_sv, u8"2>=2"_sv, u8"2<=2"_sv, u8"2&&2"_sv, u8"2??2"_sv,
        u8"2||2"_sv}) {
    SCOPED_TRACE(out_string8(u8"input = " + string8(input)));
    test_parser p(input, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
  }

  {
    test_parser p(u8"!x"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }
}

TEST_F(test_parse_expression, parse_keyword_binary_operators) {
  {
    test_parser p(u8"prop in object"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var prop, var object)");
  }

  {
    test_parser p(u8"object instanceof Class"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var object, var Class)");
  }
}

TEST_F(test_parse_expression, parse_typeof_unary_operator) {
  {
    test_parser p(u8"typeof o"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "typeof(var o)");
  }

  {
    test_parser p(u8"typeof o === 'number'"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(typeof(var o), literal)");
  }

  {
    test_parser p(u8"typeof o.p"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "typeof(dot(var o, p))");
  }
}

TEST_F(test_parse_expression, parse_typeof_conditional_operator) {
  {
    test_parser p(u8"typeof o ? 10 : 20"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(typeof(var o), literal, literal)");
  }
}

TEST_F(test_parse_expression, delete_unary_operator) {
  {
    test_parser p(u8"delete variable"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "delete(var variable)");
  }

  {
    test_parser p(u8"delete variable.property"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "delete(dot(var variable, property))");
  }
}

TEST_F(test_parse_expression, void_unary_operator) {
  {
    test_parser p(u8"void 0"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(literal)");
  }
}

TEST_F(test_parse_expression, spread) {
  {
    test_parser p(u8"...args"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "spread(var args)");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_function_call) {
  {
    test_parser p(u8"f()"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
    expression::call* call = expression_cast<expression::call>(ast);
    EXPECT_THAT(call->left_paren_span(), p.matches_offsets(1, 2));
  }

  {
    test_parser p(u8"f(x)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(1)), "var x");
  }

  {
    test_parser p(u8"f(x,y)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 3);
    EXPECT_EQ(summarize(ast->child(1)), "var x");
    EXPECT_EQ(summarize(ast->child(2)), "var y");
  }
}

TEST_F(test_parse_expression, function_call_with_invalid_extra_commas) {
  {
    test_parser p(u8"f(,)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_extra_comma_not_allowed_between_arguments,  //
                comma, strlen(u8"f("), u8","_sv),
        }));
  }

  {
    test_parser p(u8"f(a,,b)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f, var a, var b)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_extra_comma_not_allowed_between_arguments,  //
                comma, strlen(u8"f(a,"), u8","_sv),
        }));
  }

  {
    // A function named 'async' in a special case because of lookahead:
    // 'async()' is a function call, but 'async()=>{}' is an arrow function.
    test_parser p(u8"async(a,,b)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var async, var a, var b)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_extra_comma_not_allowed_between_arguments,  //
                comma, strlen(u8"async(a,"), u8","_sv),
        }));
  }
}

TEST_F(test_parse_expression, parse_optional_function_call) {
  {
    test_parser p(u8"f?.(x,y)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f, var x, var y)");
  }
}

TEST_F(test_parse_expression, parse_dot_expressions) {
  {
    test_parser p(u8"x.prop"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::dot);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"prop"_sv);
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 6));
  }

  {
    test_parser p(u8"x.p1.p2"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(dot(var x, p1), p2)");
  }

  for (string8_view keyword : keywords) {
    SCOPED_TRACE(out_string8(keyword));
    test_parser p(concat(u8"promise."_sv, keyword));
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var promise, " + to_string(keyword) + ")");
  }

  {
    test_parser p(u8"x.#private"_sv);
    auto class_guard = p.enter_class();  // Allow to call private identifiers.
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, #private)");
  }
}

TEST_F(test_parse_expression, invalid_dot_expression) {
  {
    test_parser p(u8"x.''"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, literal)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_invalid_rhs_for_dot_operator,  //
                              dot, strlen(u8"x"), u8"."_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x.''"_sv));
  }

  {
    test_parser p(u8"x. "_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, )");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_property_name_for_dot_operator,  //
                        dot, strlen(u8"x"), u8"."_sv),
                }));
  }

  {
    test_parser p(u8"(x.)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(dot(var x, ))");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_property_name_for_dot_operator,  //
                        dot, strlen(u8"(x"), u8"."_sv),
                }));
  }

  for (string8_view op : {
           u8"!="_sv,   u8"!=="_sv, u8"%"_sv,      u8"%="_sv,  u8"&"_sv,
           u8"&&"_sv,   u8"&&="_sv, u8"&="_sv,     u8"*"_sv,   u8"**"_sv,
           u8"**="_sv,  u8"*="_sv,  u8"+"_sv,      u8"+="_sv,  u8","_sv,
           u8"-"_sv,    u8"-="_sv,  u8"/="_sv,     u8"<"_sv,   u8"<<"_sv,
           u8"<<="_sv,  u8"<="_sv,  u8"="_sv,      u8"=="_sv,  u8"==="_sv,
           u8">"_sv,    u8">="_sv,  u8">>"_sv,     u8">>="_sv, u8">>>"_sv,
           u8">>>="_sv, u8"??"_sv,  u8"?\x3f="_sv, u8"^"_sv,   u8"^="_sv,
           u8"|"_sv,    u8"|="_sv,  u8"||"_sv,     u8"||="_sv,
       }) {
    test_parser p(concat(u8"x. "_sv, op, u8" y"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast),
                ::testing::AnyOf("assign(dot(var x, ), var y)",      //
                                 "binary(dot(var x, ), var y)",      //
                                 "condassign(dot(var x, ), var y)",  //
                                 "upassign(dot(var x, ), var y)"));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_property_name_for_dot_operator,  //
                        dot, strlen(u8"x"), u8"."_sv),
                }));
  }

  {
    test_parser p(u8"x. ? y. : z"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(dot(var x, ), dot(var y, ), var z)");
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_property_name_for_dot_operator,  //
                              dot, strlen(u8"x"), u8"."_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_property_name_for_dot_operator,  //
                              dot, strlen(u8"x. ? y"), u8"."_sv)));
  }

  {
    test_parser p(u8"x.;"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, )");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_property_name_for_dot_operator,  //
                        dot, strlen(u8"x"), u8"."_sv),
                }));
  }

  {
    test_parser p(u8".;"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(missing, )");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"."_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_property_name_for_dot_operator,  //
                              dot, 0, u8"."_sv),
        }));
  }

  {
    test_parser p(u8"console.('hello');"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(dot(var console, ), literal)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_property_name_for_dot_operator,  //
                        dot, strlen(u8"console"), u8"."_sv),
                }));
  }

  {
    test_parser p(u8"'hello' .. 'world'"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_dot_dot_is_not_an_operator,  //
                              dots, strlen(u8"'hello' "), u8".."_sv),
        }));
  }
}

TEST_F(test_parse_expression, parse_optional_dot_expressions) {
  {
    test_parser p(u8"x?.prop"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, prop)");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"x?.prop"_sv));
  }

  for (string8 keyword : keywords) {
    padded_string code(u8"obj?." + keyword);
    SCOPED_TRACE(code);
    test_parser p(code.string_view());
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::dot);
    EXPECT_EQ(summarize(ast->child_0()), "var obj");
    EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
  }

  {
    test_parser p(u8"x?.#private"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, #private)");
  }
}

TEST_F(test_parse_expression, parse_indexing_expression) {
  {
    test_parser p(u8"xs[i]"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::index);
    EXPECT_EQ(summarize(ast->child_0()), "var xs");
    EXPECT_EQ(summarize(ast->child_1()), "var i");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
  }
}

TEST_F(test_parse_expression, parse_optional_indexing_expression) {
  {
    test_parser p(u8"xs?.[i]"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, var i)");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"xs?.[i]"_sv));
  }
}

TEST_F(test_parse_expression, parse_unclosed_indexing_expression) {
  {
    test_parser p(u8"xs[i"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, var i)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_unmatched_indexing_bracket,  //
                              left_square, strlen(u8"xs"), u8"["_sv),
        }));
  }

  {
    test_parser p(u8"(xs[i)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(index(var xs, var i))");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_unmatched_indexing_bracket,  //
                              left_square, strlen(u8"(xs"), u8"["_sv),
        }));
  }
}

TEST_F(test_parse_expression, empty_indexing_expression) {
  {
    test_parser p(u8"xs[]"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_indexing_requires_expression,  //
                              squares, strlen(u8"xs"), u8"[]"_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"xs[]"_sv));
  }
}

TEST_F(test_parse_expression, parse_parenthesized_expression) {
  {
    test_parser p(u8"(x)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(var x)");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"(x)"_sv));
    EXPECT_THAT(ast->child_0()->span(), p.matches_offsets(1, 2));
  }

  {
    test_parser p(u8"x+(y)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, paren(var y))");
  }

  {
    test_parser p(u8"x+(y+z)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, paren(binary(var y, var z)))");
  }

  {
    test_parser p(u8"(x+y)+z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(paren(binary(var x, var y)), var z)");
  }

  {
    test_parser p(u8"x+(y+z)+w"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(var x, paren(binary(var y, var z)), var w)");
  }
}

TEST_F(test_parse_expression, await_unary_operator_inside_async_functions) {
  {
    test_parser p(u8"await myPromise"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::async);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(var myPromise)");
    EXPECT_EQ(ast->kind(), expression_kind::await);
    EXPECT_EQ(summarize(ast->child_0()), "var myPromise");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 15));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"await(x)"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::async);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(paren(var x))");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, redundant_await) {
  {
    test_parser p(u8"await await p"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(await(var p))");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_redundant_await,
                                      await_operator, 0, u8"await"_sv),
                }));
  }

  {
    test_parser p(u8"await await await p"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(await(await(var p)))");
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code, diag_redundant_await, await_operator, 0,
                              u8"await"_sv),
            DIAG_TYPE_OFFSETS(p.code, diag_redundant_await, await_operator,
                              strlen(u8"await "), u8"await"_sv)));
  }
}

TEST_F(test_parse_expression, await_followed_by_arrow_function) {
  auto test = [](auto&& make_guard) -> void {
    {
      test_parser p(u8"await x => {}"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "asyncarrowfunc(var x)");
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_await_followed_by_arrow_function,  //
                          await_operator, 0, u8"await"_sv),
                  }));
    }

    {
      test_parser p(u8"await () => {}"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "asyncarrowfunc()");
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_await_followed_by_arrow_function,  //
                          await_operator, 0, u8"await"_sv),
                  }));
    }

    {
      test_parser p(u8"await (param) => {}"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "asyncarrowfunc(var param)");
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_await_followed_by_arrow_function,  //
                          await_operator, 0, u8"await"_sv),
                  }));
    }

    {
      test_parser p(u8"await (param) => { await param; }"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "asyncarrowfunc(var param)");
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_await_followed_by_arrow_function,  //
                          await_operator, 0, u8"await"_sv),
                  }));
    }

    {
      test_parser p(u8"await (param) => body"_sv, capture_diags);
      [[maybe_unused]] auto guard = make_guard(p);
      expression* ast = p.parse_expression();
      EXPECT_THAT(p.visits, ElementsAreArray({
                                "visit_enter_function_scope",       //
                                "visit_variable_declaration",       // param
                                "visit_enter_function_scope_body",  //
                                "visit_variable_use",               // body
                                "visit_exit_function_scope",
                            }));
      EXPECT_EQ(summarize(ast), "asyncarrowfunc(var param)");
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE_OFFSETS(
                          p.code, diag_await_followed_by_arrow_function,  //
                          await_operator, 0, u8"await"_sv),
                  }));
    }

    // TODO(strager): Test an arrow function with TypeScript a return type
    // annotation.
  };

  {
    SCOPED_TRACE("in async function");
    test([](test_parser& p) {
      return p.enter_function(function_attributes::async);
    });
  }

  {
    SCOPED_TRACE("in non-async function");
    test([](test_parser& p) {
      return p.enter_function(function_attributes::normal);
    });
  }

  {
    SCOPED_TRACE("top-level");
    test([](test_parser&) -> int {
      return 0;  // No guard.
    });
  }

  {
    test_parser p(u8"await?.() => c"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(call(var await), var c)")
        << "'await' should not be treated as if it was 'async' in an arrow "
           "function.";
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_unexpected_arrow_after_expression,  //
                        arrow, strlen(u8"await?.() "), u8"=>"_sv, expression, 0,
                        u8"await?.()"_sv),
                }));
  }
}

TEST_F(test_parse_expression,
       await_in_normal_function_vs_async_function_vs_top_level) {
  static parser_options default_parser_options;

  parser_options no_jsx;
  no_jsx.jsx = false;

  parser_options jsx;
  jsx.jsx = true;

  struct test_case {
    string8_view code;
    const char* expected_normal_function;
    const char* expected_async_function;

    const parser_options& options = default_parser_options;
  };

  for (const test_case& test : {
         // clang-format off
         test_case

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
      test_parser p(test.code, test.options, capture_diags);
      auto guard = p.enter_function(function_attributes::normal);
      expression* ast = p.parse_expression();

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
                  {DIAG_TYPE_OFFSETS(p.code, diag_await_operator_outside_async,
                                     await_operator, 0, u8"await"_sv),
                   DIAG_TYPE_OFFSETS(p.code, diag_await_operator_outside_async,
                                     await_operator, strlen(u8"await "),
                                     u8"await"_sv)}));
        } else {
          std::size_t await_offset = test.code.find(u8"await"_sv);
          EXPECT_THAT(p.errors,
                      ElementsAreArray({
                          DIAG_TYPE_OFFSETS(
                              p.code, diag_await_operator_outside_async,  //
                              await_operator, await_offset, u8"await"_sv),
                      }));
        }
      }
    }

    if (test.expected_async_function) {
      // Async function:
      test_parser p(test.code, test.options, capture_diags);
      auto guard = p.enter_function(function_attributes::async);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), test.expected_async_function);
      if (test.code == u8"await await x"_sv) {
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_OFFSETS(p.code, diag_redundant_await,
                                          await_operator, 0, u8"await"_sv),
                    }));
      } else {
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    {
      // Top level:
      test_parser p(test.code, test.options, capture_diags);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), test.expected_async_function
                                    ? test.expected_async_function
                                    : test.expected_normal_function);
      if (test.code == u8"await await x"_sv) {
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_OFFSETS(p.code, diag_redundant_await,
                                          await_operator, 0, u8"await"_sv),
                    }));
      } else {
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }
  }
}

TEST_F(test_parse_expression, await_variable_name_outside_async_functions) {
  {
    test_parser p(u8"await(x)"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::normal);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var await, var x)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"await < rhs"_sv, jsx_options, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var await, var rhs)");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, await_unary_operator_outside_async_functions) {
  {
    test_parser p(u8"await myPromise"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::normal);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(var myPromise)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_await_operator_outside_async,  //
                              await_operator, 0, u8"await"_sv),
        }));
  }
}

TEST_F(test_parse_expression,
       yield_nullary_operator_inside_generator_functions) {
  auto parse_expression_in_generator = [](string8_view code) -> std::string {
    test_parser p(code);
    auto guard = p.enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    return summarize(ast);
  };

  {
    test_parser p(u8"yield"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yieldnone");
    EXPECT_EQ(ast->kind(), expression_kind::yield_none);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
    EXPECT_THAT(p.errors, IsEmpty());
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

TEST_F(test_parse_expression, yield_unary_operator_inside_generator_functions) {
  {
    test_parser p(u8"yield v"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yield(var v)");
    EXPECT_EQ(ast->kind(), expression_kind::yield_one);
    EXPECT_EQ(summarize(ast->child_0()), "var v");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"yield(x)"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yield(paren(var x))");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"f(yield a, yield b, c)}"_sv, capture_diags);
    auto generator_guard = p.enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f, yield(var a), yield(var b), var c)");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression,
       yield_many_unary_operator_inside_generator_functions) {
  {
    test_parser p(u8"yield *other"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yieldmany(var other)");
    EXPECT_EQ(ast->kind(), expression_kind::yield_many);
    EXPECT_EQ(summarize(ast->child_0()), "var other");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 12));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"f(yield *a, yield* b, c)}"_sv, capture_diags);
    auto generator_guard = p.enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "call(var f, yieldmany(var a), yieldmany(var b), var c)");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, yield_variable_name_outside_generator_functions) {
  {
    test_parser p(u8"yield(x)"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::normal);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var yield, var x)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"yield*other"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::normal);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var yield, var other)");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_new_expression) {
  {
    test_parser p(u8"new Date"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_new);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var Date");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 8));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"new Date()"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_new);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var Date");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 10));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"new Date(y,m,d)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Date, var y, var m, var d)");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, new_target) {
  {
    test_parser p(u8"new.target"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "newtarget");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 10));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"new.target()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(newtarget)");
  }
}

TEST_F(test_parse_expression, super) {
  {
    test_parser p(u8"super()"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(super)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"super.method()"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(dot(super, method))");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, import) {
  {
    test_parser p(u8"import(url)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(import, var url)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"import.meta"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(import, meta)");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_assignment) {
  {
    test_parser p(u8"x=y"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
  }

  {
    test_parser p(u8"x.p=z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::assignment);
    EXPECT_EQ(summarize(ast->child_0()), "dot(var x, p)");
    EXPECT_EQ(summarize(ast->child_1()), "var z");
  }

  {
    test_parser p(u8"f().p=x"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(dot(call(var f), p), var x)");
  }

  {
    test_parser p(u8"x=y=z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var x, assign(var y, var z))");
  }

  {
    test_parser p(u8"x,y=z,w"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, assign(var y, var z), var w)");
  }

  {
    test_parser p(u8"[x,y]=[z,w]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "assign(array(var x, var y), array(var z, var w))");
  }
}

TEST_F(test_parse_expression, parse_compound_assignment) {
  for (string8_view op :
       {u8"*="_sv, u8"/="_sv, u8"%="_sv, u8"+="_sv, u8"-="_sv, u8"<<="_sv,
        u8">>="_sv, u8">>>="_sv, u8"&="_sv, u8"^="_sv, u8"|="_sv, u8"**="_sv}) {
    SCOPED_TRACE(out_string8(op));
    test_parser p(concat(u8"x "_sv, op, u8" y"_sv), capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::compound_assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(
            0, narrow_cast<cli_source_position::offset_type>(p.code.size())));
  }
}

TEST_F(test_parse_expression, parse_conditional_assignment) {
  for (string8_view op : {u8"&&="_sv, u8"?\x3f="_sv, u8"||="_sv}) {
    SCOPED_TRACE(out_string8(op));
    test_parser p(concat(u8"x "_sv, op, u8" y"_sv), capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::conditional_assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_THAT(
        ast->span(),
        p.matches_offsets(
            0, narrow_cast<cli_source_position::offset_type>(p.code.size())));
  }
}

TEST_F(test_parse_expression, parse_invalid_assignment) {
  {
    test_parser p(u8"x+y=z"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(binary(var x, var y), var z)");

    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_invalid_expression_left_of_assignment,  //
                        where, 0, u8"x+y"_sv),
                }));
  }

  for (string8_view code : {
           u8"f()=x"_sv,
           u8"-x=y"_sv,
           u8"42=y"_sv,
           u8"(x=y)=z"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, capture_diags);
    p.parse_expression();

    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_invalid_expression_left_of_assignment),
                }));
  }

  for (string8_view code : {
           u8"f()! = x"_sv,
       }) {
    SCOPED_TRACE(out_string8(code));
    test_parser p(code, typescript_options, capture_diags);
    p.parse_expression();

    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_invalid_expression_left_of_assignment),
                }));
  }
}

TEST_F(test_parse_expression, parse_prefix_plusplus_minusminus) {
  {
    test_parser p(u8"++x"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_prefix);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"--y"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_prefix);
    EXPECT_EQ(summarize(ast->child_0()), "var y");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_prefix_plusplus_plus_operand) {
  {
    test_parser p(u8"++x\n+\ny"_sv, capture_diags);

    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(rwunary(var x), var y)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"--x\n+\ny"_sv, capture_diags);

    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(rwunary(var x), var y)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"++x.y"_sv, capture_diags);

    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(dot(var x, y))");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"++x[y]"_sv, capture_diags);

    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(index(var x, var y))");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_unary_prefix_operator_with_no_operand) {
  {
    test_parser p(u8"--"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"--"_sv),
        }));
  }

  {
    test_parser p(u8"++;"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"++"_sv),
        }));
  }

  {
    test_parser p(u8"(-)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(unary(missing))");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, strlen(u8"("), u8"-"_sv),
        }));
  }

  {
    test_parser p(u8"!;"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"!"_sv),
        }));
  }

  {
    test_parser p(u8"await}"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::async);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(missing)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_operand_for_operator,  //
                              where, 0, u8"await"_sv),
        }));
  }
}

TEST_F(test_parse_expression, parse_suffix_plusplus_minusminus) {
  {
    test_parser p(u8"x++"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_suffix);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 3));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, suffix_plusplus_minusminus_disallows_line_break) {
  {
    test_parser p(u8"x\n++\ny"_sv, capture_diags);

    expression* ast_1 = p.parse_expression();
    EXPECT_EQ(summarize(ast_1), "var x");

    expression* ast_2 = p.parse_expression();
    EXPECT_EQ(summarize(ast_2), "rwunary(var y)");

    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, prefix_plusplus_minusminus_cannot_nest) {
  {
    test_parser p(u8"++ ++ x"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(rwunary(var x))");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
    // TODO(strager): Report an error. ++ takes a LeftHandExpression, but ++x is
    // not a LeftHandExpression.
  }
}

TEST_F(test_parse_expression, parse_template) {
  {
    test_parser p(u8"`hello`"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"`hello${world}`"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_template);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var world");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 15));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"`${one}${two}${three}`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var one, var two, var three)");
  }

  {
    test_parser p(u8"`hello${world}` + rhs"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(template(var world), var rhs)");
  }
}

TEST_F(test_parse_expression, tagged_template_literal) {
  {
    test_parser p(u8"hello`world`"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::tagged_template_literal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var hello");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 12));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"hello`template ${literal} thingy`"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::tagged_template_literal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var hello");
    EXPECT_EQ(summarize(ast->child(1)), "var literal");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 33));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"a.b()`c`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(call(dot(var a, b)))");
  }

  {
    test_parser p(u8"tag`template`.property"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(taggedtemplate(var tag), property)");
  }

  {
    test_parser p(u8"x + tag`template`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, taggedtemplate(var tag))");
  }
}

TEST_F(test_parse_expression, optional_tagged_template_literal) {
  {
    test_parser p(u8"hello?.`world`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var hello)");
  }

  {
    test_parser p(u8"hello?.`template ${literal} thingy`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var hello, var literal)");
  }
}

TEST_F(test_parse_expression, untagged_template_with_invalid_escape) {
  {
    test_parser p(u8R"(`invalid\uescape`)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "literal");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_expected_hex_digits_in_unicode_escape),
                }));
  }

  {
    test_parser p(u8R"(`invalid\u${expr}escape`)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var expr)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_expected_hex_digits_in_unicode_escape),
                }));
  }

  {
    test_parser p(u8R"(`invalid${expr}\uescape`)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var expr)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_expected_hex_digits_in_unicode_escape),
                }));
  }

  {
    test_parser p(u8R"(`invalid${expr1}\u${expr2}escape`)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var expr1, var expr2)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE(diag_expected_hex_digits_in_unicode_escape),
                }));
  }
}

TEST_F(test_parse_expression,
       tagged_template_with_invalid_escape_reports_no_error) {
  {
    test_parser p(u8R"(tag`invalid\uescape`)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var tag)");
  }

  {
    test_parser p(u8R"(tag`invalid\uescape${expr}`)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var tag, var expr)");
  }

  {
    test_parser p(u8R"(tag?.`invalid\uescape`)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var tag)");
  }

  {
    test_parser p(u8R"(tag?.`invalid\uescape${expr}`)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "taggedtemplate(var tag, var expr)");
  }
}

TEST_F(test_parse_expression, array_literal) {
  {
    test_parser p(u8"[]"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::array);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 2));
  }

  {
    test_parser p(u8"[x]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::array);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
  }

  {
    test_parser p(u8"[x, y]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::array);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_EQ(summarize(ast->child(1)), "var y");
  }

  {
    test_parser p(u8"[,,x,,y,,]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "array(var x, var y)");
  }

  {
    // Comma should be parsed as an array separator, not as a comma operator.
    test_parser p(u8"[await myPromise,]"_sv, capture_diags);
    auto guard = p.enter_function(function_attributes::async);
    expression* ast = p.parse_expression();
    EXPECT_THAT(p.errors, IsEmpty());
    EXPECT_EQ(summarize(ast), "array(await(var myPromise))");
  }
}

TEST_F(test_parse_expression, malformed_array_literal) {
  {
    test_parser p(u8"[ "_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code, diag_missing_array_close,  //
                                left_square, 0, u8"["_sv, expected_right_square,
                                strlen(u8"["), u8""_sv),
        }));
    EXPECT_EQ(summarize(ast), "array()");
  }

  {
    test_parser p(u8"[ x "_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code, diag_missing_array_close,  //
                                left_square, 0, u8"["_sv, expected_right_square,
                                strlen(u8"[ x"), u8""_sv),
        }));
    EXPECT_EQ(summarize(ast), "array(var x)");
  }

  {
    test_parser p(u8"[\nif (true) {}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code, diag_missing_array_close,  //
                                left_square, 0, u8"["_sv, expected_right_square,
                                strlen(u8"["), u8""_sv),
        }));
    EXPECT_EQ(summarize(ast), "array()");
  }
}

TEST_F(test_parse_expression, object_literal) {
  {
    test_parser p(u8"{}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 2));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"{key: value}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
    EXPECT_FALSE(ast->object_entry(0).init);
  }

  {
    test_parser p(u8"{key1: value1, key2: value2}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 2);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value1");
    EXPECT_EQ(summarize(ast->object_entry(1).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(1).value), "var value2");
  }

  {
    test_parser p(u8"{'key': value}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
  }

  {
    test_parser p(u8"{[key]: value}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var key");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
  }

  {
    test_parser p(u8"{thing}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    auto entry = ast->object_entry(0);
    EXPECT_EQ(summarize(entry.property), "literal");
    EXPECT_THAT(entry.property->span(), p.matches_offsets(1, 6));
    EXPECT_EQ(summarize(entry.value), "var thing");
    EXPECT_THAT(entry.value->span(), p.matches_offsets(1, 6));
    EXPECT_FALSE(entry.init);
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"{key1: value1, thing2, key3: value3}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: var value1, literal: var thing2, literal: var "
              "value3)");
  }

  {
    test_parser p(u8"{key: variable = value}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    const object_property_value_pair& entry = ast->object_entry(0);
    EXPECT_EQ(summarize(entry.property), "literal");
    EXPECT_EQ(summarize(entry.value), "var variable");
    EXPECT_EQ(summarize(entry.init), "var value");
    EXPECT_THAT(entry.init_equals_span(),
                p.matches_offsets(strlen(u8"{key: variable "), u8"="_sv));
  }

  {
    test_parser p(u8"{key = value}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    const object_property_value_pair& entry = ast->object_entry(0);
    EXPECT_EQ(summarize(entry.property), "literal");
    EXPECT_EQ(summarize(entry.value), "var key");
    EXPECT_EQ(summarize(entry.init), "var value");
    EXPECT_THAT(entry.init_equals_span(),
                p.matches_offsets(strlen(u8"{key "), u8"="_sv));
  }

  {
    test_parser p(u8"{...other, k: v}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 2);
    EXPECT_FALSE(ast->object_entry(0).property);
    EXPECT_EQ(summarize(ast->object_entry(0).value), "spread(var other)");
    EXPECT_EQ(summarize(ast->object_entry(1).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(1).value), "var v");
    EXPECT_EQ(summarize(ast), "object(spread(var other), literal: var v)");
  }

  {
    test_parser p(u8"{...other = init}"_sv);
    EXPECT_EQ(summarize(p.parse_expression()),
              "object(spread(assign(var other, var init)))");
  }
}

TEST_F(test_parse_expression, object_literal_with_method_key) {
  {
    test_parser p(u8"{ func(a, b) { } }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 16));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"{ 'func'(a, b) { } }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 18));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"{ [func](a, b) { } }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 18));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"{ async func(a, b) { } }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 22));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"{ async 'func'(a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    test_parser p(u8"{ async [func](a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    test_parser p(u8"{ *func(a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    test_parser p(u8"{ *'func'(a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    test_parser p(u8"{ *[func](a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    test_parser p(u8"{ async *func(a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    test_parser p(u8"{ async *'func'(a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    test_parser p(u8"{ async *[func](a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }
}

TEST_F(test_parse_expression, object_literal_with_getter_setter_key) {
  {
    test_parser p(u8"{ get prop() { } }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 16));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"{ set prop(v) { } }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_THAT(ast->object_entry(0).value->span(), p.matches_offsets(2, 17));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"{get 1234() { }}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
  }

  {
    test_parser p(u8"{get 'string key'() { }}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
  }

  {
    test_parser p(u8"{get [expression + key]() { }}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(binary(var expression, var key): function)");
  }
}

TEST_F(test_parse_expression, object_literal_with_keyword_key) {
  for (string8_view keyword :
       {u8"async"_sv, u8"catch"_sv, u8"class"_sv, u8"default"_sv,
        u8"function"_sv, u8"get"_sv, u8"set"_sv, u8"try"_sv}) {
    SCOPED_TRACE(out_string8(keyword));

    {
      test_parser p(concat(u8"{"_sv, keyword, u8": null}"_sv));
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: literal)");
    }

    {
      test_parser p(concat(u8"{"_sv, keyword, u8"() { }}"_sv));
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{get "_sv, keyword, u8"() {}}"_sv));
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{set "_sv, keyword, u8"() {}}"_sv));
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{async "_sv, keyword, u8"() {}}"_sv));
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{*"_sv, keyword, u8"() {}}"_sv));
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }
  }
}

TEST_F(test_parse_expression, object_literal_with_contextual_keyword_keyvalue) {
  for (string8_view keyword :
       {u8"as"_sv, u8"async"_sv, u8"await"_sv, u8"from"_sv, u8"get"_sv,
        u8"let"_sv, u8"of"_sv, u8"private"_sv, u8"protected"_sv, u8"public"_sv,
        u8"set"_sv, u8"static"_sv, u8"yield"_sv}) {
    SCOPED_TRACE(out_string8(keyword));

    {
      test_parser p(concat(u8"{"_sv, keyword, u8"}"_sv));
      expression* ast = p.parse_expression();
      EXPECT_EQ(ast->kind(), expression_kind::object);
      EXPECT_EQ(ast->object_entry_count(), 1);
      EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
      EXPECT_EQ(ast->object_entry(0).value->kind(), expression_kind::variable);
      EXPECT_EQ(
          ast->object_entry(0).value->variable_identifier().normalized_name(),
          keyword);
    }

    {
      test_parser p(concat(u8"{"_sv, keyword, u8", other}"_sv));
      expression* ast = p.parse_expression();
      EXPECT_EQ(ast->kind(), expression_kind::object);
      EXPECT_EQ(ast->object_entry_count(), 2);
      EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
      EXPECT_EQ(ast->object_entry(0).value->kind(), expression_kind::variable);
      EXPECT_EQ(
          ast->object_entry(0).value->variable_identifier().normalized_name(),
          keyword);
      EXPECT_EQ(summarize(ast->object_entry(1).value), "var other");
    }
  }
}

TEST_F(test_parse_expression,
       object_literal_with_reserved_keyword_keyvalue_is_an_error) {
  // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
  for (string8_view keyword : disallowed_binding_identifier_keywords) {
    SCOPED_TRACE(out_string8(keyword));

    {
      test_parser p(concat(u8"{"_sv, keyword, u8"}"_sv), capture_diags);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: missing)");
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code,
                                diag_missing_value_for_object_literal_entry,  //
                                key, strlen(u8"{"), keyword),
          }));
    }

    {
      test_parser p(concat(u8"{"_sv, keyword, u8", other}"_sv), capture_diags);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: missing, literal: var other)");
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code,
                                diag_missing_value_for_object_literal_entry,  //
                                key, strlen(u8"{"), keyword),
          }));
    }
  }
}

TEST_F(
    test_parse_expression,
    object_literal_with_reserved_keyword_keyvalue_with_unicode_escapes_is_an_error) {
  {
    test_parser p(u8"{ \\u{69}f }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var if)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_keywords_cannot_contain_escape_sequences,  //
                              escape_sequence, strlen(u8"{ "), u8"\\u{69}"_sv),
        }));
  }
}

TEST_F(test_parse_expression, object_literal_with_number_key) {
  {
    test_parser p(u8"{1234: null}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: literal)");
  }

  {
    test_parser p(u8"{async 42() {}}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
  }

  {
    test_parser p(u8"{*42() {}}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
  }
}

TEST_F(test_parse_expression, incomplete_object_literal) {
  {
    test_parser p(u8"{ p1 "_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var p1)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code, diag_unclosed_object_literal,  //
                                object_open, 0, u8"{"_sv, expected_object_close,
                                strlen(u8"{ p1"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"{ p1, "_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var p1)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code, diag_unclosed_object_literal,  //
                                object_open, 0, u8"{"_sv, expected_object_close,
                                strlen(u8"{ p1,"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"({ p1, )"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "paren(object(literal: var p1))");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_unclosed_object_literal,  //
                        object_open, strlen(u8"("), u8"{"_sv,
                        expected_object_close, strlen(u8"({ p1,"), u8""_sv),
                }));
  }

  {
    test_parser p(u8"[{ p1, ]"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "array(object(literal: var p1))");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_unclosed_object_literal,  //
                        object_open, strlen(u8"["), u8"{"_sv,
                        expected_object_close, strlen(u8"[{ p1,"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_expression, malformed_object_literal) {
  {
    test_parser p(u8"{p1: v1 p2}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var v1, literal: var p2)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_missing_comma_between_object_literal_entries,  //
                where, strlen(u8"{p1: v1"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"{1234}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: missing)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_invalid_lone_literal_in_object_literal,  //
                        where, strlen(u8"{"), u8"1234"_sv),
                }));
  }

  {
    test_parser p(u8"{'x'}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: missing)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_invalid_lone_literal_in_object_literal,  //
                        where, strlen(u8"{"), u8"'x'"_sv),
                }));
  }

  {
    test_parser p(u8"{a b: c}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var a, literal: var c)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_missing_comma_between_object_literal_entries,  //
                where, strlen(u8"{a"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"{a *generator() {}}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var a, literal: function)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_missing_comma_between_object_literal_entries,  //
                where, strlen(u8"{a"), u8""_sv),
        }));
  }

  {
    test_parser p(u8"{async f}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_function_parameter_list,  //
                        expected_parameter_list, strlen(u8"{async f"), u8""_sv),
                }));
  }

  {
    test_parser p(u8"{*f}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_function_parameter_list,  //
                        expected_parameter_list, strlen(u8"{*f"), u8""_sv),
                }));
  }

  {
    test_parser p(u8"{function a(){}}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_methods_should_not_use_function_keyword,  //
                              function_token, strlen(u8"{"), u8"function"_sv),
        }));
  }

  {
    test_parser p(u8"{async function a(){}}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code, diag_methods_should_not_use_function_keyword,  //
                function_token, strlen(u8"{async "), u8"function"_sv),
        }));
  }

  {
    test_parser p(u8"{function *a(){}}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_methods_should_not_use_function_keyword,  //
                              function_token, strlen(u8"{"), u8"function"_sv),
        }));
  }

  {
    test_parser p(u8"{ [x] }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var x: missing)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_value_for_object_literal_entry,  //
                        key, strlen(u8"{ "), u8"[x]"_sv),
                }));
  }

  {
    test_parser p(u8"{ [x], other }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var x: missing, literal: var other)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_value_for_object_literal_entry,  //
                        key, strlen(u8"{ "), u8"[x]"_sv),
                }));
  }

  for (string8_view op : {
           u8"!="_sv,  u8"!=="_sv,    u8"%"_sv,   u8"%="_sv,   u8"&"_sv,
           u8"&&"_sv,  u8"&&="_sv,    u8"&="_sv,  u8"*"_sv,    u8"**"_sv,
           u8"**="_sv, u8"*="_sv,     u8"+"_sv,   u8"+="_sv,   u8"-"_sv,
           u8"-="_sv,  u8"."_sv,      u8"/="_sv,  u8"<<"_sv,   u8"<<="_sv,
           u8"<="_sv,  u8"=="_sv,     u8"==="_sv, u8">"_sv,    u8">="_sv,
           u8">>"_sv,  u8">>="_sv,    u8">>>"_sv, u8">>>="_sv, u8"?."_sv,
           u8"??"_sv,  u8"?\x3f="_sv, u8"^"_sv,   u8"^="_sv,   u8"|"_sv,
           u8"|="_sv,  u8"||"_sv,     u8"||="_sv,
       }) {
    test_parser p(concat(u8"{one "_sv, op, u8" two}"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    expression* ast = p.parse_expression();
    EXPECT_THAT(
        summarize(ast),
        ::testing::AnyOf("object(literal: binary(var one, var two))",
                         "object(literal: condassign(var one, var two))",
                         "object(literal: dot(var one, two))",
                         "object(literal: upassign(var one, var two))"));
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_missing_key_for_object_entry,  //
                              expression, strlen(u8"{"),
                              concat(u8"one "_sv, op, u8" two"_sv)),
        }));
  }

  for (string8_view op : {
           u8"!="_sv, u8"!=="_sv, u8"%"_sv, u8"&"_sv,  u8"&&"_sv, u8"*"_sv,
           u8"**"_sv, u8"+"_sv,   u8"-"_sv, u8"."_sv,  u8"<<"_sv, u8"<="_sv,
           u8"=="_sv, u8"==="_sv, u8">"_sv, u8">="_sv, u8">>"_sv, u8">>>"_sv,
           u8"?."_sv, u8"??"_sv,  u8"^"_sv, u8"|"_sv,  u8"||"_sv,
       }) {
    {
      test_parser p(concat(u8"{'one' "_sv, op, u8" two}"_sv), capture_diags);
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_THAT(summarize(ast),
                  ::testing::AnyOf("object(literal: assign(literal, var two))",
                                   "object(literal: binary(literal, var two))",
                                   "object(literal: dot(literal, two))",
                                   "object(literal: literal = var two)"));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code, diag_missing_key_for_object_entry,  //
                                expression, strlen(u8"{"),
                                concat(u8"'one' "_sv, op, u8" two"_sv)),
          }));
    }

    {
      test_parser p(concat(u8"{1234 "_sv, op, u8" two}"_sv), capture_diags);
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_THAT(summarize(ast),
                  ::testing::AnyOf("object(literal: assign(literal, var two))",
                                   "object(literal: binary(literal, var two))",
                                   "object(literal: dot(literal, two))",
                                   "object(literal: literal = var two)"));
      EXPECT_THAT(
          p.errors,
          ElementsAreArray({
              DIAG_TYPE_OFFSETS(p.code, diag_missing_key_for_object_entry,  //
                                expression, strlen(u8"{"),
                                concat(u8"1234 "_sv, op, u8" two"_sv)),
          }));
    }
  }

  for (string8_view op : {u8"++"_sv, u8"--"_sv}) {
    test_parser p(concat(u8"{one "_sv, op, u8" two}"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: rwunarysuffix(var one), literal: var two)");
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(p.code, diag_missing_key_for_object_entry,  //
                              expression, strlen(u8"{"),
                              concat(u8"one "_sv, op)),
            // TODO(strager): Don't report
            // diag_missing_comma_between_object_literal_entries.
            DIAG_TYPE(diag_missing_comma_between_object_literal_entries)));
  }

  {
    test_parser p(u8"{#key: value}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: var value)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_private_properties_are_not_allowed_in_object_literals,  //
                private_identifier, strlen(u8"{"), u8"#key"_sv),
        }));
  }

  {
    test_parser p(u8"{#key, [other]: value}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var other: var value)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_private_properties_are_not_allowed_in_object_literals,  //
                private_identifier, strlen(u8"{"), u8"#key"_sv),
        }));
  }

  for (string8_view prefix : {u8""_sv, u8"async "_sv, u8"get "_sv, u8"set "_sv,
                              u8"*"_sv, u8"async *"_sv}) {
    padded_string code(concat(u8"{ "_sv, prefix, u8"#method() { } }"_sv));
    SCOPED_TRACE(code);
    test_parser p(code.string_view(), capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_private_properties_are_not_allowed_in_object_literals,  //
                private_identifier, concat(u8"{ "_sv, prefix).size(),
                u8"#method"_sv),
        }));
  }
}

// In some other languages, ';' separates entries similar to how ',' does in
// JavaScript.
TEST_F(test_parse_expression,
       object_literal_entries_are_not_separated_by_semicolon) {
  {
    test_parser p(u8"{ key: value; other: second }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: var value, literal: var second)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ key: value"), u8";"_sv),
        }));
  }

  {
    test_parser p(u8"{ first; get; set; async; }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: var first, literal: var get, "
              "literal: var set, literal: var async)");
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ first"), u8";"_sv),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ first; get"), u8";"_sv),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ first; get; set"), u8";"_sv),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ first; get; set; async"),
                u8";"_sv)));
  }

  {
    test_parser p(u8"{ [key]; other }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var key: missing, literal: var other)");
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ [key]"), u8";"_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_value_for_object_literal_entry,  //
                              key, strlen(u8"{ "), u8"[key]"_sv)));
  }
}

// On some keyboards, '<' is input by pressing ',' while holding the SHIFT key.
TEST_F(test_parse_expression,
       object_literal_entries_are_not_separated_by_less_than_symbol) {
  {
    test_parser p(u8"{ first< get< set< async< }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: var first, literal: var get, "
              "literal: var set, literal: var async)");
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ first"), u8"<"_sv),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ first< get"), u8"<"_sv),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ first< get< set"), u8"<"_sv),
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ first< get< set< async"),
                u8"<"_sv)));
  }

  {
    test_parser p(u8"{ [key]< other }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var key: missing, literal: var other)");
    EXPECT_THAT(
        p.errors,
        UnorderedElementsAre(
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ [key]"), u8"<"_sv),
            DIAG_TYPE_OFFSETS(p.code,
                              diag_missing_value_for_object_literal_entry,  //
                              key, strlen(u8"{ "), u8"[key]"_sv)));
  }

  {
    test_parser p(u8"{ method() {}< other }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function, literal: var other)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_expected_comma_to_separate_object_literal_entries,  //
                unexpected_token, strlen(u8"{ method() {}"), u8"<"_sv),
        }));
  }
}

TEST_F(test_parse_expression,
       object_literal_generator_method_with_misplaced_star) {
  {
    test_parser p(u8"{method*() { yield 42; }}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal: function)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_generator_function_star_belongs_before_name,  //
                function_name, strlen(u8"{"), u8"method"_sv, star,
                strlen(u8"{method"), u8"*"_sv),
        }));
  }

  {
    test_parser p(u8"{ [computed] *() { yield 42; }}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var computed: function)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_generator_function_star_belongs_before_name,  //
                function_name, strlen(u8"{ "), u8"[computed]"_sv, star,
                strlen(u8"{ [computed] "), u8"*"_sv),
        }));
  }
}

TEST_F(test_parse_expression, parse_comma_expression) {
  {
    test_parser p(u8"x,y,z"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::binary_operator);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_EQ(summarize(ast->child(1)), "var y");
    EXPECT_EQ(summarize(ast->child(2)), "var z");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 5));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"(x+(y,z)+w)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "paren(binary(var x, paren(binary(var y, var z)), var w))");
  }

  {
    test_parser p(u8"`${2+2, four}`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(binary(literal, literal, var four))");
  }

  {
    test_parser p(u8"i = 0, j = 0"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(assign(var i, literal), assign(var j, literal))");
  }
}

TEST_F(test_parse_expression, binary_operator_span) {
  for (string8_view op : {
           u8"!="_sv, u8"!=="_sv, u8"%"_sv,  u8"&"_sv,   u8"&&"_sv, u8"*"_sv,
           u8"**"_sv, u8"+"_sv,   u8","_sv,  u8"-"_sv,   u8"/"_sv,  u8"<"_sv,
           u8"<<"_sv, u8"<="_sv,  u8"=="_sv, u8"==="_sv, u8">"_sv,  u8">="_sv,
           u8">>"_sv, u8">>>"_sv, u8"??"_sv, u8"^"_sv,   u8"|"_sv,  u8"||"_sv,
       }) {
    test_parser p(concat(u8"x"_sv, op, u8"y"_sv), capture_diags);
    SCOPED_TRACE(p.code);
    expression* ast = p.parse_expression();
    ASSERT_EQ(ast->kind(), expression_kind::binary_operator);
    auto* binary = static_cast<expression::binary_operator*>(ast);
    EXPECT_THAT(binary->operator_spans_[0],
                p.matches_offsets(strlen(u8"x"), concat(u8"x"_sv, op).size()));
  }

  {
    test_parser p(u8"x + y * z"_sv, capture_diags);
    auto* ast = static_cast<expression::binary_operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0],
                p.matches_offsets(strlen(u8"x "), u8"+"_sv));
    EXPECT_THAT(ast->operator_spans_[1],
                p.matches_offsets(strlen(u8"x + y "), u8"*"_sv));
  }

  {
    test_parser p(u8"x.'foo'"_sv, capture_diags);
    auto* ast = static_cast<expression::binary_operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0], p.matches_offsets(1, 2));
  }

  {
    test_parser p(u8"x .. y"_sv, capture_diags);
    auto* ast = static_cast<expression::binary_operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0],
                p.matches_offsets(strlen(u8"x ."), u8"."_sv));
  }

  {
    test_parser p(u8"x in y"_sv, capture_diags);
    auto* ast = static_cast<expression::binary_operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0],
                p.matches_offsets(strlen(u8"x "), u8"in"_sv));
  }

  {
    test_parser p(u8"f(x y)"_sv, capture_diags);
    expression* ast = p.parse_expression();
    auto* binary = static_cast<expression::binary_operator*>(ast->child_1());
    EXPECT_THAT(binary->operator_spans_[0],
                p.matches_offsets(strlen(u8"f(x"), u8""_sv));
  }

  {
    test_parser p(u8"x.y => z"_sv, capture_diags);
    auto* ast = static_cast<expression::binary_operator*>(p.parse_expression());
    EXPECT_THAT(ast->operator_spans_[0],
                p.matches_offsets(strlen(u8"x.y "), u8"=>"_sv));
  }

  {
    test_parser p(u8"f() => {}"_sv, capture_diags);
    auto* ast = static_cast<expression::binary_operator*>(p.parse_expression());
    // FIXME(strager): These spans look weird.
    EXPECT_THAT(ast->operator_spans_[0], p.matches_offsets(0, u8"f("_sv));
  }
}

TEST_F(test_parse_expression, parse_function_expression) {
  {
    test_parser p(u8"function(){} /* */"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 12));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"function(x, y){}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
  }

  {
    test_parser p(u8"function(){}()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(ast->child_0()->kind(), expression_kind::function);
  }

  {
    test_parser p(u8"function f(){}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::named_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"f"_sv);
  }
}

TEST_F(test_parse_expression, function_with_destructuring_parameters) {
  {
    test_parser p(u8"function({a, b}) { c }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function");
  }

  {
    test_parser p(u8"function([a, b]) { c }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function");
  }
}

TEST_F(test_parse_expression, function_with_spread_and_comma) {
  {
    test_parser p(u8"function(...a, ) { b; }"_sv, capture_diags);
    p.parse_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_comma_not_allowed_after_spread_parameter,  //
                comma, strlen(u8"function(...a"), u8","_sv, spread,
                strlen(u8"function("), u8"...a"_sv),
        }));
  }
}

TEST_F(test_parse_expression, async_function_expression) {
  {
    test_parser p(u8"async function(){}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 18));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"async function f(){}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::named_function);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 20));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, generator_function_expression) {
  {
    test_parser p(u8"function*(){}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(ast->attributes(), function_attributes::generator);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 13));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"function* f(){}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function f");
  }
}

TEST_F(test_parse_expression, async_generator_function_expression) {
  {
    test_parser p(u8"async function*(){}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(ast->attributes(), function_attributes::async_generator);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 19));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"async function* f(){}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function f");
  }
}

TEST_F(test_parse_expression, arrow_function) {
  {
    test_parser p(u8"() => a"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 7));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"a => b"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 6));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"(a) => b"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 8));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"(a, b) => c"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_EQ(summarize(ast->child(1)), "var b");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"() => a, b"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(arrowfunc(), var b)");
  }

  {
    test_parser p(u8"a => b, c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(arrowfunc(var a), var c)");
  }

  {
    test_parser p(u8"(a,) => b"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc(var a)");
  }

  {
    test_parser p(u8"async => value"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc(var async)");
  }

  {
    test_parser p(u8"() => { a; }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 12));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"a => { b; } /* */"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 11));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, arrow_function_with_spread_and_comma) {
  {
    test_parser p(u8"(...b, ) => { c; }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_comma_not_allowed_after_spread_parameter,  //
                comma, strlen(u8"(...b"), u8","_sv, spread, strlen(u8"("),
                u8"...b"_sv),
        }));
    EXPECT_EQ(summarize(ast), "arrowfunc(spread(var b))");
  }
}

TEST_F(test_parse_expression, arrow_function_with_destructuring_parameters) {
  {
    test_parser p(u8"({a, b}) => c"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)),
              "object(literal: var a, literal: var b)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"([a, b]) => c"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "array(var a, var b)");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"(...args) => null"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "spread(var args)");
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, async_arrow_function) {
  {
    test_parser p(u8"async () => { a; }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 18));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"async x => { y; }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"async (x, y, z) => { w; }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "asyncarrowfunc(var x, var y, var z)");
  }

  {
    test_parser p(u8"async () => a"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 13));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"async x => y"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"async (x, y, z) => w"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "asyncarrowfunc(var x, var y, var z)");
  }

  {
    test_parser p(u8"async (a,) => b"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "asyncarrowfunc(var a)");
  }
}

TEST_F(test_parse_expression, invalid_arrow_function) {
  {
    test_parser p(u8"a() => b"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_unexpected_arrow_after_expression,  //
                arrow, strlen(u8"a() "), u8"=>"_sv, expression, 0, u8"a()"_sv),
        }));
    EXPECT_EQ(summarize(ast), "binary(call(var a), var b)");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 0 + strlen(u8"a() => b")));
  }

  {
    test_parser p(u8"a(b) => c"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(call(var a, var b), var c)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(
                        p.code, diag_unexpected_arrow_after_expression,  //
                        arrow, strlen(u8"a(b) "), u8"=>"_sv, expression, 0,
                        u8"a(b)"_sv),
                }));
  }

  {
    test_parser p(u8"a() => {}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, arrowfunc())");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_missing_operator_between_expression_and_arrow_function,  //
                where, 0, u8"a("_sv),
        }));
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 0 + strlen(u8"a() => {}")));
  }

  {
    test_parser p(u8"a(b) => {}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, arrowfunc(var b))");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(
                p.code,
                diag_missing_operator_between_expression_and_arrow_function,  //
                where, 0, u8"a("_sv),
        }));
  }

  {
    test_parser p(u8"=> a"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_arrow_function_parameter_list,  //
                        arrow, 0, u8"=>"_sv),
                }));
  }

  {
    test_parser p(u8"=> { body; }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_arrow_function_parameter_list,  //
                        arrow, 0, u8"=>"_sv),
                }));
  }

  {
    test_parser p(u8"=> { body; }, other"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(arrowfunc(), var other)");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_arrow_function_parameter_list,  //
                        arrow, 0, u8"=>"_sv),
                }));
  }

  {
    test_parser p(u8"42 => body"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, var body)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code, diag_unexpected_arrow_after_literal,  //
                                arrow, strlen(u8"42 "), u8"=>"_sv,
                                literal_parameter, 0, u8"42"_sv),
        }));
  }

  {
    test_parser p(u8"42 => {body();}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(p.code, diag_unexpected_arrow_after_literal,  //
                                arrow, strlen(u8"42 "), u8"=>"_sv,
                                literal_parameter, 0, u8"42"_sv),
        }));
  }

  {
    test_parser p(u8"x.p => rhs"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(dot(var x, p), var rhs)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_unexpected_arrow_after_expression,  //
                arrow, strlen(u8"x.p "), u8"=>"_sv, expression, 0, u8"x.p"_sv),
        }));
  }

  {
    test_parser p(u8"x.p => {body();}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc()");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code, diag_unexpected_arrow_after_expression,  //
                arrow, strlen(u8"x.p "), u8"=>"_sv, expression, 0, u8"x.p"_sv),
        }));
  }

  {
    test_parser p(u8"(x, 42, y) => {body();}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowfunc(var x, literal, var y)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code,
                              diag_unexpected_literal_in_parameter_list,  //
                              literal, strlen(u8"(x, "), u8"42"_sv),
        }));
  }
}

TEST_F(test_parse_expression, function_without_parameter_list) {
  {
    test_parser p(u8"function { return 42; }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_function_parameter_list,  //
                        expected_parameter_list, strlen(u8"function"), u8""_sv),
                }));
  }

  {
    // e.g. if (x) { function }
    test_parser p(u8"function }"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "function");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_function_parameter_list,  //
                        expected_parameter_list, strlen(u8"function"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_expression, invalid_parentheses) {
  {
    test_parser p(u8"()"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "parenempty");
    EXPECT_THAT(p.errors, IsEmpty())
        << "errors should be reported during visitation";
  }

  {
    test_parser p(u8"x = ()"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var x, parenempty)");
    EXPECT_THAT(p.errors, IsEmpty())
        << "errors should be reported during visitation";
  }

  {
    test_parser p(u8"() = x"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(parenempty, var x)");
    EXPECT_THAT(p.errors, IsEmpty())
        << "errors should be reported during visitation";
  }
}

TEST_F(test_parse_expression, invalid_keyword_in_expression) {
  {
    test_parser p(u8"debugger"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "invalid");
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_unexpected_token,  //
                                      token, 0, u8"debugger"_sv),
                }));
  }
}

TEST_F(test_parse_expression, anonymous_class) {
  {
    test_parser p(u8"class {}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_class);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 8));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"class C {}"_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_class);
    EXPECT_THAT(ast->span(), p.matches_offsets(0, 10));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_expression, class_requires_a_body) {
  {
    test_parser p(u8"class C "_sv, capture_diags);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "class");
    EXPECT_THAT(ast->span(), p.matches_offsets(0, u8"class C"_sv));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_missing_body_for_class,  //
                                      class_keyword_and_name_and_heritage, 0,
                                      u8"class C"_sv),
                }));
  }
}

TEST_F(test_parse_expression, parse_mixed_expression) {
  {
    test_parser p(u8"a+f()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, call(var f))");
  }

  {
    test_parser p(u8"a+f(x+y,-z-w)+b"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(var a, call(var f, binary(var x, var y), "
              "binary(unary(var z), var w)), var b)");
  }

  {
    test_parser p(u8"(x+y).z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(paren(binary(var x, var y)), z)");
  }

  {
    test_parser p(u8"/hello/.test(string)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(dot(literal, test), var string)");
  }

  {
    test_parser p(u8"!/hello/.test(string)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(call(dot(literal, test), var string))");
  }

  {
    test_parser p(u8"{a: new A(), b: new B()}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal: new(var A), literal: new(var B))");
  }

  {
    test_parser p(u8"o && typeof o === 'object' ? o[k] : null"_sv);
    expression* ast = p.parse_expression();
    if (false) {  // TODO(strager): Check AST.
      EXPECT_EQ(summarize(ast),
                "cond(binary(var o, binary(typeof(var o), literal)), "
                "index(var o, var k), literal)");
    }
  }

  {
    test_parser p(u8"!!o && k in o"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(unary(unary(var o)), var k, var o)");
  }

  {
    test_parser p(u8"x --> 0"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(rwunarysuffix(var x), literal)");
  }

  {
    test_parser p(u8"class {} + 42"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(class, literal)");
  }

  {
    test_parser p(u8"other + async(a)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var other, call(var async, var a))");
  }

  {
    test_parser p(u8"left + async() + right"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var left, call(var async), var right)");
  }

  {
    test_parser p(u8"left + async + right"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var left, var async, var right)");
  }
}

TEST_F(test_parse_expression,
       reserved_keywords_for_object_properties_can_contain_escape_sequences) {
  for (string8_view keyword : strict_reserved_keywords) {
    string8 property = escape_first_character_in_keyword(keyword);

    {
      test_parser p(concat(u8"obj."_sv, property));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(ast->kind(), expression_kind::dot);
      EXPECT_EQ(summarize(ast->child_0()), "var obj");
      EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
    }

    {
      test_parser p(concat(u8"obj?."_sv, property));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(ast->kind(), expression_kind::dot);
      EXPECT_EQ(summarize(ast->child_0()), "var obj");
      EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
    }

    {
      test_parser p(concat(u8"{ "_sv, property, u8": value }"_sv));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: var value)");
    }

    {
      test_parser p(concat(u8"{ "_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{ get "_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{ set "_sv, property, u8"(v) {} }"_sv));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{ async "_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{ *"_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{ async *"_sv, property, u8"() {} }"_sv));
      SCOPED_TRACE(p.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
    }

    {
      test_parser p(concat(u8"{ function *"_sv, property, u8"() {} }"_sv),
                    capture_diags);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal: function)");
      EXPECT_THAT(p.errors,
                  ElementsAreArray({
                      DIAG_TYPE(diag_methods_should_not_use_function_keyword),
                  }));
    }
  }
}

TEST_F(test_parse_expression, generator_misplaced_star) {
  test_parser p(u8"(*function f(){})"_sv, capture_diags);
  expression* ast = p.parse_expression();
  EXPECT_THAT(ast->child_0()->span(), p.matches_offsets(1, 16));
}

TEST_F(test_parse_expression, unary_cannot_mix_with_star_star) {
  for (char8 op : u8"~!-+"_sv) {
    test_parser p(op + u8"a ** b"s, capture_diags);
    SCOPED_TRACE(p.code);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(unary(var a), var b)");
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_missing_parentheses_around_unary_lhs_of_exponent,  //
                unary_expression, 0, op + u8"a"s,                       //
                exponent_operator, (op + u8"a "s).size(), u8"**"_sv),
        }));
  }

  for (string8_view op : {u8"delete"s, u8"typeof"s, u8"void"s}) {
    test_parser p(concat(op, u8" a ** b"s), capture_diags);
    SCOPED_TRACE(p.code);
    expression* ast = p.parse_expression();
    if ((false)) {
      // TODO(strager): Rewrite the AST into something like the following:
      EXPECT_EQ(summarize(ast), "typeof(binary(var a, var b))");
    }
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_OFFSETS(
                p.code,
                diag_missing_parentheses_around_exponent_with_unary_lhs,  //
                exponent_expression, concat(op, u8" "s).size(), u8"a ** b"_sv,
                unary_operator, 0, op),
        }));
  }
}

TEST_F(test_parse_expression, precedence) {
  enum class level_type {
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

  static auto is_binary_level = [](level_type type) -> bool {
    switch (type) {
    case level_type::left:
    case level_type::right:
    case level_type::binary:
      return true;
    case level_type::prefix:
    case level_type::ternary_right:
      return false;
    }
    QLJS_UNREACHABLE();
  };

  struct operator_type {
    string8_view op;
    const char* raw_kind;

    std::string_view kind() const noexcept {
      if (this->raw_kind) {
        return std::string_view(this->raw_kind);
      } else if (this->op.empty()) {
        return "cond"sv;
      } else {
        return "binary"sv;
      }
    }
  };
  struct precedence_level {
    level_type type;
    std::vector<operator_type> ops;
  };

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_CLANG("-Wmissing-field-initializers")
  QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
  // In our table, lower index items have lower precedence.
  static const precedence_level precedence_levels[] = {
      // TODO(strager): Fix failures when testing e.g. "a,b+c".
      // {level_type::binary, {{u8","_sv}}},
      {level_type::right,
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
      {level_type::ternary_right, {{/* special-cased */}}},
      {level_type::binary, {{u8"||"_sv}, {u8"??"_sv}}},
      {level_type::binary, {{u8"&&"_sv}}},
      {level_type::binary, {{u8"|"_sv}}},
      {level_type::binary, {{u8"^"_sv}}},
      {level_type::binary, {{u8"&"_sv}}},
      {level_type::binary,
       {
           {u8"=="_sv},
           {u8"!="_sv},
           {u8"==="_sv},
           {u8"!=="_sv},
       }},
      {level_type::binary,
       {
           {u8"<"_sv},
           {u8"<="_sv},
           {u8">"_sv},
           {u8">="_sv},
           // TODO(strager): Fix failures when testing e.g. "a in b+c".
           // {u8" in "_sv},
           {u8" instanceof "_sv},
       }},
      {level_type::binary, {{u8"<<"_sv}, {u8">>"_sv}, {u8">>>"_sv}}},
      {level_type::binary, {{u8"+"_sv}, {u8"-"_sv}}},
      {level_type::binary, {{u8"*"_sv}, {u8"/"_sv}, {u8"%"_sv}}},
      {level_type::binary, {{u8"**"_sv}}},
      {level_type::prefix,
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
  for (const precedence_level& level : precedence_levels) {
    for (const operator_type& op : level.ops) {
      switch (level.type) {
      case level_type::left:
      case level_type::right:
        ASSERT_NE(op.kind(), "binary"sv);
        break;
      case level_type::binary:
        ASSERT_EQ(op.kind(), "binary"sv);
        break;
      case level_type::prefix:
        break;
      case level_type::ternary_right:
        ASSERT_EQ(op.op, u8""_sv);
        ASSERT_EQ(op.kind(), "cond"sv);
        break;
      }
    }
  }

  static auto check_expression =
      [](string8_view code, std::string_view expected_ast_summary) -> void {
    SCOPED_TRACE(out_string8(code));
    for (const parser_options& options :
         {javascript_options, typescript_options}) {
      test_parser p(code, options, capture_diags);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), expected_ast_summary);
    }
  };

  static auto test = [](level_type lo_type, operator_type lo_op,
                        level_type hi_type, operator_type hi_op,
                        bool is_same_level) -> void {
    if (lo_type == level_type::binary && hi_type == level_type::binary) {
      // Associativity is not tracked.
      // a*b+c
      check_expression(concat(u8"a"_sv, hi_op.op, u8"b"_sv, lo_op.op, u8"c"_sv),
                       "binary(var a, var b, var c)"sv);
      // a+b*c
      check_expression(concat(u8"a"_sv, lo_op.op, u8"b"_sv, hi_op.op, u8"c"_sv),
                       "binary(var a, var b, var c)"sv);
    } else if (is_binary_level(lo_type) && is_binary_level(hi_type)) {
      if (!is_same_level || hi_type == level_type::right) {
        // a=b+c
        check_expression(
            concat(u8"a"_sv, lo_op.op, u8"b"_sv, hi_op.op, u8"c"_sv),
            concat(lo_op.kind(), "(var a, "sv, hi_op.kind(),
                   "(var b, var c))"sv));
      }
      if (!is_same_level || hi_type == level_type::left) {
        // a=b,c
        check_expression(
            concat(u8"a"_sv, hi_op.op, u8"b"_sv, lo_op.op, u8"c"_sv),
            concat(lo_op.kind(), "("sv, hi_op.kind(),
                   "(var a, var b), var c)"sv));
      }
    } else if (is_binary_level(lo_type) && hi_type == level_type::prefix) {
      // -a*b
      check_expression(
          concat(hi_op.op, u8"a"_sv, lo_op.op, u8"b"_sv),
          concat(lo_op.kind(), "("sv, hi_op.kind(), "(var a), var b)"sv));
    } else if (is_binary_level(lo_type) &&
               hi_type == level_type::ternary_right) {
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
               lo_type == level_type::ternary_right) {
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
    } else if (hi_type == level_type::prefix &&
               lo_type == level_type::ternary_right) {
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
    const precedence_level& hi = precedence_levels[hi_index];
    for (const operator_type& hi_op : hi.ops) {
      for (std::size_t lo_index = 0; lo_index < hi_index; ++lo_index) {
        bool is_same_level = hi_index == lo_index;
        const precedence_level& lo = precedence_levels[lo_index];
        for (const operator_type& lo_op : lo.ops) {
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
