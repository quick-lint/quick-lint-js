// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::_;
using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::VariantWith;

namespace quick_lint_js {
namespace {
std::string summarize(const expression&);
std::string summarize(expression*);
std::string summarize(std::optional<expression*>);
std::string string8_to_string(string8_view);

class test_parser {
 public:
  explicit test_parser(string8_view input)
      : code_(input),
        locator(&this->code_),
        parser_(&this->code_, &this->errors_) {}

  expression* parse_expression() {
    expression* ast = this->parser_.parse_expression();
    this->expressions_needing_cleanup_.push_back(ast);
    return ast;
  }

  const std::vector<error_collector::error>& errors() const noexcept {
    return this->errors_.errors;
  }

  cli_source_range range(expression* ast) { return this->range(ast->span()); }

  cli_source_range range(source_code_span span) {
    return this->locator.range(span);
  }

  padded_string_view code() const noexcept { return &this->code_; }

  quick_lint_js::parser& parser() noexcept { return this->parser_; }

 private:
  padded_string code_;

 public:
  cli_locator locator;

 private:
  error_collector errors_;
  quick_lint_js::parser parser_;
  std::vector<expression*> expressions_needing_cleanup_;
};

class test_parse_expression : public ::testing::Test {
 protected:
  expression* parse_expression(string8_view input) {
    test_parser& p = this->make_parser(input);

    expression* ast = p.parse_expression();
    EXPECT_THAT(p.errors(), IsEmpty());
    return ast;
  }

  test_parser& make_parser(string8_view input) {
    return this->parsers_.emplace_back(input);
  }

 private:
  std::deque<test_parser> parsers_;
};

TEST_F(test_parse_expression, parse_single_token_expression) {
  {
    test_parser p(u8"x"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::variable);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"x");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 1);
  }

  {
    test_parser p(u8"42"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 2);
  }

  {
    test_parser p(u8"'hello'"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 7);
  }

  {
    test_parser p(u8"null"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 4);
  }

  {
    test_parser p(u8"true"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 4);
  }

  {
    test_parser p(u8"false"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 5);
  }

  {
    test_parser p(u8"this"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 4);
  }
}

TEST_F(test_parse_expression, keyword_variable_reference) {
  {
    expression* ast = this->parse_expression(u8"async"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::variable);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"async");
  }

  {
    expression* ast = this->parse_expression(u8"async()"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(ast->child_0()->kind(), expression_kind::variable);
    EXPECT_EQ(ast->child_0()->variable_identifier().normalized_name(),
              u8"async");
  }

  {
    expression* ast = this->parse_expression(u8"async(a, b).c"_sv);
    EXPECT_EQ(summarize(ast), "dot(call(var async, var a, var b), c)");
  }
}

TEST_F(test_parse_expression, private_identifiers_are_not_valid_expressions) {
  {
    test_parser p(u8"#myPrivateField"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::private_variable);
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_cannot_refer_to_private_variable_without_object,
                    private_identifier,
                    offsets_matcher(p.code(), 0, u8"#myPrivateField"))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"#myPrivateField"));
  }
}

TEST_F(test_parse_expression, parse_regular_expression) {
  {
    test_parser p(u8"/regexp/"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 8);
  }

  {
    test_parser p(u8"/=regexp/"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 9);
  }
}

TEST_F(test_parse_expression, parse_math_expression) {
  {
    test_parser p(u8"-x"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::unary_operator);
    EXPECT_EQ(ast->child_0()->kind(), expression_kind::variable);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 2);
  }

  {
    expression* ast = this->parse_expression(u8"+x"_sv);
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }

  {
    expression* ast = this->parse_expression(u8"~x"_sv);
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }

  {
    test_parser p(u8"x+y"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y)");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
  }

  {
    expression* ast = this->parse_expression(u8"x+y-z"_sv);
    EXPECT_EQ(summarize(ast), "binary(var x, var y, var z)");
  }

  {
    expression* ast = this->parse_expression(u8"2-4+1"_sv);
    EXPECT_EQ(summarize(ast), "binary(literal, literal, literal)");
  }

  {
    expression* ast = this->parse_expression(u8"-x+y"_sv);
    EXPECT_EQ(summarize(ast), "binary(unary(var x), var y)");
  }

  for (const char8* input :
       {u8"2+2", u8"2-2", u8"2*2", u8"2/2", u8"2%2", u8"2**2", u8"2^2", u8"2&2",
        u8"2|2", u8"2<<2", u8"2>>2", u8"2>>>2"}) {
    SCOPED_TRACE(out_string8(u8"input = " + string8(input)));
    expression* ast = this->parse_expression(input);
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
  }
}

TEST_F(test_parse_expression, parse_broken_math_expression) {
  {
    test_parser p(u8"2+"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, ?)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_operand_for_operator, where,
                    offsets_matcher(p.code(), strlen(u8"2"), u8"+"))));
  }

  {
    test_parser p(u8"^2"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(?, literal)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_operand_for_operator, where,
                                offsets_matcher(p.code(), 0, u8"^"))));
  }

  {
    test_parser p(u8"2 * * 2"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, ?, literal)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_operand_for_operator, where,
                    offsets_matcher(p.code(), strlen(u8"2 "), u8"*"))));
  }

  {
    test_parser p(u8"2 & & & 2"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, ?, ?, literal)");

    EXPECT_THAT(
        p.errors(),
        ElementsAre(
            ERROR_TYPE_FIELD(error_missing_operand_for_operator, where,
                             offsets_matcher(p.code(), strlen(u8"2 "), u8"&")),
            ERROR_TYPE_FIELD(
                error_missing_operand_for_operator, where,
                offsets_matcher(p.code(), strlen(u8"2 & "), u8"&"))));
  }

  {
    test_parser p(u8"(2*)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, ?)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_operand_for_operator, where,
                                offsets_matcher(p.code(), 2, u8"*"))));
  }

  {
    test_parser p(u8"2 * (3 + 4"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, binary(literal, literal))");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unmatched_parenthesis, where,
                    offsets_matcher(p.code(), strlen(u8"2 * "), u8"("))));
  }

  {
    test_parser p(u8"2 * (3 + (4"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, binary(literal, literal))");

    EXPECT_THAT(
        p.errors(),
        ElementsAre(
            ERROR_TYPE_FIELD(
                error_unmatched_parenthesis, where,
                offsets_matcher(p.code(), strlen(u8"2 * (3 + "), u8"(")),
            ERROR_TYPE_FIELD(
                error_unmatched_parenthesis, where,
                offsets_matcher(p.code(), strlen(u8"2 * "), u8"("))));
  }
}

TEST_F(test_parse_expression, comma_expression_with_trailing_comma) {
  {
    // Arrow expressions allow trailing commas in their parenthesized parameter
    // lists, but comma expressions do not allow trailing commas.
    test_parser p(u8"(a, b, c,)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "trailingcomma(var a, var b, var c)");
    EXPECT_THAT(p.errors(), IsEmpty())
        << "trailing comma expression emits no errors; errors are emitted "
           "depending on the context";
  }
}

TEST_F(test_parse_expression, parse_logical_expression) {
  for (const char8* input :
       {u8"2==2", u8"2===2", u8"2!=2", u8"2!==2", u8"2>2", u8"2<2", u8"2>=2",
        u8"2<=2", u8"2&&2", u8"2??2", u8"2||2"}) {
    SCOPED_TRACE(out_string8(u8"input = " + string8(input)));
    test_parser p(input);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
  }

  {
    expression* ast = this->parse_expression(u8"!x"_sv);
    EXPECT_EQ(summarize(ast), "unary(var x)");
  }
}

TEST_F(test_parse_expression, parse_keyword_binary_operators) {
  {
    expression* ast = this->parse_expression(u8"prop in object"_sv);
    EXPECT_EQ(summarize(ast), "binary(var prop, var object)");
  }

  {
    expression* ast = this->parse_expression(u8"object instanceof Class"_sv);
    EXPECT_EQ(summarize(ast), "binary(var object, var Class)");
  }
}

TEST_F(test_parse_expression, parse_typeof_unary_operator) {
  {
    expression* ast = this->parse_expression(u8"typeof o"_sv);
    EXPECT_EQ(summarize(ast), "typeof(var o)");
  }

  {
    expression* ast = this->parse_expression(u8"typeof o === 'number'"_sv);
    EXPECT_EQ(summarize(ast), "binary(typeof(var o), literal)");
  }

  {
    expression* ast = this->parse_expression(u8"typeof o.p"_sv);
    EXPECT_EQ(summarize(ast), "typeof(dot(var o, p))");
  }
}

TEST_F(test_parse_expression, parse_typeof_conditional_operator) {
  {
    expression* ast = this->parse_expression(u8"typeof o ? 10 : 20"_sv);
    EXPECT_EQ(summarize(ast), "cond(typeof(var o), literal, literal)");
  }
}

TEST_F(test_parse_expression, delete_unary_operator) {
  {
    test_parser p(u8"delete variable");
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(var variable)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_redundant_delete_statement_on_variable, delete_expression,
            offsets_matcher(p.code(), 0, u8"delete variable"))));
  }

  {
    expression* ast = this->parse_expression(u8"delete variable.property"_sv);
    EXPECT_EQ(summarize(ast), "unary(dot(var variable, property))");
  }
}

TEST_F(test_parse_expression, void_unary_operator) {
  {
    expression* ast = this->parse_expression(u8"void 0"_sv);
    EXPECT_EQ(summarize(ast), "unary(literal)");
  }
}

TEST_F(test_parse_expression, spread) {
  {
    test_parser p(u8"...args"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "spread(var args)");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 7);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, conditional_expression) {
  {
    test_parser p(u8"x?y:z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::conditional);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_EQ(summarize(ast->child_2()), "var z");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 5);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"x+x?y+y:z+z"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::conditional);
    EXPECT_EQ(summarize(ast->child_0()), "binary(var x, var x)");
    EXPECT_EQ(summarize(ast->child_1()), "binary(var y, var y)");
    EXPECT_EQ(summarize(ast->child_2()), "binary(var z, var z)");
  }

  {
    expression* ast = this->parse_expression(u8"a ? b : c ? d : e"_sv);
    EXPECT_EQ(summarize(ast), "cond(var a, var b, cond(var c, var d, var e))");
  }
}

TEST_F(test_parse_expression, conditional_expression_with_missing_condition) {
  {
    test_parser p(u8"? b : c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(?, var b, var c)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_operand_for_operator, where,
                                offsets_matcher(p.code(), 0, u8"?"))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"? b : c"));
  }
}

TEST_F(test_parse_expression,
       conditional_expression_with_missing_true_component) {
  {
    test_parser p(u8"a ? : c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, ?, var c)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_operand_for_operator, where,
                    offsets_matcher(p.code(), strlen(u8"a "), u8"?"))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"a ? : c"));
  }
}

TEST_F(test_parse_expression,
       conditional_expression_with_missing_false_component) {
  {
    test_parser p(u8"a ? b : "_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, ?)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_operand_for_operator, where,
                    offsets_matcher(p.code(), strlen(u8"a ? b "), u8":"))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    // TODO(strager): Fix end_offset to exclude the trailing whitespace.
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"a ? b : "));
  }

  {
    test_parser p(u8"(a ? b :)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, ?)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_operand_for_operator, where,
                    offsets_matcher(p.code(), strlen(u8"(a ? b "), u8":"))));
    EXPECT_EQ(p.range(ast).begin_offset(), strlen(u8"("));
    // TODO(strager): Fix end_offset to exclude the ')'.
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"(a ? b :)"));
  }
}

TEST_F(test_parse_expression,
       conditional_expression_with_missing_colon_and_false_component) {
  {
    test_parser p(u8"a ? b "_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, ?)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_colon_in_conditional_expression, expected_colon,
            offsets_matcher(p.code(), strlen(u8"a ? b"), u8""),  //
            question, offsets_matcher(p.code(), strlen(u8"a "), u8"?"))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"a ? b"));
  }

  {
    test_parser p(u8"a ? b c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, ?)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_colon_in_conditional_expression, expected_colon,
            offsets_matcher(p.code(), strlen(u8"a ? b"), u8""),  //
            question, offsets_matcher(p.code(), strlen(u8"a "), u8"?"))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"a ? b"));
  }

  {
    test_parser p(u8"(a ? b)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(var a, var b, ?)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_colon_in_conditional_expression, expected_colon,
            offsets_matcher(p.code(), strlen(u8"(a ? b"), u8""),  //
            question, offsets_matcher(p.code(), strlen(u8"(a "), u8"?"))));
    EXPECT_EQ(p.range(ast).begin_offset(), strlen(u8"("));
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"(a ? b"));
  }
}

TEST_F(test_parse_expression, parse_function_call) {
  {
    test_parser p(u8"f()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
    expression::call* call = expression_cast<expression::call>(ast);
    EXPECT_EQ(p.range(call->left_paren_span()).begin_offset(), 1);
    EXPECT_EQ(p.range(call->left_paren_span()).end_offset(), 2);
  }

  {
    expression* ast = this->parse_expression(u8"f(x)"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(1)), "var x");
  }

  {
    expression* ast = this->parse_expression(u8"f(x,y)"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 3);
    EXPECT_EQ(summarize(ast->child(1)), "var x");
    EXPECT_EQ(summarize(ast->child(2)), "var y");
  }
}

TEST_F(test_parse_expression, function_call_with_invalid_extra_commas) {
  {
    test_parser p(u8"f(,)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_extra_comma_not_allowed_between_arguments, comma,
                    offsets_matcher(p.code(), strlen(u8"f("), u8","))));
  }

  {
    test_parser p(u8"f(a,,b)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f, var a, var b)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_extra_comma_not_allowed_between_arguments, comma,
                    offsets_matcher(p.code(), strlen(u8"f(a,"), u8","))));
  }

  {
    // A function named 'async' in a special case because of lookahead:
    // 'async()' is a function call, but 'async()=>{}' is an arrow function.
    test_parser p(u8"async(a,,b)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var async, var a, var b)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_extra_comma_not_allowed_between_arguments, comma,
                    offsets_matcher(p.code(), strlen(u8"async(a,"), u8","))));
  }
}

TEST_F(test_parse_expression, parse_optional_function_call) {
  {
    expression* ast = this->parse_expression(u8"f?.(x,y)"_sv);
    EXPECT_EQ(summarize(ast), "call(var f, var x, var y)");
  }
}

TEST_F(test_parse_expression, parse_dot_expressions) {
  {
    test_parser p(u8"x.prop"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::dot);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"prop");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 6);
  }

  {
    expression* ast = this->parse_expression(u8"x.p1.p2"_sv);
    EXPECT_EQ(summarize(ast), "dot(dot(var x, p1), p2)");
  }

  for (string8 keyword : keywords) {
    SCOPED_TRACE(out_string8(keyword));
    string8 code = u8"promise." + keyword;
    expression* ast = this->parse_expression(code.c_str());
    EXPECT_EQ(summarize(ast),
              "dot(var promise, " + string8_to_string(keyword) + ")");
  }

  {
    expression* ast = this->parse_expression(u8"x.#private"_sv);
    EXPECT_EQ(summarize(ast), "dot(var x, #private)");
  }
}

TEST_F(test_parse_expression, invalid_dot_expression) {
  {
    test_parser p(u8"x.''"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, literal)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_invalid_rhs_for_dot_operator, dot,
                    offsets_matcher(p.code(), strlen(u8"x"), u8"."))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"x.''"));
  }

  {
    test_parser p(u8"x. "_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, )");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_property_name_for_dot_operator, dot,
                    offsets_matcher(p.code(), strlen(u8"x"), u8"."))));
  }

  {
    test_parser p(u8"(x.)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, )");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_property_name_for_dot_operator, dot,
                    offsets_matcher(p.code(), strlen(u8"(x"), u8"."))));
  }

  for (string8 op : {
           u8"!=",  u8"!==", u8"%",    u8"%=",  u8"&",      u8"&&", u8"&&=",
           u8"&=",  u8"*",   u8"**",   u8"**=", u8"*=",     u8"+",  u8"+=",
           u8",",   u8"-",   u8"-=",   u8"/=",  u8"<",      u8"<<", u8"<<=",
           u8"<=",  u8"=",   u8"==",   u8"===", u8">",      u8">=", u8">>",
           u8">>=", u8">>>", u8">>>=", u8"??",  u8"?\x3f=", u8"^",  u8"^=",
           u8"|",   u8"|=",  u8"||",   u8"||=",
       }) {
    test_parser p(u8"x. " + op + u8" y");
    expression* ast = p.parse_expression();
    EXPECT_THAT(summarize(ast),
                ::testing::AnyOf("assign(dot(var x, ), var y)",      //
                                 "binary(dot(var x, ), var y)",      //
                                 "condassign(dot(var x, ), var y)",  //
                                 "upassign(dot(var x, ), var y)"));
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_property_name_for_dot_operator, dot,
                    offsets_matcher(p.code(), strlen(u8"x"), u8"."))));
  }

  {
    test_parser p(u8"x. ? y. : z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "cond(dot(var x, ), dot(var y, ), var z)");
    EXPECT_THAT(
        p.errors(),
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(error_missing_property_name_for_dot_operator, dot,
                             offsets_matcher(p.code(), strlen(u8"x"), u8".")),
            ERROR_TYPE_FIELD(
                error_missing_property_name_for_dot_operator, dot,
                offsets_matcher(p.code(), strlen(u8"x. ? y"), u8"."))));
  }
}

TEST_F(test_parse_expression, parse_optional_dot_expressions) {
  {
    test_parser p(u8"x?.prop"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(var x, prop)");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"x?.prop"));
  }

  for (string8 keyword : keywords) {
    padded_string code(u8"obj?." + keyword);
    SCOPED_TRACE(code);
    expression* ast = this->parse_expression(code.string_view());
    EXPECT_EQ(ast->kind(), expression_kind::dot);
    EXPECT_EQ(summarize(ast->child_0()), "var obj");
    EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
  }

  {
    expression* ast = this->parse_expression(u8"x?.#private"_sv);
    EXPECT_EQ(summarize(ast), "dot(var x, #private)");
  }
}

TEST_F(test_parse_expression, parse_indexing_expression) {
  {
    test_parser p(u8"xs[i]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::index);
    EXPECT_EQ(summarize(ast->child_0()), "var xs");
    EXPECT_EQ(summarize(ast->child_1()), "var i");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 5);
  }
}

TEST_F(test_parse_expression, parse_optional_indexing_expression) {
  {
    test_parser p(u8"xs?.[i]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, var i)");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"xs?.[i]"));
  }
}

TEST_F(test_parse_expression, parse_unclosed_indexing_expression) {
  {
    test_parser p(u8"xs[i"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, var i)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unmatched_indexing_bracket, left_square,
                    offsets_matcher(p.code(), strlen(u8"xs"), u8"["))));
  }
}

TEST_F(test_parse_expression, empty_indexing_expression) {
  {
    test_parser p(u8"xs[]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "index(var xs, ?)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_indexing_requires_expression, squares,
                    offsets_matcher(p.code(), strlen(u8"xs"), u8"[]"))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"xs[]"));
  }
}

TEST_F(test_parse_expression, parse_parenthesized_expression) {
  {
    test_parser p(u8"(x)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "var x");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 1);
    EXPECT_EQ(p.range(ast).end_offset(), 2);
  }

  {
    expression* ast = this->parse_expression(u8"x+(y)"_sv);
    EXPECT_EQ(summarize(ast), "binary(var x, var y)");
  }

  {
    expression* ast = this->parse_expression(u8"x+(y+z)"_sv);
    EXPECT_EQ(summarize(ast), "binary(var x, binary(var y, var z))");
  }

  {
    expression* ast = this->parse_expression(u8"(x+y)+z"_sv);
    EXPECT_EQ(summarize(ast), "binary(binary(var x, var y), var z)");
  }

  {
    expression* ast = this->parse_expression(u8"x+(y+z)+w"_sv);
    EXPECT_EQ(summarize(ast), "binary(var x, binary(var y, var z), var w)");
  }
}

TEST_F(test_parse_expression, await_unary_operator_inside_async_functions) {
  {
    test_parser p(u8"await myPromise"_sv);
    auto guard = p.parser().enter_function(function_attributes::async);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(var myPromise)");
    EXPECT_EQ(ast->kind(), expression_kind::await);
    EXPECT_EQ(summarize(ast->child_0()), "var myPromise");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 15);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"await(x)"_sv);
    auto guard = p.parser().enter_function(function_attributes::async);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(var x)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression,
       await_in_normal_function_vs_async_function_vs_top_level) {
  struct test_case {
    string8_view code;
    const char* expected_normal_function;
    const char* expected_async_function;
  };

  for (test_case test : {
         // clang-format off
         test_case

         // 'await' is either an identifier or a unary operator:
         {u8"await/re/g"_sv,       "binary(var await, var re, var g)",           "await(literal)"},
         {u8"await+x"_sv,          "binary(var await, var x)",                   "await(unary(var x))"},
         {u8"await-x"_sv,          "binary(var await, var x)",                   "await(unary(var x))"},
         {u8"await(x)"_sv,         "call(var await, var x)",                     "await(var x)"},
         {u8"await[x]"_sv,         "index(var await, var x)",                    "await(array(var x))"},
         {u8"await++\nx"_sv,       "rwunarysuffix(var await)",                   "await(rwunary(var x))"},
         {u8"await--\nx"_sv,       "rwunarysuffix(var await)",                   "await(rwunary(var x))"},
         {u8"await`some${x}`"_sv,  "taggedtemplate(var await, var x)",           "await(template(var x))"},
         {u8"await`something`"_sv, "taggedtemplate(var await)",                  "await(literal)"},
         {u8"await/=re/g"_sv,      "upassign(var await, binary(var re, var g))", "await(literal)"},

         // 'await' must be a unary operator:
         {u8"await async () => {}"_sv, nullptr, "await(asyncarrowblock())"},
         {u8"await await x"_sv,        nullptr, "await(await(var x))"},
         {u8"await class{}"_sv,        nullptr, "await(class)"},
         {u8"await function() {}"_sv,  nullptr, "await(function)"},
         {u8"await /regexp/"_sv,       nullptr, "await(literal)"},
         {u8"await /=regexp/"_sv,      nullptr, "await(literal)"},
         {u8"await 42"_sv,             nullptr, "await(literal)"},
         {u8"await false"_sv,          nullptr, "await(literal)"},
         {u8"await null"_sv,           nullptr, "await(literal)"},
         {u8"await this"_sv,           nullptr, "await(literal)"},
         {u8"await true"_sv,           nullptr, "await(literal)"},
         {u8"await new C()"_sv,        nullptr, "await(new(var C))"},
         {u8"await++x"_sv,             nullptr, "await(rwunary(var x))"},
         {u8"await--x"_sv,             nullptr, "await(rwunary(var x))"},
         {u8"await super"_sv,          nullptr, "await(super)"},
         {u8"await typeof x"_sv,       nullptr, "await(typeof(var x))"},
         {u8"await !x"_sv,             nullptr, "await(unary(var x))"},
         {u8"await delete x.p"_sv,     nullptr, "await(unary(dot(var x, p)))"},
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

         // 'await' must be an identifier:
         {u8"[await]"_sv,             "array(var await)",              nullptr},
         {u8"await => x"_sv,          "arrowexpr(var await, var x)",   nullptr},
         {u8"await = x"_sv,           "assign(var await, var x)",      nullptr},
         {u8"await != x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await !== x"_sv,         "binary(var await, var x)",      nullptr},
         {u8"await % x"_sv,           "binary(var await, var x)",      nullptr},
         {u8"await & x"_sv,           "binary(var await, var x)",      nullptr},
         {u8"await && x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await ** x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await / x"_sv,           "binary(var await, var x)",      nullptr},
         {u8"await < x"_sv,           "binary(var await, var x)",      nullptr},
         {u8"await << x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await <= x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await == x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await === x"_sv,         "binary(var await, var x)",      nullptr},
         {u8"await > x"_sv,           "binary(var await, var x)",      nullptr},
         {u8"await >= x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await >> x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await >>> x"_sv,         "binary(var await, var x)",      nullptr},
         {u8"await ?? x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await ^ x"_sv,           "binary(var await, var x)",      nullptr},
         {u8"await in xs"_sv,         "binary(var await, var xs)",     nullptr},
         {u8"await instanceof X"_sv,  "binary(var await, var X)",      nullptr},
         {u8"await | x"_sv,           "binary(var await, var x)",      nullptr},
         {u8"await || x"_sv,          "binary(var await, var x)",      nullptr},
         {u8"await, x"_sv,            "binary(var await, var x)",      nullptr},
         {u8"await ? x : y"_sv,       "cond(var await, var x, var y)", nullptr},
         {u8"x ? await : y"_sv,       "cond(var x, var await, var y)", nullptr},
         {u8"await &&= x"_sv,         "condassign(var await, var x)",  nullptr},
         {u8"await ?\x3f= x"_sv,      "condassign(var await, var x)",  nullptr},
         {u8"await ||= x"_sv,         "condassign(var await, var x)",  nullptr},
         {u8"await.prop"_sv,          "dot(var await, prop)",          nullptr},
         {u8"await?.prop"_sv,         "dot(var await, prop)",          nullptr},
         {u8"{key: await}"_sv,        "object(literal, var await)",    nullptr},
         {u8"await %= x"_sv,          "upassign(var await, var x)",    nullptr},
         {u8"await &= x"_sv,          "upassign(var await, var x)",    nullptr},
         {u8"await **= x"_sv,         "upassign(var await, var x)",    nullptr},
         {u8"await *= x"_sv,          "upassign(var await, var x)",    nullptr},
         {u8"await += x"_sv,          "upassign(var await, var x)",    nullptr},
         {u8"await -= x"_sv,          "upassign(var await, var x)",    nullptr},
         {u8"await /= x"_sv,          "upassign(var await, var x)",    nullptr},
         {u8"await <<= x"_sv,         "upassign(var await, var x)",    nullptr},
         {u8"await >>= x"_sv,         "upassign(var await, var x)",    nullptr},
         {u8"await >>>= x"_sv,        "upassign(var await, var x)",    nullptr},
         {u8"await ^= x"_sv,          "upassign(var await, var x)",    nullptr},
         {u8"await |= x"_sv,          "upassign(var await, var x)",    nullptr},
         {u8"await"_sv,               "var await",                     nullptr},
         {u8"(await)"_sv,             "var await",                     nullptr},
         {u8"await;"_sv,              "var await",                     nullptr},

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
      test_parser p(test.code);
      auto guard = p.parser().enter_function(function_attributes::normal);
      expression* ast = p.parse_expression();

      if (test.code == u8"await--x" || test.code == u8"await++x" ||
          test.code == u8"await of") {
        // TODO(strager): Make these test cases pass.
      } else if (test.expected_normal_function) {
        // 'await' should look like an identifier.
        EXPECT_EQ(summarize(ast), test.expected_normal_function);
        EXPECT_THAT(p.errors(), IsEmpty());
      } else {
        // 'await' doesn't look like an identifier. We should report an error
        // and recover as if 'await' was an operator.
        EXPECT_EQ(summarize(ast), test.expected_async_function);
        if (test.code == u8"await await x") {
          EXPECT_THAT(
              p.errors(),
              ElementsAre(
                  ERROR_TYPE_FIELD(error_await_operator_outside_async,
                                   await_operator,
                                   offsets_matcher(p.code(), 0, u8"await")),  //
                  ERROR_TYPE_FIELD(error_await_operator_outside_async,
                                   await_operator,
                                   offsets_matcher(p.code(), strlen(u8"await "),
                                                   u8"await"))));
        } else {
          std::size_t await_offset = test.code.find(u8"await");
          EXPECT_THAT(p.errors(),
                      ElementsAre(ERROR_TYPE_FIELD(
                          error_await_operator_outside_async, await_operator,
                          offsets_matcher(p.code(), await_offset, u8"await"))));
        }
      }
    }

    if (test.expected_async_function) {
      // Async function:
      test_parser p(test.code);
      auto guard = p.parser().enter_function(function_attributes::async);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), test.expected_async_function);
      EXPECT_THAT(p.errors(), IsEmpty());
    }

    {
      // Top level:
      test_parser p(test.code);
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), test.expected_async_function
                                    ? test.expected_async_function
                                    : test.expected_normal_function);
      EXPECT_THAT(p.errors(), IsEmpty());
    }
  }
}

TEST_F(test_parse_expression, await_variable_name_outside_async_functions) {
  {
    test_parser p(u8"await(x)"_sv);
    auto guard = p.parser().enter_function(function_attributes::normal);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var await, var x)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, await_unary_operator_outside_async_functions) {
  {
    test_parser p(u8"await myPromise"_sv);
    auto guard = p.parser().enter_function(function_attributes::normal);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(var myPromise)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_await_operator_outside_async, await_operator,
                    offsets_matcher(p.code(), 0, u8"await"))));
  }
}

TEST_F(test_parse_expression,
       yield_nullary_operator_inside_generator_functions) {
  auto parse_expression_in_generator =
      [this](const char8* code) -> expression* {
    test_parser& p = this->make_parser(code);
    auto guard = p.parser().enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_THAT(p.errors(), IsEmpty());
    return ast;
  };

  {
    test_parser p(u8"yield"_sv);
    auto guard = p.parser().enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yieldnone");
    EXPECT_EQ(ast->kind(), expression_kind::yield_none);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 5);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = parse_expression_in_generator(u8"(yield)");
    EXPECT_EQ(summarize(ast), "yieldnone");
  }

  {
    expression* ast = parse_expression_in_generator(u8"[yield]");
    EXPECT_EQ(summarize(ast), "array(yieldnone)");
  }

  {
    expression* ast = parse_expression_in_generator(u8"f(yield, 42)");
    EXPECT_EQ(summarize(ast), "call(var f, yieldnone, literal)");
  }

  {
    expression* ast = parse_expression_in_generator(u8"yield ? a : b");
    EXPECT_EQ(summarize(ast), "cond(yieldnone, var a, var b)");
  }

  {
    expression* ast = parse_expression_in_generator(u8"yield in stuff");
    EXPECT_EQ(summarize(ast), "binary(yieldnone, var stuff)");
  }

  {
    expression* ast = parse_expression_in_generator(u8"yield;");
    EXPECT_EQ(summarize(ast), "yieldnone");
  }

  {
    // '}' is the end of a function's body, for example.
    expression* ast = parse_expression_in_generator(u8"yield }");
    EXPECT_EQ(summarize(ast), "yieldnone");
  }

  {
    expression* ast = parse_expression_in_generator(u8"a ? yield : b");
    EXPECT_EQ(summarize(ast), "cond(var a, yieldnone, var b)");
  }

  {
    expression* ast = parse_expression_in_generator(u8"yield, yield");
    EXPECT_EQ(summarize(ast), "binary(yieldnone, yieldnone)");
  }

  {
    expression* ast = parse_expression_in_generator(u8"[yield, yield, yield]");
    EXPECT_EQ(summarize(ast), "array(yieldnone, yieldnone, yieldnone)");
  }
}

TEST_F(test_parse_expression, yield_unary_operator_inside_generator_functions) {
  {
    test_parser p(u8"yield v"_sv);
    auto guard = p.parser().enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yield(var v)");
    EXPECT_EQ(ast->kind(), expression_kind::yield_one);
    EXPECT_EQ(summarize(ast->child_0()), "var v");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 7);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"yield(x)"_sv);
    auto guard = p.parser().enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yield(var x)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"f(yield a, yield b, c)}"_sv);
    auto generator_guard =
        p.parser().enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var f, yield(var a), yield(var b), var c)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression,
       yield_many_unary_operator_inside_generator_functions) {
  {
    test_parser p(u8"yield *other"_sv);
    auto guard = p.parser().enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "yieldmany(var other)");
    EXPECT_EQ(ast->kind(), expression_kind::yield_many);
    EXPECT_EQ(summarize(ast->child_0()), "var other");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 12);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"f(yield *a, yield* b, c)}"_sv);
    auto generator_guard =
        p.parser().enter_function(function_attributes::generator);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "call(var f, yieldmany(var a), yieldmany(var b), var c)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, yield_variable_name_outside_generator_functions) {
  {
    test_parser p(u8"yield(x)"_sv);
    auto guard = p.parser().enter_function(function_attributes::normal);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(var yield, var x)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"yield*other"_sv);
    auto guard = p.parser().enter_function(function_attributes::normal);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var yield, var other)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_new_expression) {
  {
    test_parser p(u8"new Date"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_new);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var Date");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 8);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"new Date()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_new);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var Date");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 10);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"new Date(y,m,d)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Date, var y, var m, var d)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, new_target) {
  {
    test_parser p(u8"new.target"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "newtarget");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 10);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"new.target()"_sv);
    EXPECT_EQ(summarize(ast), "call(newtarget)");
  }
}

TEST_F(test_parse_expression, super) {
  {
    test_parser p(u8"super()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(super)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"super.method()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(dot(super, method))");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, import) {
  {
    test_parser p(u8"import(url)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "call(import, var url)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"import.meta"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(import, meta)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_assignment) {
  {
    test_parser p(u8"x=y"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
  }

  {
    expression* ast = this->parse_expression(u8"x.p=z"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::assignment);
    EXPECT_EQ(summarize(ast->child_0()), "dot(var x, p)");
    EXPECT_EQ(summarize(ast->child_1()), "var z");
  }

  {
    expression* ast = this->parse_expression(u8"f().p=x"_sv);
    EXPECT_EQ(summarize(ast), "assign(dot(call(var f), p), var x)");
  }

  {
    expression* ast = this->parse_expression(u8"x=y=z"_sv);
    EXPECT_EQ(summarize(ast), "assign(var x, assign(var y, var z))");
  }

  {
    expression* ast = this->parse_expression(u8"x,y=z,w"_sv);
    EXPECT_EQ(summarize(ast), "binary(var x, assign(var y, var z), var w)");
  }
}

TEST_F(test_parse_expression, parse_compound_assignment) {
  for (string8 op : {u8"*=", u8"/=", u8"%=", u8"+=", u8"-=", u8"<<=", u8">>=",
                     u8">>>=", u8"&=", u8"^=", u8"|=", u8"**="}) {
    SCOPED_TRACE(out_string8(op));
    string8 code = u8"x " + op + u8" y";
    test_parser p(code.c_str());
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::compound_assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), code.size());
  }
}

TEST_F(test_parse_expression, parse_conditional_assignment) {
  for (string8 op : {u8"&&=", u8"?\x3f=", u8"||="}) {
    SCOPED_TRACE(out_string8(op));
    string8 code = u8"x " + op + u8" y";
    test_parser p(code.c_str());
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::conditional_assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), code.size());
  }
}

TEST_F(test_parse_expression, parse_invalid_assignment) {
  {
    test_parser p(u8"x+y=z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(binary(var x, var y), var z)");

    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_invalid_expression_left_of_assignment,
                                where, offsets_matcher(p.code(), 0, u8"x+y"))));
  }

  for (const char8* code : {
           u8"f()=x",
           u8"-x=y",
           u8"42=y",
           u8"(x=y)=z",
       }) {
    SCOPED_TRACE(out_string8(code));
    test_parser p(code);
    p.parse_expression();

    EXPECT_THAT(
        p.errors(),
        ElementsAre(
            VariantWith<error_invalid_expression_left_of_assignment>(_)));
  }
}

TEST_F(test_parse_expression, parse_prefix_plusplus_minusminus) {
  {
    test_parser p(u8"++x"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_prefix);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"--y"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_prefix);
    EXPECT_EQ(summarize(ast->child_0()), "var y");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_prefix_plusplus_plus_operand) {
  {
    test_parser p(u8"++x\n+\ny"_sv);

    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(rwunary(var x), var y)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"--x\n+\ny"_sv);

    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(rwunary(var x), var y)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"++x.y"_sv);

    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(dot(var x, y))");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"++x[y]"_sv);

    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(index(var x, var y))");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, parse_unary_prefix_operator_with_no_operand) {
  {
    test_parser p(u8"--"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(?)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_operand_for_operator, where,
                                offsets_matcher(p.code(), 0, u8"--"))));
  }

  {
    test_parser p(u8"++;"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(?)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_operand_for_operator, where,
                                offsets_matcher(p.code(), 0, u8"++"))));
  }

  {
    test_parser p(u8"(-)"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(?)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_operand_for_operator, where,
                    offsets_matcher(p.code(), strlen(u8"("), u8"-"))));
  }

  {
    test_parser p(u8"!;"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(?)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_operand_for_operator, where,
                                offsets_matcher(p.code(), 0, u8"!"))));
  }

  {
    test_parser p(u8"await}"_sv);
    auto guard = p.parser().enter_function(function_attributes::async);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "await(?)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_operand_for_operator, where,
                                offsets_matcher(p.code(), 0, u8"await"))));
  }
}

TEST_F(test_parse_expression, parse_suffix_plusplus_minusminus) {
  {
    test_parser p(u8"x++"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_suffix);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, suffix_plusplus_minusminus_disallows_line_break) {
  {
    test_parser p(u8"x\n++\ny"_sv);

    expression* ast_1 = p.parse_expression();
    EXPECT_EQ(summarize(ast_1), "var x");

    expression* ast_2 = p.parse_expression();
    EXPECT_EQ(summarize(ast_2), "rwunary(var y)");

    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, prefix_plusplus_minusminus_cannot_nest) {
  {
    test_parser p(u8"++ ++ x"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "rwunary(rwunary(var x))");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 7);
    // TODO(strager): Report an error. ++ takes a LeftHandExpression, but ++x is
    // not a LeftHandExpression.
  }
}

TEST_F(test_parse_expression, parse_template) {
  {
    test_parser p(u8"`hello`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 7);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"`hello${world}`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_template);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var world");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 15);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"`${one}${two}${three}`"_sv);
    EXPECT_EQ(summarize(ast), "template(var one, var two, var three)");
  }

  {
    expression* ast = this->parse_expression(u8"`hello${world}` + rhs"_sv);
    EXPECT_EQ(summarize(ast), "binary(template(var world), var rhs)");
  }
}

TEST_F(test_parse_expression, tagged_template_literal) {
  {
    test_parser p(u8"hello`world`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::tagged_template_literal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var hello");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 12);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"hello`template ${literal} thingy`"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::tagged_template_literal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var hello");
    EXPECT_EQ(summarize(ast->child(1)), "var literal");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 33);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"a.b()`c`"_sv);
    EXPECT_EQ(summarize(ast), "taggedtemplate(call(dot(var a, b)))");
  }

  {
    expression* ast = this->parse_expression(u8"tag`template`.property"_sv);
    EXPECT_EQ(summarize(ast), "dot(taggedtemplate(var tag), property)");
  }

  {
    expression* ast = this->parse_expression(u8"x + tag`template`"_sv);
    EXPECT_EQ(summarize(ast), "binary(var x, taggedtemplate(var tag))");
  }
}

TEST_F(test_parse_expression, optional_tagged_template_literal) {
  {
    expression* ast = this->parse_expression(u8"hello?.`world`"_sv);
    EXPECT_EQ(summarize(ast), "taggedtemplate(var hello)");
  }

  {
    expression* ast =
        this->parse_expression(u8"hello?.`template ${literal} thingy`"_sv);
    EXPECT_EQ(summarize(ast), "taggedtemplate(var hello, var literal)");
  }
}

TEST_F(test_parse_expression, array_literal) {
  {
    test_parser p(u8"[]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::array);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 2);
  }

  {
    expression* ast = this->parse_expression(u8"[x]"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::array);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
  }

  {
    expression* ast = this->parse_expression(u8"[x, y]"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::array);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_EQ(summarize(ast->child(1)), "var y");
  }

  {
    expression* ast = this->parse_expression(u8"[,,x,,y,,]"_sv);
    EXPECT_EQ(summarize(ast), "array(var x, var y)");
  }

  {
    // Comma should be parsed as an array separator, not as a comma operator.
    test_parser p(u8"[await myPromise,]"_sv);
    auto guard = p.parser().enter_function(function_attributes::async);
    expression* ast = p.parse_expression();
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(summarize(ast), "array(await(var myPromise))");
  }
}

TEST_F(test_parse_expression, malformed_array_literal) {
  {
    test_parser p(u8"[ "_sv);
    expression* ast = p.parse_expression();
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_missing_array_close, left_square,
                    offsets_matcher(p.code(), 0, u8"["),  //
                    expected_right_square,
                    offsets_matcher(p.code(), strlen(u8"["), u8""))));
    EXPECT_EQ(summarize(ast), "array()");
  }

  {
    test_parser p(u8"[ x "_sv);
    expression* ast = p.parse_expression();
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_missing_array_close, left_square,
                    offsets_matcher(p.code(), 0, u8"["),  //
                    expected_right_square,
                    offsets_matcher(p.code(), strlen(u8"[ x"), u8""))));
    EXPECT_EQ(summarize(ast), "array(var x)");
  }

  {
    test_parser p(u8"[\nif (true) {}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_missing_array_close, left_square,
                    offsets_matcher(p.code(), 0, u8"["),  //
                    expected_right_square,
                    offsets_matcher(p.code(), strlen(u8"["), u8""))));
    EXPECT_EQ(summarize(ast), "array()");
  }
}

TEST_F(test_parse_expression, object_literal) {
  {
    test_parser p(u8"{}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 0);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 2);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"{key: value}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
  }

  {
    expression* ast =
        this->parse_expression(u8"{key1: value1, key2: value2}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 2);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value1");
    EXPECT_EQ(summarize(ast->object_entry(1).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(1).value), "var value2");
  }

  {
    expression* ast = this->parse_expression(u8"{'key': value}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
  }

  {
    expression* ast = this->parse_expression(u8"{[key]: value}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var key");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "var value");
  }

  {
    test_parser p(u8"{thing}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    auto entry = ast->object_entry(0);
    EXPECT_EQ(summarize(entry.property), "literal");
    EXPECT_EQ(p.range(entry.property.value()).begin_offset(), 1);
    EXPECT_EQ(p.range(entry.property.value()).end_offset(), 6);
    EXPECT_EQ(summarize(entry.value), "var thing");
    EXPECT_EQ(p.range(entry.value).begin_offset(), 1);
    EXPECT_EQ(p.range(entry.value).end_offset(), 6);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast =
        this->parse_expression(u8"{key1: value1, thing2, key3: value3}"_sv);
    EXPECT_EQ(summarize(ast),
              "object(literal, var value1, literal, var thing2, literal, var "
              "value3)");
  }

  {
    expression* ast = this->parse_expression(u8"{key: variable = value}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value),
              "assign(var variable, var value)");
  }

  {
    expression* ast = this->parse_expression(u8"{key = value}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value),
              "assign(var key, var value)");
  }

  {
    expression* ast = this->parse_expression(u8"{...other, k: v}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 2);
    EXPECT_FALSE(ast->object_entry(0).property.has_value());
    EXPECT_EQ(summarize(ast->object_entry(0).value), "spread(var other)");
    EXPECT_EQ(summarize(ast->object_entry(1).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(1).value), "var v");
  }
}

TEST_F(test_parse_expression, object_literal_with_method_key) {
  {
    test_parser p(u8"{ func(a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_EQ(p.range(ast->object_entry(0).value).begin_offset(), 2);
    EXPECT_EQ(p.range(ast->object_entry(0).value).end_offset(), 16);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"{ 'func'(a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_EQ(p.range(ast->object_entry(0).value).begin_offset(), 2);
    EXPECT_EQ(p.range(ast->object_entry(0).value).end_offset(), 18);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"{ [func](a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_EQ(p.range(ast->object_entry(0).value).begin_offset(), 2);
    EXPECT_EQ(p.range(ast->object_entry(0).value).end_offset(), 18);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"{ async func(a, b) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_EQ(p.range(ast->object_entry(0).value).begin_offset(), 2);
    EXPECT_EQ(p.range(ast->object_entry(0).value).end_offset(), 22);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"{ async 'func'(a, b) { } }"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    expression* ast = this->parse_expression(u8"{ async [func](a, b) { } }"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    expression* ast = this->parse_expression(u8"{ *func(a, b) { } }"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    expression* ast = this->parse_expression(u8"{ *'func'(a, b) { } }"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    expression* ast = this->parse_expression(u8"{ *[func](a, b) { } }"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    expression* ast = this->parse_expression(u8"{ async *func(a, b) { } }"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    expression* ast =
        this->parse_expression(u8"{ async *'func'(a, b) { } }"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }

  {
    expression* ast =
        this->parse_expression(u8"{ async *[func](a, b) { } }"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "var func");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
  }
}

TEST_F(test_parse_expression, object_literal_with_getter_setter_key) {
  {
    test_parser p(u8"{ get prop() { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_EQ(p.range(ast->object_entry(0).value).begin_offset(), 2);
    EXPECT_EQ(p.range(ast->object_entry(0).value).end_offset(), 16);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"{ set prop(v) { } }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::object);
    EXPECT_EQ(ast->object_entry_count(), 1);
    EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
    EXPECT_EQ(summarize(ast->object_entry(0).value), "function");
    EXPECT_EQ(p.range(ast->object_entry(0).value).begin_offset(), 2);
    EXPECT_EQ(p.range(ast->object_entry(0).value).end_offset(), 17);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"{get 1234() { }}"_sv);
    EXPECT_EQ(summarize(ast), "object(literal, function)");
  }

  {
    expression* ast = this->parse_expression(u8"{get 'string key'() { }}"_sv);
    EXPECT_EQ(summarize(ast), "object(literal, function)");
  }

  {
    expression* ast =
        this->parse_expression(u8"{get [expression + key]() { }}"_sv);
    EXPECT_EQ(summarize(ast),
              "object(binary(var expression, var key), function)");
  }
}

TEST_F(test_parse_expression, object_literal_with_keyword_key) {
  for (string8 keyword : {u8"async", u8"catch", u8"class", u8"default",
                          u8"function", u8"get", u8"set", u8"try"}) {
    SCOPED_TRACE(out_string8(keyword));

    {
      string8 code = u8"{" + keyword + u8": null}";
      expression* ast = this->parse_expression(code.c_str());
      EXPECT_EQ(summarize(ast), "object(literal, literal)");
    }

    {
      string8 code = u8"{" + keyword + u8"() { }}";
      expression* ast = this->parse_expression(code.c_str());
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{get " + keyword + u8"() {}}";
      expression* ast = this->parse_expression(code.c_str());
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{set " + keyword + u8"() {}}";
      expression* ast = this->parse_expression(code.c_str());
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{async " + keyword + u8"() {}}";
      expression* ast = this->parse_expression(code.c_str());
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{*" + keyword + u8"() {}}";
      expression* ast = this->parse_expression(code.c_str());
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }
  }
}

TEST_F(test_parse_expression, object_literal_with_contextual_keyword_keyvalue) {
  for (string8 keyword : {u8"as", u8"async", u8"await", u8"from", u8"get",
                          u8"let", u8"of", u8"private", u8"protected",
                          u8"public", u8"set", u8"static", u8"yield"}) {
    SCOPED_TRACE(out_string8(keyword));

    {
      string8 code = u8"{" + keyword + u8"}";
      expression* ast = this->parse_expression(code.c_str());
      EXPECT_EQ(ast->kind(), expression_kind::object);
      EXPECT_EQ(ast->object_entry_count(), 1);
      EXPECT_EQ(summarize(ast->object_entry(0).property), "literal");
      EXPECT_EQ(ast->object_entry(0).value->kind(), expression_kind::variable);
      EXPECT_EQ(
          ast->object_entry(0).value->variable_identifier().normalized_name(),
          keyword);
    }

    {
      string8 code = u8"{" + keyword + u8", other}";
      expression* ast = this->parse_expression(code.c_str());
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
  for (string8 keyword : disallowed_binding_identifier_keywords) {
    SCOPED_TRACE(out_string8(keyword));

    {
      test_parser p(u8"{" + keyword + u8"}");
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal, ?)");
      EXPECT_THAT(p.errors(),
                  ElementsAre(ERROR_TYPE_FIELD(
                      error_missing_value_for_object_literal_entry, key,
                      offsets_matcher(p.code(), strlen(u8"{"), keyword))));
    }

    {
      test_parser p(u8"{" + keyword + u8", other}");
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal, ?, literal, var other)");
      EXPECT_THAT(p.errors(),
                  ElementsAre(ERROR_TYPE_FIELD(
                      error_missing_value_for_object_literal_entry, key,
                      offsets_matcher(p.code(), strlen(u8"{"), keyword))));
    }
  }
}

TEST_F(
    test_parse_expression,
    object_literal_with_reserved_keyword_keyvalue_with_unicode_escapes_is_an_error) {
  {
    test_parser p(u8"{ \\u{69}f }");
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, var if)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_keywords_cannot_contain_escape_sequences, escape_sequence,
            offsets_matcher(p.code(), strlen(u8"{ "), u8"\\u{69}"))));
  }
}

TEST_F(test_parse_expression, object_literal_with_number_key) {
  {
    expression* ast = this->parse_expression(u8"{1234: null}"_sv);
    EXPECT_EQ(summarize(ast), "object(literal, literal)");
  }

  {
    expression* ast = this->parse_expression(u8"{async 42() {}}"_sv);
    EXPECT_EQ(summarize(ast), "object(literal, function)");
  }

  {
    expression* ast = this->parse_expression(u8"{*42() {}}"_sv);
    EXPECT_EQ(summarize(ast), "object(literal, function)");
  }
}

TEST_F(test_parse_expression, incomplete_object_literal) {
  {
    test_parser p(u8"{ p1 "_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, var p1)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_unclosed_object_literal, object_open,
                    offsets_matcher(p.code(), 0, u8"{"),  //
                    expected_object_close,
                    offsets_matcher(p.code(), strlen(u8"{ p1"), u8""))));
  }

  {
    test_parser p(u8"{ p1, "_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, var p1)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_unclosed_object_literal, object_open,
                    offsets_matcher(p.code(), 0, u8"{"),  //
                    expected_object_close,
                    offsets_matcher(p.code(), strlen(u8"{ p1,"), u8""))));
  }

  {
    test_parser p(u8"({ p1, )"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, var p1)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_unclosed_object_literal, object_open,
                    offsets_matcher(p.code(), strlen(u8"("), u8"{"),  //
                    expected_object_close,
                    offsets_matcher(p.code(), strlen(u8"({ p1,"), u8""))));
  }

  {
    test_parser p(u8"[{ p1, ]"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "array(object(literal, var p1))");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_unclosed_object_literal, object_open,
                    offsets_matcher(p.code(), strlen(u8"["), u8"{"),  //
                    expected_object_close,
                    offsets_matcher(p.code(), strlen(u8"[{ p1,"), u8""))));
  }
}

TEST_F(test_parse_expression, malformed_object_literal) {
  {
    test_parser p(u8"{p1: v1 p2}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, var v1, literal, var p2)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_comma_between_object_literal_entries, where,
                    offsets_matcher(p.code(), strlen(u8"{p1: v1"), u8""))));
  }

  {
    test_parser p(u8"{1234}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, ?)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_invalid_lone_literal_in_object_literal, where,
                    offsets_matcher(p.code(), strlen(u8"{"), u8"1234"))));
  }

  {
    test_parser p(u8"{'x'}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, ?)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_invalid_lone_literal_in_object_literal, where,
                    offsets_matcher(p.code(), strlen(u8"{"), u8"'x'"))));
  }

  {
    test_parser p(u8"{a b: c}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, var a, literal, var c)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_comma_between_object_literal_entries, where,
                    offsets_matcher(p.code(), strlen(u8"{a"), u8""))));
  }

  {
    test_parser p(u8"{a *generator() {}}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, var a, literal, function)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_comma_between_object_literal_entries, where,
                    offsets_matcher(p.code(), strlen(u8"{a"), u8""))));
  }

  {
    test_parser p(u8"{async f}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, function)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_function_parameter_list, function_name,
                    offsets_matcher(p.code(), strlen(u8"{async "), u8"f"))));
  }

  {
    test_parser p(u8"{*f}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, function)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_function_parameter_list, function_name,
                    offsets_matcher(p.code(), strlen(u8"{*"), u8"f"))));
  }

  {
    test_parser p(u8"{function a(){}}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, function)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_methods_should_not_use_function_keyword, function_token,
            offsets_matcher(p.code(), strlen(u8"{"), u8"function"))));
  }

  {
    test_parser p(u8"{async function a(){}}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, function)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_methods_should_not_use_function_keyword, function_token,
            offsets_matcher(p.code(), strlen(u8"{async "), u8"function"))));
  }

  {
    test_parser p(u8"{function *a(){}}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, function)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_methods_should_not_use_function_keyword, function_token,
            offsets_matcher(p.code(), strlen(u8"{"), u8"function"))));
  }

  {
    test_parser p(u8"{ [x] }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var x, ?)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_value_for_object_literal_entry, key,
                    offsets_matcher(p.code(), strlen(u8"{ "), u8"[x]"))));
  }

  {
    test_parser p(u8"{ [x], other }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var x, ?, literal, var other)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_missing_value_for_object_literal_entry, key,
                    offsets_matcher(p.code(), strlen(u8"{ "), u8"[x]"))));
  }

  for (string8 op : {
           u8"!=",   u8"!==", u8"%",   u8"%=",     u8"&",  u8"&&",  u8"&&=",
           u8"&=",   u8"*",   u8"**",  u8"**=",    u8"*=", u8"+",   u8"+=",
           u8"-",    u8"-=",  u8".",   u8"/=",     u8"<<", u8"<<=", u8"<=",
           u8"==",   u8"===", u8">",   u8">=",     u8">>", u8">>=", u8">>>",
           u8">>>=", u8"?.",  u8"??",  u8"?\x3f=", u8"^",  u8"^=",  u8"|",
           u8"|=",   u8"||",  u8"||=",
       }) {
    string8 code = u8"{one " + op + u8" two}";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code);
    expression* ast = p.parse_expression();
    EXPECT_THAT(
        summarize(ast),
        ::testing::AnyOf("object(literal, binary(var one, var two))",
                         "object(literal, condassign(var one, var two))",
                         "object(literal, dot(var one, two))",
                         "object(literal, upassign(var one, var two))"));
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_key_for_object_entry, expression,
                                offsets_matcher(p.code(), strlen(u8"{"),
                                                u8"one " + op + u8" two"))));
  }

  for (string8 op : {
           u8"!=", u8"!==", u8"%",   u8"&",  u8"&&", u8"*",  u8"**",  u8"+",
           u8"-",  u8".",   u8"<<",  u8"<=", u8"=",  u8"==", u8"===", u8">",
           u8">=", u8">>",  u8">>>", u8"?.", u8"??", u8"^",  u8"|",   u8"||",
       }) {
    {
      string8 code = u8"{'one' " + op + u8" two}";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code);
      expression* ast = p.parse_expression();
      EXPECT_THAT(summarize(ast),
                  ::testing::AnyOf("object(literal, assign(literal, var two))",
                                   "object(literal, binary(literal, var two))",
                                   "object(literal, dot(literal, two))"));
      EXPECT_THAT(p.errors(),
                  ElementsAre(ERROR_TYPE_FIELD(
                      error_missing_key_for_object_entry, expression,
                      offsets_matcher(p.code(), strlen(u8"{"),
                                      u8"'one' " + op + u8" two"))));
    }

    {
      string8 code = u8"{1234 " + op + u8" two}";
      SCOPED_TRACE(out_string8(code));
      test_parser p(code);
      expression* ast = p.parse_expression();
      EXPECT_THAT(summarize(ast),
                  ::testing::AnyOf("object(literal, assign(literal, var two))",
                                   "object(literal, binary(literal, var two))",
                                   "object(literal, dot(literal, two))"));
      EXPECT_THAT(p.errors(),
                  ElementsAre(ERROR_TYPE_FIELD(
                      error_missing_key_for_object_entry, expression,
                      offsets_matcher(p.code(), strlen(u8"{"),
                                      u8"1234 " + op + u8" two"))));
    }
  }

  for (string8 op : {u8"++", u8"--"}) {
    string8 code = u8"{one " + op + u8" two}";
    SCOPED_TRACE(out_string8(code));
    test_parser p(code);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal, rwunarysuffix(var one), literal, var two)");
    EXPECT_THAT(
        p.errors(),
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_missing_key_for_object_entry, expression,
                offsets_matcher(p.code(), strlen(u8"{"), u8"one " + op)),
            // TODO(strager): Don't report
            // error_missing_comma_between_object_literal_entries.
            ::testing::VariantWith<
                error_missing_comma_between_object_literal_entries>(_)));
  }

  {
    test_parser p(u8"{#key: value}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, var value)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_private_properties_are_not_allowed_in_object_literals,
                    private_identifier,
                    offsets_matcher(p.code(), strlen(u8"{"), u8"#key"))));
  }

  for (string8 prefix :
       {u8"", u8"async ", u8"get ", u8"set ", u8"*", u8"async *"}) {
    padded_string code(u8"{ " + prefix + u8"#method() { } }");
    SCOPED_TRACE(code);
    test_parser p(code.string_view());
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, function)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_private_properties_are_not_allowed_in_object_literals,
            private_identifier,
            offsets_matcher(p.code(), (u8"{ " + prefix).size(), u8"#method"))));
  }
}

// In some other languages, ';' separates entries similar to how ',' does in
// JavaScript.
TEST_F(test_parse_expression,
       object_literal_entries_are_not_separated_by_semicolon) {
  {
    test_parser p(u8"{ key: value; other: second }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal, var value, literal, var second)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_expected_comma_to_separate_object_literal_entries,
            unexpected_token,
            offsets_matcher(p.code(), strlen(u8"{ key: value"), u8";"))));
  }

  {
    test_parser p(u8"{ first; get; set; async; }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal, var first, literal, var get, "
              "literal, var set, literal, var async)");
    EXPECT_THAT(
        p.errors(),
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_expected_comma_to_separate_object_literal_entries,
                unexpected_token,
                offsets_matcher(p.code(), strlen(u8"{ first"), u8";")),
            ERROR_TYPE_FIELD(
                error_expected_comma_to_separate_object_literal_entries,
                unexpected_token,
                offsets_matcher(p.code(), strlen(u8"{ first; get"), u8";")),
            ERROR_TYPE_FIELD(
                error_expected_comma_to_separate_object_literal_entries,
                unexpected_token,
                offsets_matcher(p.code(), strlen(u8"{ first; get; set"),
                                u8";")),
            ERROR_TYPE_FIELD(
                error_expected_comma_to_separate_object_literal_entries,
                unexpected_token,
                offsets_matcher(p.code(), strlen(u8"{ first; get; set; async"),
                                u8";"))));
  }

  {
    test_parser p(u8"{ [key]; other }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var key, ?, literal, var other)");
    EXPECT_THAT(p.errors(),
                UnorderedElementsAre(
                    ERROR_TYPE_FIELD(
                        error_expected_comma_to_separate_object_literal_entries,
                        unexpected_token,
                        offsets_matcher(p.code(), strlen(u8"{ [key]"), u8";")),
                    ERROR_TYPE_FIELD(
                        error_missing_value_for_object_literal_entry, key,
                        offsets_matcher(p.code(), strlen(u8"{ "), u8"[key]"))));
  }
}

// On some keyboards, '<' is input by pressing ',' while holding the SHIFT key.
TEST_F(test_parse_expression,
       object_literal_entries_are_not_separated_by_less_than_symbol) {
  {
    test_parser p(u8"{ first< get< set< async< }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "object(literal, var first, literal, var get, "
              "literal, var set, literal, var async)");
    EXPECT_THAT(
        p.errors(),
        UnorderedElementsAre(
            ERROR_TYPE_FIELD(
                error_expected_comma_to_separate_object_literal_entries,
                unexpected_token,
                offsets_matcher(p.code(), strlen(u8"{ first"), u8"<")),
            ERROR_TYPE_FIELD(
                error_expected_comma_to_separate_object_literal_entries,
                unexpected_token,
                offsets_matcher(p.code(), strlen(u8"{ first< get"), u8"<")),
            ERROR_TYPE_FIELD(
                error_expected_comma_to_separate_object_literal_entries,
                unexpected_token,
                offsets_matcher(p.code(), strlen(u8"{ first< get< set"),
                                u8"<")),
            ERROR_TYPE_FIELD(
                error_expected_comma_to_separate_object_literal_entries,
                unexpected_token,
                offsets_matcher(p.code(), strlen(u8"{ first< get< set< async"),
                                u8"<"))));
  }

  {
    test_parser p(u8"{ [key]< other }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var key, ?, literal, var other)");
    EXPECT_THAT(p.errors(),
                UnorderedElementsAre(
                    ERROR_TYPE_FIELD(
                        error_expected_comma_to_separate_object_literal_entries,
                        unexpected_token,
                        offsets_matcher(p.code(), strlen(u8"{ [key]"), u8"<")),
                    ERROR_TYPE_FIELD(
                        error_missing_value_for_object_literal_entry, key,
                        offsets_matcher(p.code(), strlen(u8"{ "), u8"[key]"))));
  }

  {
    test_parser p(u8"{ method() {}< other }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, function, literal, var other)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_expected_comma_to_separate_object_literal_entries,
            unexpected_token,
            offsets_matcher(p.code(), strlen(u8"{ method() {}"), u8"<"))));
  }
}

TEST(test_parse, object_literal_generator_method_with_misplaced_star) {
  {
    test_parser p(u8"{method*() { yield 42; }}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(literal, function)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_generator_function_star_belongs_before_name, function_name,
            offsets_matcher(p.code(), strlen(u8"{"), u8"method"),  //
            star, offsets_matcher(p.code(), strlen(u8"{method"), u8"*"))));
  }

  {
    test_parser p(u8"{ [computed] *() { yield 42; }}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "object(var computed, function)");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_generator_function_star_belongs_before_name, function_name,
            offsets_matcher(p.code(), strlen(u8"{ "), u8"[computed]"),  //
            star,
            offsets_matcher(p.code(), strlen(u8"{ [computed] "), u8"*"))));
  }
}

TEST_F(test_parse_expression, parse_comma_expression) {
  {
    test_parser p(u8"x,y,z"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::binary_operator);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_EQ(summarize(ast->child(1)), "var y");
    EXPECT_EQ(summarize(ast->child(2)), "var z");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 5);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"(x+(y,z)+w)"_sv);
    EXPECT_EQ(summarize(ast), "binary(var x, binary(var y, var z), var w)");
  }

  {
    expression* ast = this->parse_expression(u8"`${2+2, four}`"_sv);
    EXPECT_EQ(summarize(ast), "template(binary(literal, literal, var four))");
  }

  {
    expression* ast = this->parse_expression(u8"i = 0, j = 0"_sv);
    EXPECT_EQ(summarize(ast),
              "binary(assign(var i, literal), assign(var j, literal))");
  }
}

TEST_F(test_parse_expression, parse_function_expression) {
  {
    test_parser p(u8"function(){} /* */"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 12);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"function(x, y){}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::function);
  }

  {
    expression* ast = this->parse_expression(u8"function(){}()"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(ast->child_0()->kind(), expression_kind::function);
  }

  {
    expression* ast = this->parse_expression(u8"function f(){}"_sv);
    EXPECT_EQ(ast->kind(), expression_kind::named_function);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->variable_identifier().normalized_name(), u8"f");
  }
}

TEST_F(test_parse_expression, function_with_destructuring_parameters) {
  {
    expression* ast = this->parse_expression(u8"function({a, b}) { c }"_sv);
    EXPECT_EQ(summarize(ast), "function");
  }

  {
    expression* ast = this->parse_expression(u8"function([a, b]) { c }"_sv);
    EXPECT_EQ(summarize(ast), "function");
  }
}

TEST_F(test_parse_expression, function_with_spread_and_comma) {
  {
    test_parser p(u8"function(...a, ) { b; }"_sv);
    p.parse_expression();
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_comma_not_allowed_after_spread_parameter, comma,
            offsets_matcher(p.code(), strlen(u8"function(...a"), u8","),  //
            spread,
            offsets_matcher(p.code(), strlen(u8"function("), u8"...a"))));
  }
}

TEST_F(test_parse_expression, async_function_expression) {
  {
    test_parser p(u8"async function(){}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 18);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"async function f(){}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::named_function);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 20);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, generator_function_expression) {
  {
    test_parser p(u8"function*(){}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(ast->attributes(), function_attributes::generator);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 13);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"function* f(){}"_sv);
    EXPECT_EQ(summarize(ast), "function f");
  }
}

TEST_F(test_parse_expression, async_generator_function_expression) {
  {
    test_parser p(u8"async function*(){}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(ast->attributes(), function_attributes::async_generator);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 19);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"async function* f(){}"_sv);
    EXPECT_EQ(summarize(ast), "function f");
  }
}

TEST_F(test_parse_expression, arrow_function_with_expression) {
  {
    test_parser p(u8"() => a"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var a");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 7);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"a => b"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_EQ(summarize(ast->child(1)), "var b");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 6);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"(a) => b"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_EQ(summarize(ast->child(1)), "var b");
    // TODO(strager): Implement begin_offset.
    EXPECT_EQ(p.range(ast).end_offset(), 8);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"(a, b) => c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 3);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_EQ(summarize(ast->child(1)), "var b");
    EXPECT_EQ(summarize(ast->child(2)), "var c");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"() => a, b"_sv);
    EXPECT_EQ(summarize(ast), "binary(arrowexpr(var a), var b)");
  }

  {
    expression* ast = this->parse_expression(u8"a => b, c"_sv);
    EXPECT_EQ(summarize(ast), "binary(arrowexpr(var a, var b), var c)");
  }

  {
    expression* ast = this->parse_expression(u8"(a,) => b"_sv);
    EXPECT_EQ(summarize(ast), "arrowexpr(var a, var b)");
  }

  {
    expression* ast = this->parse_expression(u8"async => value"_sv);
    EXPECT_EQ(summarize(ast), "arrowexpr(var async, var value)");
  }
}

TEST_F(test_parse_expression, arrow_function_with_statements) {
  {
    test_parser p(u8"() => { a; }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_statements);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 12);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"a => { b; } /* */"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_statements);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 11);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, arrow_function_with_spread_and_comma) {
  {
    test_parser p(u8"(...b, ) => { c; }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_comma_not_allowed_after_spread_parameter, comma,
            offsets_matcher(p.code(), strlen(u8"(...b"), u8","),  //
            spread, offsets_matcher(p.code(), strlen(u8"("), u8"...b"))));
    EXPECT_EQ(summarize(ast), "arrowblock(spread(var b))");
  }
}

TEST_F(test_parse_expression, arrow_function_with_destructuring_parameters) {
  {
    test_parser p(u8"({a, b}) => c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)),
              "object(literal, var a, literal, var b)");
    EXPECT_EQ(summarize(ast->child(1)), "var c");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"([a, b]) => c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "array(var a, var b)");
    EXPECT_EQ(summarize(ast->child(1)), "var c");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"(...args) => null"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::normal);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "spread(var args)");
    EXPECT_EQ(summarize(ast->child(1)), "literal");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, async_arrow_function) {
  {
    test_parser p(u8"async () => { a; }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_statements);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(ast->child_count(), 0);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 18);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"async x => { y; }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_statements);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"async (x, y, z) => { w; }"_sv);
    EXPECT_EQ(summarize(ast), "asyncarrowblock(var x, var y, var z)");
  }

  {
    test_parser p(u8"async () => a"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var a");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 13);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"async x => y"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::arrow_function_with_expression);
    EXPECT_EQ(ast->attributes(), function_attributes::async);
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_EQ(summarize(ast->child(1)), "var y");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    expression* ast = this->parse_expression(u8"async (x, y, z) => w"_sv);
    EXPECT_EQ(summarize(ast), "asyncarrowexpr(var x, var y, var z, var w)");
  }

  {
    expression* ast = this->parse_expression(u8"async (a,) => b"_sv);
    EXPECT_EQ(summarize(ast), "asyncarrowexpr(var a, var b)");
  }
}

TEST_F(test_parse_expression, invalid_arrow_function) {
  {
    test_parser p(u8"a() => b"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, arrowexpr(var b))");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_operator_between_expression_and_arrow_function, where,
            offsets_matcher(p.code(), 0, u8"a("))));
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 0 + strlen(u8"a() => b"));
  }

  {
    test_parser p(u8"a(b) => c"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, arrowexpr(var b, var c))");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_FIELD(
            error_missing_operator_between_expression_and_arrow_function, where,
            offsets_matcher(p.code(), 0, u8"a("))));
  }

  {
    test_parser p(u8"=> a"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowexpr(var a)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_arrow_function_parameter_list,
                                arrow, offsets_matcher(p.code(), 0, u8"=>"))));
  }

  {
    test_parser p(u8"=> { body; }"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowblock()");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_arrow_function_parameter_list,
                                arrow, offsets_matcher(p.code(), 0, u8"=>"))));
  }

  {
    test_parser p(u8"=> { body; }, other"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(arrowblock(), var other)");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_arrow_function_parameter_list,
                                arrow, offsets_matcher(p.code(), 0, u8"=>"))));
  }

  {
    test_parser p(u8"42 => body"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, var body)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_unexpected_arrow_after_literal, arrow,
                    offsets_matcher(p.code(), strlen(u8"42 "), u8"=>"),  //
                    literal_parameter, offsets_matcher(p.code(), 0, u8"42"))));
  }

  {
    test_parser p(u8"42 => {body();}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowblock()");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_unexpected_arrow_after_literal, arrow,
                    offsets_matcher(p.code(), strlen(u8"42 "), u8"=>"),  //
                    literal_parameter, offsets_matcher(p.code(), 0, u8"42"))));
  }

  {
    test_parser p(u8"x.p => rhs"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(dot(var x, p), var rhs)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_unexpected_arrow_after_expression, arrow,
                    offsets_matcher(p.code(), strlen(u8"x.p "), u8"=>"),  //
                    expression, offsets_matcher(p.code(), 0, u8"x.p"))));
  }

  {
    test_parser p(u8"x.p => {body();}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowblock()");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    error_unexpected_arrow_after_expression, arrow,
                    offsets_matcher(p.code(), strlen(u8"x.p "), u8"=>"),  //
                    expression, offsets_matcher(p.code(), 0, u8"x.p"))));
  }

  {
    test_parser p(u8"(x, 42, y) => {body();}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "arrowblock(var x, var y)");
    EXPECT_THAT(p.errors(),
                ElementsAre(ERROR_TYPE_FIELD(
                    error_unexpected_literal_in_parameter_list, literal,
                    offsets_matcher(p.code(), strlen(u8"(x, "), u8"42"))));
  }
}

TEST_F(test_parse_expression, invalid_parentheses) {
  {
    test_parser p(u8"()"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "?");
    EXPECT_THAT(
        p.errors(),
        ElementsAre(ERROR_TYPE_2_FIELDS(
            error_missing_expression_between_parentheses, left_paren,
            offsets_matcher(p.code(), 0, u8"("),  //
            right_paren, offsets_matcher(p.code(), strlen(u8"("), u8")"))));
  }
}

TEST_F(test_parse_expression, invalid_keyword_in_expression) {
  {
    test_parser p(u8"debugger"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "?");
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_unexpected_token, token,
                                offsets_matcher(p.code(), 0, u8"debugger"))));
  }
}

TEST_F(test_parse_expression, anonymous_class) {
  {
    test_parser p(u8"class {}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_class);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 8);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p(u8"class C {}"_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_class);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 10);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST_F(test_parse_expression, class_requires_a_body) {
  {
    test_parser p(u8"class C "_sv);
    expression* ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "class");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), strlen(u8"class C"));
    EXPECT_THAT(p.errors(), ElementsAre(ERROR_TYPE_FIELD(
                                error_missing_body_for_class,
                                class_keyword_and_name_and_heritage,
                                offsets_matcher(p.code(), 0, u8"class C"))));
  }
}

TEST_F(test_parse_expression, parse_mixed_expression) {
  {
    expression* ast = this->parse_expression(u8"a+f()"_sv);
    EXPECT_EQ(summarize(ast), "binary(var a, call(var f))");
  }

  {
    expression* ast = this->parse_expression(u8"a+f(x+y,-z-w)+b"_sv);
    EXPECT_EQ(summarize(ast),
              "binary(var a, call(var f, binary(var x, var y), "
              "binary(unary(var z), var w)), var b)");
  }

  {
    expression* ast = this->parse_expression(u8"(x+y).z"_sv);
    EXPECT_EQ(summarize(ast), "dot(binary(var x, var y), z)");
  }

  {
    expression* ast = this->parse_expression(u8"/hello/.test(string)"_sv);
    EXPECT_EQ(summarize(ast), "call(dot(literal, test), var string)");
  }

  {
    expression* ast = this->parse_expression(u8"!/hello/.test(string)"_sv);
    EXPECT_EQ(summarize(ast), "unary(call(dot(literal, test), var string))");
  }

  {
    expression* ast = this->parse_expression(u8"{a: new A(), b: new B()}"_sv);
    EXPECT_EQ(summarize(ast),
              "object(literal, new(var A), literal, new(var B))");
  }

  {
    expression* ast =
        this->parse_expression(u8"o && typeof o === 'object' ? o[k] : null"_sv);
    if (false) {  // TODO(strager): Check AST.
      EXPECT_EQ(summarize(ast),
                "cond(binary(var o, binary(typeof(var o), literal)), "
                "index(var o, var k), literal)");
    }
  }

  {
    expression* ast = this->parse_expression(u8"!!o && k in o"_sv);
    EXPECT_EQ(summarize(ast), "binary(unary(unary(var o)), var k, var o)");
  }

  {
    expression* ast = this->parse_expression(u8"x --> 0"_sv);
    EXPECT_EQ(summarize(ast), "binary(rwunarysuffix(var x), literal)");
  }

  {
    expression* ast = this->parse_expression(u8"class {} + 42"_sv);
    EXPECT_EQ(summarize(ast), "binary(class, literal)");
  }

  {
    expression* ast = this->parse_expression(u8"other + async(a)"_sv);
    EXPECT_EQ(summarize(ast), "binary(var other, call(var async, var a))");
  }

  {
    expression* ast = this->parse_expression(u8"left + async() + right"_sv);
    EXPECT_EQ(summarize(ast), "binary(var left, call(var async), var right)");
  }

  {
    expression* ast = this->parse_expression(u8"left + async + right"_sv);
    EXPECT_EQ(summarize(ast), "binary(var left, var async, var right)");
  }
}

TEST_F(test_parse_expression,
       reserved_keywords_for_object_properties_can_contain_escape_sequences) {
  for (string8 keyword : reserved_keywords) {
    string8 property = escape_first_character_in_keyword(keyword);

    {
      string8 code = u8"obj." + property;
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(ast->kind(), expression_kind::dot);
      EXPECT_EQ(summarize(ast->child_0()), "var obj");
      EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
    }

    {
      string8 code = u8"obj?." + property;
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(ast->kind(), expression_kind::dot);
      EXPECT_EQ(summarize(ast->child_0()), "var obj");
      EXPECT_EQ(ast->variable_identifier().normalized_name(), keyword);
    }

    {
      string8 code = u8"{ " + property + u8": value }";
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(summarize(ast), "object(literal, var value)");
    }

    {
      string8 code = u8"{ " + property + u8"() {} }";
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{ get " + property + u8"() {} }";
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{ set " + property + u8"(v) {} }";
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{ async " + property + u8"() {} }";
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{ *" + property + u8"() {} }";
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      string8 code = u8"{ async *" + property + u8"() {} }";
      SCOPED_TRACE(out_string8(code));
      expression* ast = this->parse_expression(code);
      EXPECT_EQ(summarize(ast), "object(literal, function)");
    }

    {
      test_parser p(u8"{ function *" + property + u8"() {} }");
      expression* ast = p.parse_expression();
      EXPECT_EQ(summarize(ast), "object(literal, function)");
      EXPECT_THAT(
          p.errors(),
          ElementsAre(::testing::VariantWith<
                      error_methods_should_not_use_function_keyword>(_)));
    }
  }
}

std::string summarize(const expression& expression) {
  auto children = [&] {
    std::string result;
    bool need_comma = false;
    for (int i = 0; i < expression.child_count(); ++i) {
      if (need_comma) {
        result += ", ";
      }
      result += summarize(expression.child(i));
      need_comma = true;
    }
    return result;
  };
  auto function_attributes = [&]() -> std::string {
    switch (expression.attributes()) {
    case function_attributes::normal:
      return "";
    case function_attributes::async:
      return "async";
    case function_attributes::async_generator:
      return "asyncgenerator";
    case function_attributes::generator:
      return "generator";
    }
    QLJS_UNREACHABLE();
  };
  switch (expression.kind()) {
  case expression_kind::_class:
    return "class";
  case expression_kind::_invalid:
    return "?";
  case expression_kind::_new:
    return "new(" + children() + ")";
  case expression_kind::_template:
    return "template(" + children() + ")";
  case expression_kind::_typeof:
    return "typeof(" + summarize(expression.child_0()) + ")";
  case expression_kind::array:
    return "array(" + children() + ")";
  case expression_kind::arrow_function_with_expression:
    return function_attributes() + "arrowexpr(" + children() + ")";
  case expression_kind::arrow_function_with_statements:
    return function_attributes() + "arrowblock(" + children() + ")";
  case expression_kind::assignment:
    return "assign(" + children() + ")";
  case expression_kind::await:
    return "await(" + summarize(expression.child_0()) + ")";
  case expression_kind::call:
    return "call(" + children() + ")";
  case expression_kind::conditional:
    return "cond(" + summarize(expression.child_0()) + ", " +
           summarize(expression.child_1()) + ", " +
           summarize(expression.child_2()) + ")";
  case expression_kind::dot:
    return "dot(" + summarize(expression.child_0()) + ", " +
           string8_to_string(
               expression.variable_identifier().normalized_name()) +
           ")";
  case expression_kind::function:
    return "function";
  case expression_kind::import:
    return "import";
  case expression_kind::index:
    return "index(" + children() + ")";
  case expression_kind::literal:
    return "literal";
  case expression_kind::named_function:
    return "function " +
           string8_to_string(
               expression.variable_identifier().normalized_name());
  case expression_kind::new_target:
    return "newtarget";
  case expression_kind::object: {
    std::string result = "object(";
    bool need_comma = false;
    for (int i = 0; i < expression.object_entry_count(); ++i) {
      if (need_comma) {
        result += ", ";
      }
      auto entry = expression.object_entry(i);
      result += summarize(entry.property);
      result += ", ";
      result += summarize(entry.value);
      need_comma = true;
    }
    result += ")";
    return result;
  }
  case expression_kind::private_variable:
    return "var " + string8_to_string(
                        expression.variable_identifier().normalized_name());
  case expression_kind::rw_unary_prefix:
    return "rwunary(" + summarize(expression.child_0()) + ")";
  case expression_kind::rw_unary_suffix:
    return "rwunarysuffix(" + summarize(expression.child_0()) + ")";
  case expression_kind::spread:
    return "spread(" + summarize(expression.child_0()) + ")";
  case expression_kind::super:
    return "super";
  case expression_kind::tagged_template_literal:
    return "taggedtemplate(" + children() + ")";
  case expression_kind::trailing_comma:
    return "trailingcomma(" + children() + ")";
  case expression_kind::unary_operator:
    return "unary(" + summarize(expression.child_0()) + ")";
  case expression_kind::compound_assignment:
    return "upassign(" + children() + ")";
  case expression_kind::conditional_assignment:
    return "condassign(" + children() + ")";
  case expression_kind::variable:
    return "var " + string8_to_string(
                        expression.variable_identifier().normalized_name());
  case expression_kind::binary_operator:
    return "binary(" + children() + ")";
  case expression_kind::yield_many:
    return "yieldmany(" + summarize(expression.child_0()) + ")";
  case expression_kind::yield_none:
    return "yieldnone";
  case expression_kind::yield_one:
    return "yield(" + summarize(expression.child_0()) + ")";
  }
  QLJS_UNREACHABLE();
}

std::string summarize(expression* expression) { return summarize(*expression); }

std::string summarize(std::optional<expression*> expression) {
  if (expression.has_value()) {
    return summarize(*expression);
  } else {
    return "(null)";
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
std::string string8_to_string(string8_view sv) {
  return std::string(
      std::string_view(reinterpret_cast<const char*>(sv.data()), sv.size()));
}
QLJS_WARNING_POP
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
