// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/unreachable.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
std::string summarize(const expression &);
std::string summarize(expression_ptr);

class test_parser {
 public:
  explicit test_parser(const char *input) : parser_(input, &errors_) {}

  expression_ptr parse_expression() { return this->parser_.parse_expression(); }

  const std::vector<error_collector::error> &errors() const noexcept {
    return this->errors_.errors;
  }

  source_range error_range(int error_index) {
    return this->parser_.locator().range(this->errors().at(error_index).where);
  }

  source_range range(expression_ptr ast) {
    return this->parser_.locator().range(ast->span());
  }

  quick_lint_js::lexer &lexer() noexcept { return this->parser_.lexer(); }

 private:
  error_collector errors_;
  quick_lint_js::parser parser_;
};

TEST(test_parse_expression, parse_single_token_expression) {
  {
    test_parser p("x");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::variable);
    EXPECT_EQ(ast->variable_identifier().string_view(), "x");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 1);
  }

  {
    test_parser p("42");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 2);
  }

  {
    test_parser p("'hello'");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 7);
  }

  {
    test_parser p("null");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 4);
  }

  {
    test_parser p("this");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 4);
  }
}

TEST(test_parse_expression, parse_math_expression) {
  {
    test_parser p("-x");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::unary_operator);
    EXPECT_EQ(ast->child_0()->kind(), expression_kind::variable);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 2);
  }

  {
    test_parser p("+x");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "unary(var x)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("x+y");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y)");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
  }

  {
    test_parser p("x+y-z");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y, var z)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("2-4+1");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal, literal)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("-x+y");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(unary(var x), var y)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  for (const char *input : {"2+2", "2-2", "2*2", "2/2", "2%2", "2**2", "2^2",
                            "2&2", "2|2", "2<<2", "2>>2", "2>>>2"}) {
    SCOPED_TRACE("input = " + std::string(input));
    test_parser p(input);
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_broken_math_expression) {
  {
    test_parser p("2+");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, ?)");
    ASSERT_EQ(p.errors().size(), 1);
    EXPECT_EQ(p.errors()[0].kind,
              error_collector::error_missing_operand_for_operator);
    EXPECT_EQ(p.error_range(0).begin_offset(), 1);
    EXPECT_EQ(p.error_range(0).end_offset(), 2);
  }

  {
    test_parser p("^2");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(?, literal)");
    ASSERT_EQ(p.errors().size(), 1);
    EXPECT_EQ(p.errors()[0].kind,
              error_collector::error_missing_operand_for_operator);
    EXPECT_EQ(p.error_range(0).begin_offset(), 0);
    EXPECT_EQ(p.error_range(0).end_offset(), 1);
  }

  {
    test_parser p("2 * * 2");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, ?, literal)");
    ASSERT_EQ(p.errors().size(), 1);
    EXPECT_EQ(p.errors()[0].kind,
              error_collector::error_missing_operand_for_operator);
    EXPECT_EQ(p.error_range(0).begin_offset(), 2);
    EXPECT_EQ(p.error_range(0).end_offset(), 3);
  }

  {
    test_parser p("2 & & & 2");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, ?, ?, literal)");
    ASSERT_EQ(p.errors().size(), 2);

    EXPECT_EQ(p.errors()[0].kind,
              error_collector::error_missing_operand_for_operator);
    EXPECT_EQ(p.error_range(0).begin_offset(), 2);
    EXPECT_EQ(p.error_range(0).end_offset(), 3);

    EXPECT_EQ(p.errors()[1].kind,
              error_collector::error_missing_operand_for_operator);
    EXPECT_EQ(p.error_range(1).begin_offset(), 4);
    EXPECT_EQ(p.error_range(1).end_offset(), 5);
  }

  {
    test_parser p("(2*)");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, ?)");
    ASSERT_EQ(p.errors().size(), 1);
    EXPECT_EQ(p.errors()[0].kind,
              error_collector::error_missing_operand_for_operator);
    EXPECT_EQ(p.error_range(0).begin_offset(), 2);
    EXPECT_EQ(p.error_range(0).end_offset(), 3);
  }

  {
    test_parser p("2 * (3 + 4");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, binary(literal, literal))");
    ASSERT_EQ(p.errors().size(), 1);
    EXPECT_EQ(p.errors()[0].kind, error_collector::error_unmatched_parenthesis);
    EXPECT_EQ(p.error_range(0).begin_offset(), 4);
    EXPECT_EQ(p.error_range(0).end_offset(), 5);
  }

  {
    test_parser p("2 * (3 + (4");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, binary(literal, literal))");
    ASSERT_EQ(p.errors().size(), 2);

    EXPECT_EQ(p.errors()[0].kind, error_collector::error_unmatched_parenthesis);
    EXPECT_EQ(p.error_range(0).begin_offset(), 9);
    EXPECT_EQ(p.error_range(0).end_offset(), 10);

    EXPECT_EQ(p.errors()[1].kind, error_collector::error_unmatched_parenthesis);
    EXPECT_EQ(p.error_range(1).begin_offset(), 4);
    EXPECT_EQ(p.error_range(1).end_offset(), 5);
  }
}

TEST(test_parse_expression, parse_logical_expression) {
  for (const char *input : {"2==2", "2===2", "2!=2", "2!==2", "2>2", "2<2",
                            "2>=2", "2<=2", "2&&2", "2||2"}) {
    SCOPED_TRACE("input = " + std::string(input));
    test_parser p(input);
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(literal, literal)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_keyword_binary_operators) {
  {
    test_parser p("prop in object");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var prop, var object)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("object instanceof Class");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var object, var Class)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_function_call) {
  {
    test_parser p("f()");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
  }

  {
    test_parser p("f(x)");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 2);
    EXPECT_EQ(summarize(ast->child(1)), "var x");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("f(x,y)");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(summarize(ast->child_0()), "var f");
    EXPECT_EQ(ast->child_count(), 3);
    EXPECT_EQ(summarize(ast->child(1)), "var x");
    EXPECT_EQ(summarize(ast->child(2)), "var y");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_dot_expressions) {
  {
    test_parser p("x.prop");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::dot);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(ast->variable_identifier().string_view(), "prop");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 6);
  }

  {
    test_parser p("x.p1.p2");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(dot(var x, p1), p2)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_parenthesized_expression) {
  {
    test_parser p("(x)");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "var x");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 1);
    EXPECT_EQ(p.range(ast).end_offset(), 2);
  }

  {
    test_parser p("x+(y)");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, var y)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("x+(y+z)");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, binary(var y, var z))");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("(x+y)+z");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(binary(var x, var y), var z)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("x+(y+z)+w");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, binary(var y, var z), var w)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_await_expression) {
  {
    test_parser p("await myPromise");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::await);
    EXPECT_EQ(summarize(ast->child_0()), "var myPromise");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 15);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_new_expression) {
  {
    test_parser p("new Date");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_new);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var Date");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 8);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("new Date()");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_new);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child_0()), "var Date");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 10);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("new Date(y,m,d)");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "new(var Date, var y, var m, var d)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_assignment) {
  {
    test_parser p("x=y");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::assignment);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(summarize(ast->child_1()), "var y");
    EXPECT_THAT(p.errors(), IsEmpty());
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
  }

  {
    test_parser p("x.p=z");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::assignment);
    EXPECT_EQ(summarize(ast->child_0()), "dot(var x, p)");
    EXPECT_EQ(summarize(ast->child_1()), "var z");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("f().p=x");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(dot(call(var f), p), var x)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("x=y=z");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(var x, assign(var y, var z))");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("x,y=z,w");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, assign(var y, var z), var w)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_invalid_assignment) {
  {
    test_parser p("x+y=z");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "assign(binary(var x, var y), var z)");

    ASSERT_EQ(p.errors().size(), 1);
    auto &error = p.errors()[0];
    EXPECT_EQ(error.kind,
              error_collector::error_invalid_expression_left_of_assignment);
    EXPECT_EQ(p.error_range(0).begin_offset(), 0);
    EXPECT_EQ(p.error_range(0).end_offset(), 3);
  }

  for (const char *code : {
           "f()=x",
           "-x=y",
           "42=y",
           "(x=y)=z",
       }) {
    test_parser p(code);
    p.parse_expression();

    ASSERT_EQ(p.errors().size(), 1);
    auto &error = p.errors()[0];
    EXPECT_EQ(error.kind,
              error_collector::error_invalid_expression_left_of_assignment);
  }
}

TEST(test_parse_expression, parse_prefix_plusplus_minusminus) {
  {
    test_parser p("++x");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_prefix);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("--y");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_prefix);
    EXPECT_EQ(summarize(ast->child_0()), "var y");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_suffix_plusplus_minusminus) {
  {
    test_parser p("x++");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::rw_unary_suffix);
    EXPECT_EQ(summarize(ast->child_0()), "var x");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 3);
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, suffix_plusplus_minusminus_disallows_line_break) {
  {
    test_parser p("x\n++\ny");

    expression_ptr ast_1 = p.parse_expression();
    EXPECT_EQ(summarize(ast_1), "var x");

    ASSERT_EQ(p.lexer().peek().type, token_type::semicolon)
        << "Semicolon should be inserted (ASI)";
    p.lexer().skip();

    expression_ptr ast_2 = p.parse_expression();
    EXPECT_EQ(summarize(ast_2), "rwunary(var y)");

    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_template) {
  {
    test_parser p("`hello`");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::literal);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 7);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("`hello${world}`");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::_template);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(summarize(ast->child(0)), "var world");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 15);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("`${one}${two}${three}`");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(var one, var two, var three)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_comma_expression) {
  {
    test_parser p("x,y,z");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::binary_operator);
    EXPECT_EQ(summarize(ast->child(0)), "var x");
    EXPECT_EQ(summarize(ast->child(1)), "var y");
    EXPECT_EQ(summarize(ast->child(2)), "var z");
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 5);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("(x+(y,z)+w)");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var x, binary(var y, var z), var w)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("`${2+2, four}`");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "template(binary(literal, literal, var four))");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_function_expression) {
  {
    test_parser p("function(){}");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_EQ(p.range(ast).begin_offset(), 0);
    EXPECT_EQ(p.range(ast).end_offset(), 12);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("function(x, y){}");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::function);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("function(){}()");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::call);
    EXPECT_EQ(ast->child_count(), 1);
    EXPECT_EQ(ast->child_0()->kind(), expression_kind::function);
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("function f(){}");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(ast->kind(), expression_kind::named_function);
    EXPECT_EQ(ast->variable_identifier().string_view(), "f");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

TEST(test_parse_expression, parse_mixed_expression) {
  {
    test_parser p("a+f()");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "binary(var a, call(var f))");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("a+f(x+y,-z-w)+b");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast),
              "binary(var a, call(var f, binary(var x, var y), "
              "binary(unary(var z), var w)), var b)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }

  {
    test_parser p("(x+y).z");
    expression_ptr ast = p.parse_expression();
    EXPECT_EQ(summarize(ast), "dot(binary(var x, var y), z)");
    EXPECT_THAT(p.errors(), IsEmpty());
  }
}

std::string summarize(const expression &expression) {
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
  switch (expression.kind()) {
    case expression_kind::_invalid:
      return "?";
    case expression_kind::_new:
      return "new(" + children() + ")";
    case expression_kind::_template:
      return "template(" + children() + ")";
    case expression_kind::assignment:
      return "assign(" + children() + ")";
    case expression_kind::await:
      return "await(" + children() + ")";
    case expression_kind::call:
      return "call(" + children() + ")";
    case expression_kind::dot:
      return "dot(" + summarize(expression.child_0()) + ", " +
             std::string(expression.variable_identifier().string_view()) + ")";
    case expression_kind::function:
      return "function";
    case expression_kind::literal:
      return "literal";
    case expression_kind::named_function:
      return "function " +
             std::string(expression.variable_identifier().string_view());
    case expression_kind::rw_unary_prefix:
      return "rwunary(" + summarize(expression.child_0()) + ")";
    case expression_kind::rw_unary_suffix:
      return "rwunarysuffix(" + summarize(expression.child_0()) + ")";
    case expression_kind::unary_operator:
      return "unary(" + summarize(expression.child_0()) + ")";
    case expression_kind::variable:
      return std::string("var ") +
             std::string(expression.variable_identifier().string_view());
    case expression_kind::binary_operator:
      return "binary(" + children() + ")";
  }
  QLJS_UNREACHABLE();
}

std::string summarize(expression_ptr expression) {
  return summarize(*expression);
}
}  // namespace
}  // namespace quick_lint_js
