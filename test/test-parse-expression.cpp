// quicklint-js finds bugs in JavaScript programs.
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

#include <doctest/doctest.h>
#include <quicklint-js/error-collector.h>
#include <quicklint-js/error.h>
#include <quicklint-js/location.h>
#include <quicklint-js/parse.h>
#include <string>
#include <string_view>
#include <vector>

namespace quicklint_js {
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

 private:
  error_collector errors_;
  parser parser_;
};

TEST_CASE("parse single-token expression") {
  {
    test_parser p("x");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::variable);
    CHECK(ast->variable_identifier().string_view() == "x");
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 1);
  }

  {
    test_parser p("42");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::literal);
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 2);
  }

  {
    test_parser p("'hello'");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::literal);
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 7);
  }
}

TEST_CASE("parse math expression") {
  {
    test_parser p("-x");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::unary_operator);
    CHECK(ast->child_0()->kind() == expression_kind::variable);
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 2);
  }

  {
    test_parser p("+x");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "unary(var x)");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("x+y");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(var x, var y)");
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 3);
  }

  {
    test_parser p("x+y-z");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(var x, var y, var z)");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("2-4+1");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, literal, literal)");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("-x+y");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(unary(var x), var y)");
    CHECK(p.errors().empty());
  }

  for (const char *input : {"2+2", "2-2", "2*2", "2/2", "2%2", "2**2", "2^2",
                            "2&2", "2|2", "2<<2", "2>>2", "2>>>2"}) {
    INFO("input = " << input);
    test_parser p(input);
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, literal)");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse broken math expression") {
  {
    test_parser p("2+");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, ?)");
    REQUIRE(p.errors().size() == 1);
    CHECK(p.errors()[0].kind ==
          error_collector::error_missing_oprand_for_operator);
    CHECK(p.error_range(0).begin_offset() == 1);
    CHECK(p.error_range(0).end_offset() == 2);
  }

  {
    test_parser p("^2");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(?, literal)");
    REQUIRE(p.errors().size() == 1);
    CHECK(p.errors()[0].kind ==
          error_collector::error_missing_oprand_for_operator);
    CHECK(p.error_range(0).begin_offset() == 0);
    CHECK(p.error_range(0).end_offset() == 1);
  }

  {
    test_parser p("2 * * 2");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, ?, literal)");
    REQUIRE(p.errors().size() == 1);
    CHECK(p.errors()[0].kind ==
          error_collector::error_missing_oprand_for_operator);
    CHECK(p.error_range(0).begin_offset() == 2);
    CHECK(p.error_range(0).end_offset() == 3);
  }

  {
    test_parser p("2 & & & 2");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, ?, ?, literal)");
    REQUIRE(p.errors().size() == 2);

    CHECK(p.errors()[0].kind ==
          error_collector::error_missing_oprand_for_operator);
    CHECK(p.error_range(0).begin_offset() == 2);
    CHECK(p.error_range(0).end_offset() == 3);

    CHECK(p.errors()[1].kind ==
          error_collector::error_missing_oprand_for_operator);
    CHECK(p.error_range(1).begin_offset() == 4);
    CHECK(p.error_range(1).end_offset() == 5);
  }

  {
    test_parser p("(2*)");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, ?)");
    REQUIRE(p.errors().size() == 1);
    CHECK(p.errors()[0].kind ==
          error_collector::error_missing_oprand_for_operator);
    CHECK(p.error_range(0).begin_offset() == 2);
    CHECK(p.error_range(0).end_offset() == 3);
  }

  {
    test_parser p("2 * (3 + 4");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, binary(literal, literal))");
    REQUIRE(p.errors().size() == 1);
    CHECK(p.errors()[0].kind == error_collector::error_unmatched_parenthesis);
    CHECK(p.error_range(0).begin_offset() == 4);
    CHECK(p.error_range(0).end_offset() == 5);
  }

  {
    test_parser p("2 * (3 + (4");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, binary(literal, literal))");
    REQUIRE(p.errors().size() == 2);

    CHECK(p.errors()[0].kind == error_collector::error_unmatched_parenthesis);
    CHECK(p.error_range(0).begin_offset() == 9);
    CHECK(p.error_range(0).end_offset() == 10);

    CHECK(p.errors()[1].kind == error_collector::error_unmatched_parenthesis);
    CHECK(p.error_range(1).begin_offset() == 4);
    CHECK(p.error_range(1).end_offset() == 5);
  }
}

TEST_CASE("parse logical expression") {
  for (const char *input : {"2==2", "2===2", "2!=2", "2!==2", "2>2", "2<2",
                            "2>=2", "2<=2", "2&&2", "2||2"}) {
    INFO("input = " << input);
    test_parser p(input);
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(literal, literal)");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse function call") {
  {
    test_parser p("f()");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::call);
    CHECK(summarize(ast->child_0()) == "var f");
    CHECK(ast->child_count() == 1);
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 3);
  }

  {
    test_parser p("f(x)");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::call);
    CHECK(summarize(ast->child_0()) == "var f");
    CHECK(ast->child_count() == 2);
    CHECK(summarize(ast->child(1)) == "var x");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("f(x,y)");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::call);
    CHECK(summarize(ast->child_0()) == "var f");
    CHECK(ast->child_count() == 3);
    CHECK(summarize(ast->child(1)) == "var x");
    CHECK(summarize(ast->child(2)) == "var y");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse dot expressions") {
  {
    test_parser p("x.prop");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::dot);
    CHECK(summarize(ast->child_0()) == "var x");
    CHECK(ast->variable_identifier().string_view() == "prop");
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 6);
  }

  {
    test_parser p("x.p1.p2");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "dot(dot(var x, p1), p2)");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse parenthesized expression") {
  {
    test_parser p("(x)");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "var x");
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 1);
    CHECK(p.range(ast).end_offset() == 2);
  }

  {
    test_parser p("x+(y)");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(var x, var y)");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("x+(y+z)");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(var x, binary(var y, var z))");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("(x+y)+z");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(binary(var x, var y), var z)");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("x+(y+z)+w");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(var x, binary(var y, var z), var w)");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse await expression") {
  {
    test_parser p("await myPromise");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::await);
    CHECK(summarize(ast->child_0()) == "var myPromise");
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 15);
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse new expression") {
  {
    test_parser p("new Date");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::_new);
    CHECK(ast->child_count() == 1);
    CHECK(summarize(ast->child_0()) == "var Date");
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 8);
    CHECK(p.errors().empty());
  }

  {
    test_parser p("new Date()");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::_new);
    CHECK(ast->child_count() == 1);
    CHECK(summarize(ast->child_0()) == "var Date");
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 10);
    CHECK(p.errors().empty());
  }

  {
    test_parser p("new Date(y,m,d)");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "new(var Date, var y, var m, var d)");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse assignment") {
  {
    test_parser p("x=y");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::assignment);
    CHECK(summarize(ast->child_0()) == "var x");
    CHECK(summarize(ast->child_1()) == "var y");
    CHECK(p.errors().empty());
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 3);
  }

  {
    test_parser p("x.p=z");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::assignment);
    CHECK(summarize(ast->child_0()) == "dot(var x, p)");
    CHECK(summarize(ast->child_1()) == "var z");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("f().p=x");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "assign(dot(call(var f), p), var x)");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("x=y=z");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "assign(var x, assign(var y, var z))");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("x,y=z,w");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(var x, assign(var y, var z), var w)");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse invalid assignment") {
  {
    test_parser p("x+y=z");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "assign(binary(var x, var y), var z)");

    REQUIRE(p.errors().size() == 1);
    auto &error = p.errors()[0];
    CHECK(error.kind ==
          error_collector::error_invalid_expression_left_of_assignment);
    CHECK(p.error_range(0).begin_offset() == 0);
    CHECK(p.error_range(0).end_offset() == 3);
  }

  for (const char *code : {
           "f()=x",
           "-x=y",
           "42=y",
           "(x=y)=z",
       }) {
    test_parser p(code);
    p.parse_expression();

    REQUIRE(p.errors().size() == 1);
    auto &error = p.errors()[0];
    CHECK(error.kind ==
          error_collector::error_invalid_expression_left_of_assignment);
  }
}

TEST_CASE("parse template") {
  {
    test_parser p("`hello`");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::literal);
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 7);
    CHECK(p.errors().empty());
  }

  {
    test_parser p("`hello${world}`");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::_template);
    CHECK(ast->child_count() == 1);
    CHECK(summarize(ast->child(0)) == "var world");
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 15);
    CHECK(p.errors().empty());
  }

  {
    test_parser p("`${one}${two}${three}`");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "template(var one, var two, var three)");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse comma expression") {
  {
    test_parser p("x,y,z");
    expression_ptr ast = p.parse_expression();
    CHECK(ast->kind() == expression_kind::binary_operator);
    CHECK(summarize(ast->child(0)) == "var x");
    CHECK(summarize(ast->child(1)) == "var y");
    CHECK(summarize(ast->child(2)) == "var z");
    CHECK(p.range(ast).begin_offset() == 0);
    CHECK(p.range(ast).end_offset() == 5);
    CHECK(p.errors().empty());
  }

  {
    test_parser p("(x+(y,z)+w)");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(var x, binary(var y, var z), var w)");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("`${2+2, four}`");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "template(binary(literal, literal, var four))");
    CHECK(p.errors().empty());
  }
}

TEST_CASE("parse mixed expression") {
  {
    test_parser p("a+f()");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "binary(var a, call(var f))");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("a+f(x+y,-z-w)+b");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) ==
          "binary(var a, call(var f, binary(var x, var y), binary(unary(var "
          "z), var w)), var b)");
    CHECK(p.errors().empty());
  }

  {
    test_parser p("(x+y).z");
    expression_ptr ast = p.parse_expression();
    CHECK(summarize(ast) == "dot(binary(var x, var y), z)");
    CHECK(p.errors().empty());
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
    case expression_kind::literal:
      return "literal";
    case expression_kind::variable:
      return std::string("var ") +
             std::string(expression.variable_identifier().string_view());
    case expression_kind::unary_operator:
      return "unary(" + summarize(expression.child_0()) + ")";
    case expression_kind::binary_operator:
      return "binary(" + children() + ")";
  }
  __builtin_unreachable();
}

std::string summarize(expression_ptr expression) {
  return summarize(*expression);
}
}  // namespace
}  // namespace quicklint_js
