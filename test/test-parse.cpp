#include <doctest/doctest.h>
#include <quicklint-js/error.h>
#include <quicklint-js/location.h>
#include <quicklint-js/parse.h>
#include <string>
#include <string_view>
#include <vector>

namespace quicklint_js {
namespace {
struct error_collector : public error_reporter {
  void report_error_invalid_binding_in_let_statement(source_code_span where) {
    this->errors.emplace_back(
        error{error_invalid_binding_in_let_statement, where});
  }

  void report_error_let_with_no_bindings(source_code_span where) {
    this->errors.emplace_back(error{error_let_with_no_bindings, where});
  }

  void report_error_missing_oprand_for_operator(source_code_span where) {
    this->errors.emplace_back(error{error_missing_oprand_for_operator, where});
  }

  void report_error_stray_comma_in_let_statement(source_code_span where) {
    this->errors.emplace_back(error{error_stray_comma_in_let_statement, where});
  }

  void report_error_unexpected_identifier(source_code_span where) {
    this->errors.emplace_back(error{error_unexpected_identifier, where});
  }

  void report_error_unmatched_parenthesis(source_code_span where) {
    this->errors.emplace_back(error{error_unmatched_parenthesis, where});
  }

  enum error_kind {
    error_invalid_binding_in_let_statement,
    error_let_with_no_bindings,
    error_missing_oprand_for_operator,
    error_stray_comma_in_let_statement,
    error_unexpected_identifier,
    error_unmatched_parenthesis,
  };
  struct error {
    error_kind kind;
    source_code_span where;
  };
  std::vector<error> errors;
};

struct visitor : public error_collector {
  std::vector<const char *> visits;

  void visit_variable_declaration(identifier name) {
    this->variable_declarations.emplace_back(
        visited_variable_declaration{std::string(name.string_view())});
    this->visits.emplace_back("visit_variable_declaration");
  }

  struct visited_variable_declaration {
    std::string name;
  };
  std::vector<visited_variable_declaration> variable_declarations;

  void visit_variable_use(identifier name) {
    this->variable_uses.emplace_back(
        visited_variable_use{std::string(name.string_view())});
    this->visits.emplace_back("visit_variable_use");
  }

  struct visited_variable_use {
    std::string name;
  };
  std::vector<visited_variable_use> variable_uses;
};

TEST_CASE("parse let") {
  {
    visitor v;
    parser p("let x", &v);
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 1);
    CHECK(v.variable_declarations[0].name == "x");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let a, b", &v);
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 2);
    CHECK(v.variable_declarations[0].name == "a");
    CHECK(v.variable_declarations[1].name == "b");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let a, b, c, d, e, f, g", &v);
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 7);
    CHECK(v.variable_declarations[0].name == "a");
    CHECK(v.variable_declarations[1].name == "b");
    CHECK(v.variable_declarations[2].name == "c");
    CHECK(v.variable_declarations[3].name == "d");
    CHECK(v.variable_declarations[4].name == "e");
    CHECK(v.variable_declarations[5].name == "f");
    CHECK(v.variable_declarations[6].name == "g");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let first; let second", &v);
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 1);
    CHECK(v.variable_declarations[0].name == "first");
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 2);
    CHECK(v.variable_declarations[0].name == "first");
    CHECK(v.variable_declarations[1].name == "second");
    CHECK(v.errors.empty());
  }
}

TEST_CASE("parse let with initializers") {
  {
    visitor v;
    parser p("let x = 2", &v);
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 1);
    CHECK(v.variable_declarations[0].name == "x");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let x = 2, y = 3", &v);
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 2);
    CHECK(v.variable_declarations[0].name == "x");
    CHECK(v.variable_declarations[1].name == "y");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let x = other, y = x", &v);
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 2);
    CHECK(v.variable_declarations[0].name == "x");
    CHECK(v.variable_declarations[1].name == "y");
    REQUIRE(v.variable_uses.size() == 2);
    CHECK(v.variable_uses[0].name == "other");
    CHECK(v.variable_uses[1].name == "x");
    CHECK(v.errors.empty());
  }
}

TEST_CASE(
    "variables used in let initializer are used before variable declaration") {
  using namespace std::literals::string_view_literals;

  visitor v;
  parser p("let x = x", &v);
  p.parse_statement(v);

  REQUIRE(v.visits.size() == 2);
  CHECK(v.visits[0] == "visit_variable_use");
  CHECK(v.visits[1] == "visit_variable_declaration");

  REQUIRE(v.variable_declarations.size() == 1);
  CHECK(v.variable_declarations[0].name == "x");
  REQUIRE(v.variable_uses.size() == 1);
  CHECK(v.variable_uses[0].name == "x");
  CHECK(v.errors.empty());
}

TEST_CASE("parse invalid let") {
  {
    visitor v;
    parser p("let", &v);
    p.parse_statement(v);
    CHECK(v.variable_declarations.empty());
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_let_with_no_bindings);
    CHECK(p.locator().range(error.where).begin_offset() == 0);
    CHECK(p.locator().range(error.where).end_offset() == 3);
  }

  {
    visitor v;
    parser p("let a,", &v);
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 1);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_stray_comma_in_let_statement);
    CHECK(p.locator().range(error.where).begin_offset() == 5);
    CHECK(p.locator().range(error.where).end_offset() == 6);
  }

  {
    visitor v;
    parser p("let x, 42", &v);
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 1);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_invalid_binding_in_let_statement);
    CHECK(p.locator().range(error.where).begin_offset() == 7);
    CHECK(p.locator().range(error.where).end_offset() == 9);
  }

  {
    visitor v;
    parser p("let if", &v);
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 0);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_invalid_binding_in_let_statement);
    CHECK(p.locator().range(error.where).begin_offset() == 4);
    CHECK(p.locator().range(error.where).end_offset() == 6);
  }

  {
    visitor v;
    parser p("let 42", &v);
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 0);
    CHECK(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_invalid_binding_in_let_statement);
    CHECK(p.locator().range(error.where).begin_offset() == 4);
    CHECK(p.locator().range(error.where).end_offset() == 6);
  }
}

TEST_CASE("parse math expression") {
  expression_options options = {.parse_commas = true};

  for (const char *input :
       {"2", "2+2", "2^2", "2 + + 2", "2 * (3 + 4)", "1+1+1+1+1"}) {
    INFO("input = " << input);
    visitor v;
    parser p(input, &v);
    p.parse_expression(v, options);
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("some_var", &v);
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 1);
    CHECK(v.variable_uses[0].name == "some_var");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("some_var + some_other_var", &v);
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 2);
    CHECK(v.variable_uses[0].name == "some_var");
    CHECK(v.variable_uses[1].name == "some_other_var");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("+ v", &v);
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 1);
    CHECK(v.variable_uses[0].name == "v");
    CHECK(v.errors.empty());
  }
}

TEST_CASE("parse invalid math expression") {
  expression_options options = {.parse_commas = true};

  {
    visitor v;
    parser p("2 +", &v);
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_missing_oprand_for_operator);
    CHECK(p.locator().range(error.where).begin_offset() == 2);
    CHECK(p.locator().range(error.where).end_offset() == 3);
  }

  {
    visitor v;
    parser p("^ 2", &v);
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_missing_oprand_for_operator);
    CHECK(p.locator().range(error.where).begin_offset() == 0);
    CHECK(p.locator().range(error.where).end_offset() == 1);
  }

  {
    visitor v;
    parser p("2 * * 2", &v);
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_missing_oprand_for_operator);
    CHECK(p.locator().range(error.where).begin_offset() == 2);
    CHECK(p.locator().range(error.where).end_offset() == 3);
  }

  {
    visitor v;
    parser p("2 & & & 2", &v);
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 2);

    auto *error = &v.errors[0];
    CHECK(error->kind == visitor::error_missing_oprand_for_operator);
    CHECK(p.locator().range(error->where).begin_offset() == 2);
    CHECK(p.locator().range(error->where).end_offset() == 3);

    error = &v.errors[1];
    CHECK(error->kind == visitor::error_missing_oprand_for_operator);
    CHECK(p.locator().range(error->where).begin_offset() == 4);
    CHECK(p.locator().range(error->where).end_offset() == 5);
  }

  {
    visitor v;
    parser p("(2 *)", &v);
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_missing_oprand_for_operator);
    CHECK(p.locator().range(error.where).begin_offset() == 3);
    CHECK(p.locator().range(error.where).end_offset() == 4);
  }
  {
    visitor v;
    parser p("2 * (3 + 4", &v);
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_unmatched_parenthesis);
    CHECK(p.locator().range(error.where).begin_offset() == 4);
    CHECK(p.locator().range(error.where).end_offset() == 5);
  }

  {
    visitor v;
    parser p("2 * (3 + (4", &v);
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 2);

    auto *error = &v.errors[0];
    CHECK(error->kind == visitor::error_unmatched_parenthesis);
    CHECK(p.locator().range(error->where).begin_offset() == 9);
    CHECK(p.locator().range(error->where).end_offset() == 10);

    error = &v.errors[1];
    CHECK(error->kind == visitor::error_unmatched_parenthesis);
    CHECK(p.locator().range(error->where).begin_offset() == 4);
    CHECK(p.locator().range(error->where).end_offset() == 5);
  }

  {
    visitor v;
    parser p("ten ten", &v);
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_unexpected_identifier);
    CHECK(p.locator().range(error.where).begin_offset() == 4);
    CHECK(p.locator().range(error.where).end_offset() == 7);
  }
}

TEST_CASE("parse function calls") {
  expression_options options = {.parse_commas = true};

  {
    visitor v;
    parser p("f(x)", &v);
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 2);
    CHECK(v.variable_uses[0].name == "f");
    CHECK(v.variable_uses[1].name == "x");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("f(x, y)", &v);
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 3);
    CHECK(v.variable_uses[0].name == "f");
    CHECK(v.variable_uses[1].name == "x");
    CHECK(v.variable_uses[2].name == "y");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("o.f(x, y)", &v);
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 3);
    CHECK(v.variable_uses[0].name == "o");
    CHECK(v.variable_uses[1].name == "x");
    CHECK(v.variable_uses[2].name == "y");
    CHECK(v.errors.empty());
  }
}

TEST_CASE("parse invalid function calls") {
  expression_options options = {.parse_commas = true};

  {
    visitor v;
    parser p("(x)f", &v);
    p.parse_expression(v, options);

    REQUIRE(v.errors.size() == 1);
    auto &error = v.errors[0];
    CHECK(error.kind == visitor::error_unexpected_identifier);
    CHECK(p.locator().range(error.where).begin_offset() == 3);
    CHECK(p.locator().range(error.where).end_offset() == 4);

    REQUIRE(v.variable_uses.size() == 2);
    CHECK(v.variable_uses[0].name == "x");
    CHECK(v.variable_uses[1].name == "f");
  }
}

TEST_CASE("parse property lookup: variable.property") {
  expression_options options = {.parse_commas = true};

  {
    visitor v;
    parser p("some_var.some_property", &v);
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 1);
    CHECK(v.variable_uses[0].name == "some_var");
    CHECK(v.errors.empty());
  }
}
}  // namespace
}  // namespace quicklint_js
