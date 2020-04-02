#include <doctest/doctest.h>
#include <quicklint-js/lex.h>
#include <string>
#include <variant>
#include <vector>

namespace quicklint_js {
namespace {
class parser {
 public:
  parser(const char *input) : lexer_(input), original_input_(input) {}

  template <class Visitor>
  void parse_statement(Visitor &v) {
    this->parse_let_bindings(v);
  }

 private:
  template <class Visitor>
  void parse_let_bindings(Visitor &v) {
    token let = this->lexer_.peek();
    this->lexer_.skip();
    bool first_binding = true;
    for (;;) {
      token comma;
      if (!first_binding) {
        comma = this->lexer_.peek();
        if (comma.type != token_type::comma) {
          break;
        }
        this->lexer_.skip();
      }

      const token &t2 = this->lexer_.peek();
      switch (t2.type) {
        case token_type::identifier:
          v.visit_variable_declaration(t2.identifier_name());
          this->lexer_.skip();
          break;
        case token_type::_if:
        case token_type::number:
          v.visit_error_invalid_binding_in_let_statement(
              t2.range(this->original_input_));
          break;
        default:
          if (first_binding) {
            v.visit_error_let_with_no_bindings(
                let.range(this->original_input_));
          } else {
            v.visit_error_stray_comma_in_let_statement(
                comma.range(this->original_input_));
          }
          break;
      }
      first_binding = false;
    }

    if (this->lexer_.peek().type == token_type::semicolon) {
      this->lexer_.skip();
    }
  }

  lexer lexer_;
  const char *original_input_;
};

struct visitor {
  void visit_variable_declaration(std::string_view name) {
    this->variable_declarations.emplace_back(
        visited_variable_declaration{std::string(name)});
  }

  struct visited_variable_declaration {
    std::string name;
  };
  std::vector<visited_variable_declaration> variable_declarations;

  void visit_error_invalid_binding_in_let_statement(source_range where) {
    this->errors.emplace_back(error_invalid_binding_in_let_statement{where});
  }

  void visit_error_let_with_no_bindings(source_range where) {
    this->errors.emplace_back(error_let_with_no_bindings{where});
  }

  void visit_error_stray_comma_in_let_statement(source_range where) {
    this->errors.emplace_back(error_stray_comma_in_let_statement{where});
  }

  struct error {
    source_range where;
  };
  struct error_invalid_binding_in_let_statement : error {};
  struct error_let_with_no_bindings : error {};
  struct error_stray_comma_in_let_statement : error {};
  using visited_error = std::variant<error_invalid_binding_in_let_statement,
                                     error_let_with_no_bindings,
                                     error_stray_comma_in_let_statement>;
  std::vector<visited_error> errors;
};

TEST_CASE("parse let") {
  {
    visitor v;
    parser p("let x");
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 1);
    CHECK(v.variable_declarations[0].name == "x");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let a, b");
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 2);
    CHECK(v.variable_declarations[0].name == "a");
    CHECK(v.variable_declarations[1].name == "b");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let a, b, c, d, e, f, g");
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
    parser p("let first; let second");
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

TEST_CASE("parse invalid let") {
  {
    visitor v;
    parser p("let");
    p.parse_statement(v);
    CHECK(v.variable_declarations.empty());
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_let_with_no_bindings>(&v.errors[0]);
    REQUIRE(error);
    CHECK(error->where.begin_offset() == 0);
    CHECK(error->where.end_offset() == 3);
  }

  {
    visitor v;
    parser p("let a,");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 1);
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_stray_comma_in_let_statement>(&v.errors[0]);
    REQUIRE(error);
    CHECK(error->where.begin_offset() == 5);
    CHECK(error->where.end_offset() == 6);
  }

  {
    visitor v;
    parser p("let x, 42");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 1);
    REQUIRE(v.errors.size() == 1);
    auto *error = std::get_if<visitor::error_invalid_binding_in_let_statement>(
        &v.errors[0]);
    REQUIRE(error);
    CHECK(error->where.begin_offset() == 7);
    CHECK(error->where.end_offset() == 9);
  }

  {
    visitor v;
    parser p("let if");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 0);
    REQUIRE(v.errors.size() == 1);
    auto *error = std::get_if<visitor::error_invalid_binding_in_let_statement>(
        &v.errors[0]);
    REQUIRE(error);
    CHECK(error->where.begin_offset() == 4);
    CHECK(error->where.end_offset() == 6);
  }

  {
    visitor v;
    parser p("let 42");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 0);
    CHECK(v.errors.size() == 1);
    auto *error = std::get_if<visitor::error_invalid_binding_in_let_statement>(
        &v.errors[0]);
    REQUIRE(error);
    CHECK(error->where.begin_offset() == 4);
    CHECK(error->where.end_offset() == 6);
  }
}
}  // namespace
}  // namespace quicklint_js
