#include <doctest/doctest.h>
#include <quicklint-js/lex.h>
#include <string>
#include <vector>

namespace quicklint_js {
namespace {
class parser {
 public:
  parser(const char *input) : lexer_(input) {}

  template <class Visitor>
  void parse_statement(Visitor &v) {
    this->lexer_.skip();
    this->parse_let_bindings(v);
  }

 private:
  template <class Visitor>
  void parse_let_bindings(Visitor &v) {
    const token &t = this->lexer_.peek();
    v.visit_variable_declaration(t.identifier_name());
    this->lexer_.skip();

    for (;;) {
      if (this->lexer_.peek().type != token_type::comma) {
        break;
      }
      this->lexer_.skip();

      const token &t2 = this->lexer_.peek();
      switch (t2.type) {
        case token_type::identifier:
          v.visit_variable_declaration(t2.identifier_name());
          this->lexer_.skip();
          break;
        default:
          break;
      }
    }

    if (this->lexer_.peek().type == token_type::semicolon) {
      this->lexer_.skip();
    }
  }

  lexer lexer_;
};

TEST_CASE("parse let") {
  struct visitor {
    void visit_variable_declaration(std::string_view name) {
      this->variable_declarations.emplace_back(
          visited_variable_declaration{std::string(name)});
    }

    struct visited_variable_declaration {
      std::string name;
    };
    std::vector<visited_variable_declaration> variable_declarations;
  };

  {
    visitor v;
    parser p("let x");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 1);
    CHECK(v.variable_declarations[0].name == "x");
  }

  {
    visitor v;
    parser p("let a, b");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 2);
    CHECK(v.variable_declarations[0].name == "a");
    CHECK(v.variable_declarations[1].name == "b");
  }

  {
    visitor v;
    parser p("let a, b, c, d, e, f, g");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 7);
    CHECK(v.variable_declarations[0].name == "a");
    CHECK(v.variable_declarations[1].name == "b");
    CHECK(v.variable_declarations[2].name == "c");
    CHECK(v.variable_declarations[3].name == "d");
    CHECK(v.variable_declarations[4].name == "e");
    CHECK(v.variable_declarations[5].name == "f");
    CHECK(v.variable_declarations[6].name == "g");
  }

  {
    visitor v;
    parser p("let first; let second");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 1);
    CHECK(v.variable_declarations[0].name == "first");
    p.parse_statement(v);
    CHECK(v.variable_declarations.size() == 2);
    CHECK(v.variable_declarations[0].name == "first");
    CHECK(v.variable_declarations[1].name == "second");
  }
}
}  // namespace
}  // namespace quicklint_js
