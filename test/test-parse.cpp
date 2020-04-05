#include <doctest/doctest.h>
#include <optional>
#include <quicklint-js/lex.h>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace quicklint_js {
namespace {
struct expression_options {
  bool parse_commas;
};

class parser {
 public:
  parser(const char *input) : lexer_(input), locator_(input) {}

  quicklint_js::locator &locator() noexcept { return this->locator_; }

  template <class Visitor>
  void parse_statement(Visitor &v) {
    this->parse_let_bindings(v);
  }

  template <class Visitor>
  void parse_expression(Visitor &v, expression_options options) {
    bool allow_binary_operator = false;
    bool allow_identifier = true;

    std::optional<token> last_operator;

    std::vector<token> left_parens;

    for (;;) {
      switch (this->peek().type) {
        case token_type::left_paren: {
          token left_paren = this->peek();
          this->lexer_.skip();
          this->parse_expression(v, expression_options{.parse_commas = true});
          if (this->peek().type == token_type::right_paren) {
            this->lexer_.skip();
          } else {
            v.visit_error_unmatched_parenthesis(left_paren.span());
          }
          last_operator = std::nullopt;
          allow_identifier = false;
          break;
        }

        case token_type::identifier:
          if (!allow_identifier) {
            v.visit_error_unexpected_identifier(this->peek().span());
          }
          v.visit_variable_use(this->peek().identifier_name());
          this->lexer_.skip();
          last_operator = std::nullopt;
          allow_binary_operator = true;
          allow_identifier = false;
          break;

        case token_type::number:
          this->lexer_.skip();
          last_operator = std::nullopt;
          allow_binary_operator = true;
          break;

        case token_type::plus:
          last_operator = this->peek();
          this->lexer_.skip();
          allow_binary_operator = false;
          allow_identifier = true;
          break;

        case token_type::dot:
          this->lexer_.skip();
          this->lexer_.skip();
          break;

        case token_type::comma:
          if (options.parse_commas) {
            goto parse_binary_operator;
          } else {
            goto done;
          }

        case token_type::ampersand:
        case token_type::circumflex:
        case token_type::star:
        parse_binary_operator:
          if (!allow_binary_operator) {
            const token &bad_token =
                last_operator.has_value() ? *last_operator : this->peek();
            v.visit_error_missing_oprand_for_operator(bad_token.span());
          }
          last_operator = this->peek();
          this->lexer_.skip();
          allow_binary_operator = false;
          allow_identifier = true;
          break;

        case token_type::right_paren:
        default:
        done:
          if (last_operator.has_value()) {
            v.visit_error_missing_oprand_for_operator(last_operator->span());
          }
          for (const token &left_paren : left_parens) {
            v.visit_error_unmatched_parenthesis(left_paren.span());
          }
          return;
      }
    }
  }

 private:
  template <class Visitor>
  void parse_let_bindings(Visitor &v) {
    token let = this->peek();
    this->lexer_.skip();
    bool first_binding = true;
    for (;;) {
      token comma;
      if (!first_binding) {
        comma = this->peek();
        if (comma.type != token_type::comma) {
          break;
        }
        this->lexer_.skip();
      }

      switch (this->peek().type) {
        case token_type::identifier: {
          token identifier_token = this->peek();
          this->lexer_.skip();
          if (this->peek().type == token_type::equal) {
            this->lexer_.skip();
            this->parse_expression(v,
                                   expression_options{.parse_commas = false});
          }
          v.visit_variable_declaration(identifier_token.identifier_name());
          break;
        }
        case token_type::_if:
        case token_type::number:
          v.visit_error_invalid_binding_in_let_statement(this->peek().span());
          break;
        default:
          if (first_binding) {
            v.visit_error_let_with_no_bindings(let.span());
          } else {
            v.visit_error_stray_comma_in_let_statement(comma.span());
          }
          break;
      }
      first_binding = false;
    }

    if (this->peek().type == token_type::semicolon) {
      this->lexer_.skip();
    }
  }

  const token &peek() const noexcept { return this->lexer_.peek(); }

  lexer lexer_;
  quicklint_js::locator locator_;
};

struct visitor {
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

  void visit_error_invalid_binding_in_let_statement(source_code_span where) {
    this->errors.emplace_back(error_invalid_binding_in_let_statement{where});
    this->visits.emplace_back("visit_error_invalid_binding_in_let_statement");
  }

  void visit_error_let_with_no_bindings(source_code_span where) {
    this->errors.emplace_back(error_let_with_no_bindings{where});
    this->visits.emplace_back("visit_error_let_with_no_bindings");
  }

  void visit_error_missing_oprand_for_operator(source_code_span where) {
    this->errors.emplace_back(error_missing_oprand_for_operator{where});
    this->visits.emplace_back("visit_error_missing_oprand_for_operator");
  }

  void visit_error_stray_comma_in_let_statement(source_code_span where) {
    this->errors.emplace_back(error_stray_comma_in_let_statement{where});
    this->visits.emplace_back("visit_error_stray_comma_in_let_statement");
  }

  void visit_error_unexpected_identifier(source_code_span where) {
    this->errors.emplace_back(error_unexpected_identifier{where});
    this->visits.emplace_back("visit_error_unexpected_identifier");
  }

  void visit_error_unmatched_parenthesis(source_code_span where) {
    this->errors.emplace_back(error_unmatched_parenthesis{where});
    this->visits.emplace_back("visit_error_unmatched_parenthesis");
  }

  struct error {
    source_code_span where;
  };
  struct error_invalid_binding_in_let_statement : error {};
  struct error_let_with_no_bindings : error {};
  struct error_missing_oprand_for_operator : error {};
  struct error_stray_comma_in_let_statement : error {};
  struct error_unexpected_identifier : error {};
  struct error_unmatched_parenthesis : error {};
  using visited_error = std::variant<
      error_invalid_binding_in_let_statement, error_let_with_no_bindings,
      error_missing_oprand_for_operator, error_stray_comma_in_let_statement,
      error_unexpected_identifier, error_unmatched_parenthesis>;
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

TEST_CASE("parse let with initializers") {
  {
    visitor v;
    parser p("let x = 2");
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 1);
    CHECK(v.variable_declarations[0].name == "x");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let x = 2, y = 3");
    p.parse_statement(v);
    REQUIRE(v.variable_declarations.size() == 2);
    CHECK(v.variable_declarations[0].name == "x");
    CHECK(v.variable_declarations[1].name == "y");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("let x = other, y = x");
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
  parser p("let x = x");
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
    parser p("let");
    p.parse_statement(v);
    CHECK(v.variable_declarations.empty());
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_let_with_no_bindings>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 0);
    CHECK(p.locator().range(error->where).end_offset() == 3);
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
    CHECK(p.locator().range(error->where).begin_offset() == 5);
    CHECK(p.locator().range(error->where).end_offset() == 6);
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
    CHECK(p.locator().range(error->where).begin_offset() == 7);
    CHECK(p.locator().range(error->where).end_offset() == 9);
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
    CHECK(p.locator().range(error->where).begin_offset() == 4);
    CHECK(p.locator().range(error->where).end_offset() == 6);
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
    CHECK(p.locator().range(error->where).begin_offset() == 4);
    CHECK(p.locator().range(error->where).end_offset() == 6);
  }
}

TEST_CASE("parse math expression") {
  expression_options options = {.parse_commas = true};

  for (const char *input :
       {"2", "2+2", "2^2", "2 + + 2", "2 * (3 + 4)", "1+1+1+1+1"}) {
    INFO("input = " << input);
    visitor v;
    parser p(input);
    p.parse_expression(v, options);
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("some_var");
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 1);
    CHECK(v.variable_uses[0].name == "some_var");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("some_var + some_other_var");
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 2);
    CHECK(v.variable_uses[0].name == "some_var");
    CHECK(v.variable_uses[1].name == "some_other_var");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("+ v");
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
    parser p("2 +");
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_missing_oprand_for_operator>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 2);
    CHECK(p.locator().range(error->where).end_offset() == 3);
  }

  {
    visitor v;
    parser p("^ 2");
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_missing_oprand_for_operator>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 0);
    CHECK(p.locator().range(error->where).end_offset() == 1);
  }

  {
    visitor v;
    parser p("2 * * 2");
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_missing_oprand_for_operator>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 2);
    CHECK(p.locator().range(error->where).end_offset() == 3);
  }

  {
    visitor v;
    parser p("2 & & & 2");
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 2);

    auto *error =
        std::get_if<visitor::error_missing_oprand_for_operator>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 2);
    CHECK(p.locator().range(error->where).end_offset() == 3);

    error =
        std::get_if<visitor::error_missing_oprand_for_operator>(&v.errors[1]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 4);
    CHECK(p.locator().range(error->where).end_offset() == 5);
  }

  {
    visitor v;
    parser p("(2 *)");
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_missing_oprand_for_operator>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 3);
    CHECK(p.locator().range(error->where).end_offset() == 4);
  }
  {
    visitor v;
    parser p("2 * (3 + 4");
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_unmatched_parenthesis>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 4);
    CHECK(p.locator().range(error->where).end_offset() == 5);
  }

  {
    visitor v;
    parser p("2 * (3 + (4");
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 2);

    auto *error =
        std::get_if<visitor::error_unmatched_parenthesis>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 9);
    CHECK(p.locator().range(error->where).end_offset() == 10);

    error = std::get_if<visitor::error_unmatched_parenthesis>(&v.errors[1]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 4);
    CHECK(p.locator().range(error->where).end_offset() == 5);
  }

  {
    visitor v;
    parser p("ten ten");
    p.parse_expression(v, options);
    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_unexpected_identifier>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 4);
    CHECK(p.locator().range(error->where).end_offset() == 7);
  }
}

TEST_CASE("parse function calls") {
  expression_options options = {.parse_commas = true};

  {
    visitor v;
    parser p("f(x)");
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 2);
    CHECK(v.variable_uses[0].name == "f");
    CHECK(v.variable_uses[1].name == "x");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("f(x, y)");
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 3);
    CHECK(v.variable_uses[0].name == "f");
    CHECK(v.variable_uses[1].name == "x");
    CHECK(v.variable_uses[2].name == "y");
    CHECK(v.errors.empty());
  }

  {
    visitor v;
    parser p("o.f(x, y)");
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
    parser p("(x)f");
    p.parse_expression(v, options);

    REQUIRE(v.errors.size() == 1);
    auto *error =
        std::get_if<visitor::error_unexpected_identifier>(&v.errors[0]);
    REQUIRE(error);
    CHECK(p.locator().range(error->where).begin_offset() == 3);
    CHECK(p.locator().range(error->where).end_offset() == 4);

    REQUIRE(v.variable_uses.size() == 2);
    CHECK(v.variable_uses[0].name == "x");
    CHECK(v.variable_uses[1].name == "f");
  }
}

TEST_CASE("parse property lookup: variable.property") {
  expression_options options = {.parse_commas = true};

  {
    visitor v;
    parser p("some_var.some_property");
    p.parse_expression(v, options);
    REQUIRE(v.variable_uses.size() == 1);
    CHECK(v.variable_uses[0].name == "some_var");
    CHECK(v.errors.empty());
  }
}
}  // namespace
}  // namespace quicklint_js
