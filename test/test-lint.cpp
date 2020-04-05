#include <algorithm>
#include <doctest/doctest.h>
#include <quicklint-js/lex.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace quicklint_js {
namespace {
class error_collector {
 public:
  void visit_error_variable_used_before_declaration(identifier name) {
    this->errors.emplace_back(
        error{error_variable_used_before_declaration, name.span()});
  }

  enum error_kind {
    error_variable_used_before_declaration,
  };
  struct error {
    error_kind kind;
    source_code_span where;
  };
  std::vector<error> errors;
};

class linter : public error_collector {
 public:
  void visit_variable_declaration(identifier name) {
    this->declared_variables_.emplace_back(name.string_view());
  }

  void visit_variable_use(identifier name) {
    bool variable_is_declared =
        std::find(this->declared_variables_.begin(),
                  this->declared_variables_.end(),
                  name.string_view()) != this->declared_variables_.end();
    if (!variable_is_declared) {
      this->visit_error_variable_used_before_declaration(name);
    }
  }

  std::vector<std::string> declared_variables_;
};

TEST_CASE("variable use before declaration") {
  const char *input = "x x";
  linter l;
  l.visit_variable_use(identifier(source_code_span(&input[0], &input[1])));
  l.visit_variable_declaration(
      identifier(source_code_span(&input[2], &input[3])));

  REQUIRE(l.errors.size() == 1);
  CHECK(l.errors[0].kind ==
        error_collector::error_variable_used_before_declaration);
  CHECK(locator(input).range(l.errors[0].where).begin_offset() == 0);
  CHECK(locator(input).range(l.errors[0].where).end_offset() == 1);
}

TEST_CASE("variable use after declaration") {
  const char *input = "x x";
  linter l;
  l.visit_variable_declaration(
      identifier(source_code_span(&input[0], &input[1])));
  l.visit_variable_use(identifier(source_code_span(&input[2], &input[3])));
  CHECK(l.errors.empty());
}
}  // namespace
}  // namespace quicklint_js
