#include <algorithm>
#include <doctest/doctest.h>
#include <quicklint-js/error-collector.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/parse.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace quicklint_js {
namespace {
class linter {
 public:
  explicit linter(error_reporter *error_reporter) noexcept
      : error_reporter_(error_reporter) {}

  void visit_variable_declaration(identifier name) {
    this->declared_variables_.emplace_back(name.string_view());
  }

  void visit_variable_use(identifier name) {
    bool variable_is_declared =
        std::find(this->declared_variables_.begin(),
                  this->declared_variables_.end(),
                  name.string_view()) != this->declared_variables_.end();
    if (!variable_is_declared) {
      this->error_reporter_->report_error_variable_used_before_declaration(
          name);
    }
  }

 private:
  std::vector<std::string> declared_variables_;
  error_reporter *error_reporter_;
};

TEST_CASE("variable use before declaration") {
  const char *input = "x x";
  error_collector v;
  linter l(&v);
  l.visit_variable_use(identifier(source_code_span(&input[0], &input[1])));
  l.visit_variable_declaration(
      identifier(source_code_span(&input[2], &input[3])));

  REQUIRE(v.errors.size() == 1);
  CHECK(v.errors[0].kind ==
        error_collector::error_variable_used_before_declaration);
  CHECK(locator(input).range(v.errors[0].where).begin_offset() == 0);
  CHECK(locator(input).range(v.errors[0].where).end_offset() == 1);
}

TEST_CASE("variable use before declaration (with parsing)") {
  const char *input = "let x = y, y = x;";
  error_collector v;
  linter l(&v);
  parser p(input, &v);
  p.parse_statement(l);

  REQUIRE(v.errors.size() == 1);
  CHECK(v.errors[0].kind ==
        error_collector::error_variable_used_before_declaration);
  CHECK(locator(input).range(v.errors[0].where).begin_offset() == 8);
  CHECK(locator(input).range(v.errors[0].where).end_offset() == 9);
}

TEST_CASE("variable use after declaration") {
  const char *input = "x x";
  error_collector v;
  linter l(&v);
  l.visit_variable_declaration(
      identifier(source_code_span(&input[0], &input[1])));
  l.visit_variable_use(identifier(source_code_span(&input[2], &input[3])));
  CHECK(v.errors.empty());
}
}  // namespace
}  // namespace quicklint_js
