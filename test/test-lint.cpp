#include <doctest/doctest.h>
#include <quicklint-js/error-collector.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/lint.h>
#include <quicklint-js/parse.h>

namespace quicklint_js {
namespace {
TEST_CASE("variable use before declaration") {
  const char *input = "x x";
  error_collector v;
  linter l(&v);
  l.visit_variable_use(identifier(source_code_span(&input[0], &input[1])));
  l.visit_variable_declaration(
      identifier(source_code_span(&input[2], &input[3])), variable_kind::_let);

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
      identifier(source_code_span(&input[0], &input[1])), variable_kind::_let);
  l.visit_variable_use(identifier(source_code_span(&input[2], &input[3])));
  CHECK(v.errors.empty());
}
}  // namespace
}  // namespace quicklint_js
