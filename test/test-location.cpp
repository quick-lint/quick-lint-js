#include <doctest/doctest.h>
#include <quicklint-js/location.h>

namespace quicklint_js {
namespace {
TEST_CASE("ranges on first line") {
  const char code[] = "let x = 2;";
  locator l(code);
  source_range x_range = l.range(source_code_span(&code[4], &code[5]));

  CHECK(x_range.begin_offset() == 4);
  CHECK(x_range.begin().line_number == 1);
  CHECK(x_range.begin().column_number == 5);

  CHECK(x_range.end_offset() == 5);
  CHECK(x_range.end().line_number == 1);
  CHECK(x_range.end().column_number == 6);
}

TEST_CASE("ranges on second line") {
  const char code[] = "let x = 2;\nlet y = 3;";
  locator l(code);
  source_range x_range = l.range(source_code_span(&code[15], &code[16]));

  CHECK(x_range.begin_offset() == 15);
  CHECK(x_range.begin().line_number == 2);
  CHECK(x_range.begin().column_number == 5);

  CHECK(x_range.end_offset() == 16);
  CHECK(x_range.end().line_number == 2);
  CHECK(x_range.end().column_number == 6);
}
}  // namespace
}  // namespace quicklint_js
