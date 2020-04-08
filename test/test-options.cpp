#include <doctest/doctest.h>
#include <initializer_list>
#include <iostream>
#include <quicklint-js/options.h>
#include <string_view>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quicklint_js {
namespace {
options parse_options(std::initializer_list<const char *> arguments) {
  std::vector<char *> argv;
  argv.emplace_back(const_cast<char *>("(program)"));
  for (const char *argument : arguments) {
    argv.emplace_back(const_cast<char *>(argument));
  }
  return quicklint_js::parse_options(argv.size(), argv.data());
}

TEST_CASE("default options with no files") {
  options o = parse_options({});
  CHECK_FALSE(o.print_parser_visits);
  CHECK(o.files_to_lint.empty());
}

TEST_CASE("default options with files") {
  options o = parse_options({"foo.js"});
  CHECK_FALSE(o.print_parser_visits);
  REQUIRE(o.files_to_lint.size() == 1);
  CHECK(o.files_to_lint[0] == "foo.js"sv);
}

TEST_CASE("--debug-parser-visits") {
  options o = parse_options({"--debug-parser-visits", "foo.js"});
  CHECK(o.print_parser_visits);
  REQUIRE(o.files_to_lint.size() == 1);
  CHECK(o.files_to_lint[0] == "foo.js"sv);
}

TEST_CASE("invalid option") {
  options o = parse_options({"--option-does-not-exist", "foo.js"});
  REQUIRE(o.error_unrecognized_options.size() == 1);
  CHECK(o.error_unrecognized_options[0] == "--option-does-not-exist"sv);
  CHECK(o.files_to_lint.empty());
}
}  // namespace
}  // namespace quicklint_js
