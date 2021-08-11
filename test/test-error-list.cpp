// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/error-list.h>
#include <quick-lint-js/error.h>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_error_list, compiled_default_matches_all_errors) {
  compiled_error_list errors;
#define QLJS_ERROR_TYPE(error_name, error_code, struct_body, format) \
  EXPECT_TRUE(errors.is_present(error_type::error_name)) << #error_name;
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  EXPECT_THAT(errors.parse_errors("--testoption"), IsEmpty());
  EXPECT_THAT(errors.parse_warnings(), IsEmpty());
}

TEST(test_error_list, compiled_excluded_error_by_code) {
  parsed_error_list parsed_errors;
  parsed_errors.excluded_codes.emplace_back("E003");

  compiled_error_list errors;
  errors.add(parsed_errors);

  EXPECT_FALSE(
      errors.is_present(error_type::error_assignment_to_const_variable))
      << "E003 should be disabled";
  EXPECT_TRUE(errors.is_present(
      error_type::error_big_int_literal_contains_decimal_point))
      << "E005 should be enabled";

  EXPECT_THAT(errors.parse_errors("--testoption"), IsEmpty());
  EXPECT_THAT(errors.parse_warnings(), IsEmpty());
}

TEST(test_error_list, compiled_excluded_then_included_error_by_code) {
  std::array<parsed_error_list, 2> parsed_error_lists;
  parsed_error_lists[0].excluded_codes.emplace_back("E003");
  parsed_error_lists[1].included_codes.emplace_back("E003");

  compiled_error_list errors;
  errors.add(parsed_error_lists[0]);
  errors.add(parsed_error_lists[1]);

  EXPECT_TRUE(errors.is_present(error_type::error_assignment_to_const_variable))
      << "E003 should be enabled";
}

TEST(test_error_list, compiled_included_then_excluded_error_by_code) {
  std::array<parsed_error_list, 2> parsed_error_lists;
  parsed_error_lists[0].included_codes.emplace_back("E003");
  parsed_error_lists[1].excluded_codes.emplace_back("E003");

  compiled_error_list errors;
  errors.add(parsed_error_lists[0]);
  errors.add(parsed_error_lists[1]);

  EXPECT_FALSE(
      errors.is_present(error_type::error_assignment_to_const_variable))
      << "E003 should be disabled";
}

TEST(test_error_list, compiled_exclude_all_matches_no_errors) {
  parsed_error_list parsed_errors;
  parsed_errors.excluded_categories.emplace_back("all");

  compiled_error_list errors;
  errors.add(parsed_errors);

#define QLJS_ERROR_TYPE(error_name, error_code, struct_body, format) \
  EXPECT_FALSE(errors.is_present(error_type::error_name)) << #error_name;
  QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

  EXPECT_THAT(errors.parse_errors("--testoption"), IsEmpty());
  EXPECT_THAT(errors.parse_warnings(), IsEmpty());
}

TEST(test_error_list,
     compiled_override_and_include_code_matches_only_explicit) {
  parsed_error_list parsed_errors;
  parsed_errors.included_codes.emplace_back("E003");
  parsed_errors.override_defaults = true;

  compiled_error_list errors;
  errors.add(parsed_errors);

  EXPECT_TRUE(errors.is_present(error_type::error_assignment_to_const_variable))
      << "E003 should be enabled";
  EXPECT_FALSE(errors.is_present(
      error_type::error_big_int_literal_contains_decimal_point))
      << "E005 (default) should be disabled";
}

TEST(test_error_list,
     compiled_include_code_then_override_matches_only_later_included_codes) {
  std::array<parsed_error_list, 2> parsed_error_lists;
  parsed_error_lists[0].included_codes.emplace_back("E003");

  parsed_error_lists[1].override_defaults = true;
  parsed_error_lists[1].included_codes.emplace_back("E005");

  compiled_error_list errors;
  errors.add(parsed_error_lists[0]);
  errors.add(parsed_error_lists[1]);

  EXPECT_FALSE(
      errors.is_present(error_type::error_assignment_to_const_variable))
      << "E003 should be disabled";
  EXPECT_TRUE(errors.is_present(
      error_type::error_big_int_literal_contains_decimal_point))
      << "E005 should be enabled";
}

TEST(test_error_list,
     compiled_exclude_all_and_include_code_matches_only_explicit) {
  parsed_error_list parsed_errors;
  parsed_errors.excluded_categories.emplace_back("all");
  parsed_errors.included_codes.emplace_back("E003");

  compiled_error_list errors;
  errors.add(parsed_errors);

  EXPECT_TRUE(errors.is_present(error_type::error_assignment_to_const_variable))
      << "E003 should be enabled";
  EXPECT_FALSE(errors.is_present(
      error_type::error_big_int_literal_contains_decimal_point))
      << "E005 (all) should be disabled";
}

TEST(test_error_list,
     compiled_exclude_default_code_and_include_all_matches_excluded_code) {
  std::array<parsed_error_list, 2> parsed_error_lists;
  // These codes are enabled by default.
  parsed_error_lists[0].excluded_codes.emplace_back("E003");
  parsed_error_lists[0].excluded_codes.emplace_back("E005");

  parsed_error_lists[1].included_categories.emplace_back("all");

  compiled_error_list errors;
  errors.add(parsed_error_lists[0]);
  errors.add(parsed_error_lists[1]);

  EXPECT_TRUE(errors.is_present(error_type::error_assignment_to_const_variable))
      << "E003 (all) should be enabled";
  EXPECT_TRUE(errors.is_present(
      error_type::error_big_int_literal_contains_decimal_point))
      << "E005 (all) should be enabled";
}

TEST(test_error_list, compiling_invalid_category_is_an_error) {
  parsed_error_list parsed_errors;
  parsed_errors.included_categories.emplace_back("banana");
  parsed_errors.excluded_categories.emplace_back("strawberry");

  compiled_error_list errors;
  errors.add(parsed_errors);

  EXPECT_THAT(errors.parse_warnings(),
              UnorderedElementsAre("unknown error category: banana",
                                   "unknown error category: strawberry"));
}

TEST(test_error_list, compiling_invalid_code_is_an_error) {
  parsed_error_list parsed_errors;
  parsed_errors.included_codes.emplace_back("E999");
  parsed_errors.excluded_codes.emplace_back("E000");

  compiled_error_list errors;
  errors.add(parsed_errors);

  EXPECT_THAT(errors.parse_warnings(),
              UnorderedElementsAre("unknown error code: E999",
                                   "unknown error code: E000"));
}

TEST(test_error_list, compiling_empty_parsed_error_list_is_an_error) {
  std::array<parsed_error_list, 3> parsed_error_lists;
  parsed_error_lists[0].included_codes.emplace_back("E003");
  parsed_error_lists[2].excluded_codes.emplace_back("E003");

  compiled_error_list errors;
  errors.add(parsed_error_lists[0]);
  errors.add(parsed_error_lists[1]);
  errors.add(parsed_error_lists[2]);

  EXPECT_THAT(errors.parse_warnings(), IsEmpty());
  EXPECT_THAT(
      errors.parse_errors("--testoption"),
      ElementsAre("--testoption must be given at least one category or code"));
}

TEST(test_error_list, empty_list_is_disallowed) {
  {
    parsed_error_list errors = parse_error_list("");
    EXPECT_TRUE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }

  {
    parsed_error_list errors = parse_error_list("    \t ");
    EXPECT_TRUE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, add_error_code_to_default) {
  {
    parsed_error_list errors = parse_error_list("+E420");
    EXPECT_THAT(errors.included_codes, ElementsAre("E420"));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }

  {
    parsed_error_list errors = parse_error_list("+E420,+E069");
    EXPECT_THAT(errors.included_codes, UnorderedElementsAre("E420", "E069"));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, remove_error_code_from_default) {
  {
    parsed_error_list errors = parse_error_list("-E420");
    EXPECT_THAT(errors.excluded_codes, ElementsAre("E420"));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }

  {
    parsed_error_list errors = parse_error_list("-E420,-E069");
    EXPECT_THAT(errors.excluded_codes, UnorderedElementsAre("E420", "E069"));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, add_error_category_to_default) {
  {
    parsed_error_list errors = parse_error_list("+warning");
    EXPECT_THAT(errors.included_categories, ElementsAre("warning"));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, remove_error_category_from_default) {
  {
    parsed_error_list errors = parse_error_list("-warning");
    EXPECT_THAT(errors.excluded_categories, ElementsAre("warning"));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, set_default_to_category) {
  {
    parsed_error_list errors = parse_error_list("warning");
    EXPECT_THAT(errors.included_categories, ElementsAre("warning"));
    EXPECT_TRUE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, set_default_to_error_code) {
  {
    parsed_error_list errors = parse_error_list("E420");
    EXPECT_THAT(errors.included_codes, ElementsAre("E420"));
    EXPECT_TRUE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, mixed) {
  {
    parsed_error_list errors =
        parse_error_list("+E420,warning,-pedantic,syntax-error");
    EXPECT_THAT(errors.included_codes, ElementsAre("E420"));
    EXPECT_THAT(errors.excluded_codes, IsEmpty());
    EXPECT_THAT(errors.included_categories,
                ElementsAre("warning", "syntax-error"));
    EXPECT_THAT(errors.excluded_categories, ElementsAre("pedantic"));
    EXPECT_TRUE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, whitespace_around_predicates_is_ignored) {
  {
    parsed_error_list errors = parse_error_list("\t +E420 \t");
    EXPECT_THAT(errors.included_codes, ElementsAre("E420"));
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }

  {
    parsed_error_list errors = parse_error_list(" +E420 , +E069");
    EXPECT_THAT(errors.included_codes, UnorderedElementsAre("E420", "E069"));
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, stray_commas_are_ignored) {
  {
    parsed_error_list errors = parse_error_list(",one,,two , three");
    EXPECT_THAT(errors.included_categories,
                UnorderedElementsAre("one", "two", "three"));
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST(test_error_list, unexpected_characters) {
  {
    parsed_error_list errors = parse_error_list("~E420");
    EXPECT_THAT(errors.unexpected, ElementsAre("~"));
  }

  {
    parsed_error_list errors = parse_error_list("E???");
    EXPECT_THAT(errors.unexpected, ElementsAre("?"));
  }

  {
    parsed_error_list errors = parse_error_list("err0r");
    EXPECT_THAT(errors.unexpected, ElementsAre("0"));
  }

  {
    parsed_error_list errors = parse_error_list("err?r");
    EXPECT_THAT(errors.unexpected, ElementsAre("?"));
  }

  {
    parsed_error_list errors = parse_error_list("+err+r");
    EXPECT_THAT(errors.unexpected, ElementsAre("+"));
  }

  {
    parsed_error_list errors = parse_error_list("+%^#");
    EXPECT_THAT(errors.unexpected, ElementsAre("%"));
  }

  {
    parsed_error_list errors = parse_error_list("warning error");
    EXPECT_THAT(errors.unexpected, ElementsAre("e"));
  }
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
