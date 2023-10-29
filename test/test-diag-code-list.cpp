// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/diag/diag-code-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
class Test_Diag_Code_List : public ::testing::Test {
 public:
  Monotonic_Allocator allocator{"Test_Diag_Code_List"};
};

TEST_F(Test_Diag_Code_List, compiled_default_matches_all_errors) {
  Compiled_Diag_Code_List errors;
#define QLJS_DIAG_TYPE_NAME(error_name) \
  EXPECT_TRUE(errors.is_present(Diag_Type::error_name)) << #error_name;
  QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

  EXPECT_THAT(errors.parse_errors("--testoption"), IsEmpty());
  EXPECT_THAT(errors.parse_warnings(), IsEmpty());
}

TEST_F(Test_Diag_Code_List, compiled_excluded_error_by_code) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .excluded_codes = Span<const std::string_view>({"E0003"}),
  });

  EXPECT_FALSE(errors.is_present(Diag_Type::Diag_Assignment_To_Const_Variable))
      << "E0003 should be disabled";
  EXPECT_TRUE(
      errors.is_present(Diag_Type::Diag_Big_Int_Literal_Contains_Decimal_Point))
      << "E0005 should be enabled";

  EXPECT_THAT(errors.parse_errors("--testoption"), IsEmpty());
  EXPECT_THAT(errors.parse_warnings(), IsEmpty());
}

TEST_F(Test_Diag_Code_List, compiled_excluded_then_included_error_by_code) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .excluded_codes = Span<const std::string_view>({"E0003"}),
  });
  errors.add(Parsed_Diag_Code_List{
      .included_codes = Span<const std::string_view>({"E0003"}),
  });

  EXPECT_TRUE(errors.is_present(Diag_Type::Diag_Assignment_To_Const_Variable))
      << "E0003 should be enabled";
}

TEST_F(Test_Diag_Code_List, compiled_included_then_excluded_error_by_code) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .included_codes = Span<const std::string_view>({"E0003"}),
  });
  errors.add(Parsed_Diag_Code_List{
      .excluded_codes = Span<const std::string_view>({"E0003"}),
  });

  EXPECT_FALSE(errors.is_present(Diag_Type::Diag_Assignment_To_Const_Variable))
      << "E0003 should be disabled";
}

TEST_F(Test_Diag_Code_List, compiled_exclude_all_matches_no_errors) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .excluded_categories = Span<const std::string_view>({"all"}),
  });

#define QLJS_DIAG_TYPE_NAME(error_name) \
  EXPECT_FALSE(errors.is_present(Diag_Type::error_name)) << #error_name;
  QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

  EXPECT_THAT(errors.parse_errors("--testoption"), IsEmpty());
  EXPECT_THAT(errors.parse_warnings(), IsEmpty());
}

TEST_F(Test_Diag_Code_List,
       compiled_override_and_include_code_matches_only_explicit) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .included_codes = Span<const std::string_view>({"E0003"}),
      .override_defaults = true,
  });

  EXPECT_TRUE(errors.is_present(Diag_Type::Diag_Assignment_To_Const_Variable))
      << "E0003 should be enabled";
  EXPECT_FALSE(
      errors.is_present(Diag_Type::Diag_Big_Int_Literal_Contains_Decimal_Point))
      << "E0005 (default) should be disabled";
}

TEST_F(Test_Diag_Code_List,
       compiled_include_code_then_override_matches_only_later_included_codes) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .included_codes = Span<const std::string_view>({"E0003"}),
  });
  errors.add(Parsed_Diag_Code_List{
      .included_codes = Span<const std::string_view>({"E0005"}),
      .override_defaults = true,
  });

  EXPECT_FALSE(errors.is_present(Diag_Type::Diag_Assignment_To_Const_Variable))
      << "E0003 should be disabled";
  EXPECT_TRUE(
      errors.is_present(Diag_Type::Diag_Big_Int_Literal_Contains_Decimal_Point))
      << "E0005 should be enabled";
}

TEST_F(Test_Diag_Code_List,
       compiled_exclude_all_and_include_code_matches_only_explicit) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .included_codes = Span<const std::string_view>({"E0003"}),
      .excluded_categories = Span<const std::string_view>({"all"}),
  });

  EXPECT_TRUE(errors.is_present(Diag_Type::Diag_Assignment_To_Const_Variable))
      << "E0003 should be enabled";
  EXPECT_FALSE(
      errors.is_present(Diag_Type::Diag_Big_Int_Literal_Contains_Decimal_Point))
      << "E0005 (all) should be disabled";
}

TEST_F(Test_Diag_Code_List,
       compiled_exclude_default_code_and_include_all_matches_excluded_code) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      // These codes are enabled by default.
      .excluded_codes = Span<const std::string_view>({"E0003", "E0005"}),
  });
  errors.add(Parsed_Diag_Code_List{
      .included_categories = Span<const std::string_view>({"all"}),
  });

  EXPECT_TRUE(errors.is_present(Diag_Type::Diag_Assignment_To_Const_Variable))
      << "E0003 (all) should be enabled";
  EXPECT_TRUE(
      errors.is_present(Diag_Type::Diag_Big_Int_Literal_Contains_Decimal_Point))
      << "E0005 (all) should be enabled";
}

TEST_F(Test_Diag_Code_List, compiling_invalid_category_is_an_error) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .included_categories = Span<const std::string_view>({"banana"}),
      .excluded_categories = Span<const std::string_view>({"strawberry"}),
  });

  EXPECT_THAT(errors.parse_warnings(), UnorderedElementsAreArray({
                                           "unknown error category: banana",
                                           "unknown error category: strawberry",
                                       }));
}

TEST_F(Test_Diag_Code_List, compiling_invalid_code_is_an_error) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .included_codes = Span<const std::string_view>({"E9999"}),
      .excluded_codes = Span<const std::string_view>({"E0000"}),
  });

  EXPECT_THAT(errors.parse_warnings(), UnorderedElementsAreArray({
                                           "unknown error code: E9999",
                                           "unknown error code: E0000",
                                       }));
}

TEST_F(Test_Diag_Code_List, compiling_empty_parsed_diag_code_list_is_an_error) {
  Compiled_Diag_Code_List errors;
  errors.add(Parsed_Diag_Code_List{
      .included_codes = Span<const std::string_view>({"E0003"}),
  });
  errors.add(Parsed_Diag_Code_List{});
  errors.add(Parsed_Diag_Code_List{
      .excluded_codes = Span<const std::string_view>({"E0003"}),
  });

  EXPECT_THAT(errors.parse_warnings(), IsEmpty());
  EXPECT_THAT(
      errors.parse_errors("--testoption"),
      ElementsAreArray(
          {"--testoption must be given at least one category or code"}));
}

TEST_F(Test_Diag_Code_List, empty_list_is_disallowed) {
  {
    Parsed_Diag_Code_List errors = parse_diag_code_list("", &this->allocator);
    EXPECT_TRUE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("    \t ", &this->allocator);
    EXPECT_TRUE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, add_error_code_to_default) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("+E0420", &this->allocator);
    EXPECT_THAT(errors.included_codes, ElementsAreArray({"E0420"}));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("+E0420,+E0069", &this->allocator);
    EXPECT_THAT(errors.included_codes,
                UnorderedElementsAreArray({"E0420", "E0069"}));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, remove_error_code_from_default) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("-E0420", &this->allocator);
    EXPECT_THAT(errors.excluded_codes, ElementsAreArray({"E0420"}));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("-E0420,-E0069", &this->allocator);
    EXPECT_THAT(errors.excluded_codes,
                UnorderedElementsAreArray({"E0420", "E0069"}));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, add_error_category_to_default) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("+warning", &this->allocator);
    EXPECT_THAT(errors.included_categories, ElementsAreArray({"warning"}));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, remove_error_category_from_default) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("-warning", &this->allocator);
    EXPECT_THAT(errors.excluded_categories, ElementsAreArray({"warning"}));
    EXPECT_FALSE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, set_default_to_category) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("warning", &this->allocator);
    EXPECT_THAT(errors.included_categories, ElementsAreArray({"warning"}));
    EXPECT_TRUE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, set_default_to_error_code) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("E0420", &this->allocator);
    EXPECT_THAT(errors.included_codes, ElementsAreArray({"E0420"}));
    EXPECT_TRUE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, mixed) {
  {
    Parsed_Diag_Code_List errors = parse_diag_code_list(
        "+E0420,warning,-pedantic,syntax-error", &this->allocator);
    EXPECT_THAT(errors.included_codes, ElementsAreArray({"E0420"}));
    EXPECT_THAT(errors.excluded_codes, IsEmpty());
    EXPECT_THAT(errors.included_categories,
                ElementsAreArray({"warning", "syntax-error"}));
    EXPECT_THAT(errors.excluded_categories, ElementsAreArray({"pedantic"}));
    EXPECT_TRUE(errors.override_defaults);
    EXPECT_FALSE(errors.error_missing_predicate());
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, whitespace_around_predicates_is_ignored) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("\t +E0420 \t", &this->allocator);
    EXPECT_THAT(errors.included_codes, ElementsAreArray({"E0420"}));
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list(" +E0420 , +E0069", &this->allocator);
    EXPECT_THAT(errors.included_codes,
                UnorderedElementsAreArray({"E0420", "E0069"}));
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, stray_commas_are_ignored) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list(",one,,two , three", &this->allocator);
    EXPECT_THAT(errors.included_categories,
                UnorderedElementsAreArray({"one", "two", "three"}));
    EXPECT_THAT(errors.unexpected, IsEmpty());
  }
}

TEST_F(Test_Diag_Code_List, unexpected_characters) {
  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("~E0420", &this->allocator);
    EXPECT_THAT(errors.unexpected, ElementsAreArray({"~"}));
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("E???", &this->allocator);
    EXPECT_THAT(errors.unexpected, ElementsAreArray({"?"}));
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("err0r", &this->allocator);
    EXPECT_THAT(errors.unexpected, ElementsAreArray({"0"}));
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("err?r", &this->allocator);
    EXPECT_THAT(errors.unexpected, ElementsAreArray({"?"}));
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("+err+r", &this->allocator);
    EXPECT_THAT(errors.unexpected, ElementsAreArray({"+"}));
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("+%^#", &this->allocator);
    EXPECT_THAT(errors.unexpected, ElementsAreArray({"%"}));
  }

  {
    Parsed_Diag_Code_List errors =
        parse_diag_code_list("warning error", &this->allocator);
    EXPECT_THAT(errors.unexpected, ElementsAreArray({"e"}));
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
