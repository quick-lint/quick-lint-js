// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <gmock/gmock.h>
#include <optional>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/port/unreachable.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
template <class Value>
std::string get_matcher_message(::testing::Matcher<const Value &> matcher,
                                const Value &value) {
  ::testing::StringMatchResultListener listener;
  ExplainMatchResult(matcher, value, &listener);
  return listener.str();
}

TEST(Test_Diag_Matcher, match_error_type) {
  Padded_String code(u8"hello"_sv);

  ::testing::Matcher<const Diag_Collector::Diag &> continue_matcher =
      DIAG_TYPE(Diag_Invalid_Continue);
  EXPECT_TRUE(
      continue_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[0], &code[5]),
      })));
  EXPECT_FALSE(continue_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Break{
      .break_statement = Source_Code_Span(&code[0], &code[5]),
  })));

  ::testing::Matcher<const Diag_Collector::Diag &> break_matcher =
      DIAG_TYPE(Diag_Invalid_Break);
  EXPECT_FALSE(break_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Continue{
      .continue_statement = Source_Code_Span(&code[0], &code[5]),
  })));
  EXPECT_TRUE(break_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Break{
      .break_statement = Source_Code_Span(&code[0], &code[5]),
  })));
}

TEST(Test_Diag_Matcher, match_error_type_message) {
  Padded_String code(u8"hello"_sv);
  ::testing::Matcher<const Diag_Collector::Diag &> matcher =
      DIAG_TYPE(Diag_Invalid_Break);
  Diag_Collector::Diag value(Diag_Invalid_Continue{
      .continue_statement = Source_Code_Span(&code[0], &code[5]),
  });
  EXPECT_EQ(get_matcher_message(matcher, value),
            "whose type (Diag_Invalid_Continue) isn't Diag_Invalid_Break");
}

TEST(Test_Diag_Matcher, match_error_type_with_1_field) {
  Padded_String code(u8"hello"_sv);

  ::testing::Matcher<const Diag_Collector::Diag &> continue_matcher =
      DIAG_TYPE_OFFSETS(&code, Diag_Invalid_Continue,  //
                        continue_statement, 0, u8"hello"_sv);
  EXPECT_TRUE(
      continue_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[0], &code[5]),
      })));
  EXPECT_FALSE(continue_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Break{
      .break_statement = Source_Code_Span(&code[0], &code[5]),
  })));

  ::testing::Matcher<const Diag_Collector::Diag &> break_matcher =
      DIAG_TYPE_OFFSETS(&code, Diag_Invalid_Break,  //
                        break_statement, 0, u8"hello"_sv);
  EXPECT_FALSE(break_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Continue{
      .continue_statement = Source_Code_Span(&code[0], &code[5]),
  })));
  EXPECT_TRUE(break_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Break{
      .break_statement = Source_Code_Span(&code[0], &code[5]),
  })));
}

TEST(Test_Diag_Matcher, match_error_type_with_1_field_message) {
  Padded_String code(u8"hello"_sv);
  ::testing::Matcher<const Diag_Collector::Diag &> matcher =
      DIAG_TYPE_OFFSETS(&code, Diag_Invalid_Continue,  //
                        continue_statement, 0, u8"hello"_sv);
  Diag_Collector::Diag value(Diag_Invalid_Break{
      .break_statement = Source_Code_Span(&code[0], &code[5]),
  });
  EXPECT_EQ(get_matcher_message(matcher, value),
            "whose type (Diag_Invalid_Break) isn't Diag_Invalid_Continue");
}

TEST(Test_Diag_Matcher, match_offsets_of_1_field_span) {
  Padded_String code(u8"hello"_sv);

  ::testing::Matcher<const Diag_Collector::Diag &> continue_matcher =
      DIAG_TYPE_OFFSETS(&code, Diag_Invalid_Continue,  //
                        continue_statement, 1, u8"ello"_sv);
  EXPECT_TRUE(
      continue_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[1], &code[5]),
      })));
  EXPECT_FALSE(
      continue_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[0], &code[5]),
      })));
  EXPECT_FALSE(
      continue_matcher.Matches(Diag_Collector::Diag(Diag_Invalid_Continue{
          .continue_statement = Source_Code_Span(&code[0], &code[4]),
      })));
}

TEST(Test_Diag_Matcher, match_offsets_of_1_field_message) {
  Padded_String code(u8"hello"_sv);
  {
    ::testing::Matcher<const Diag_Collector::Diag &> matcher =
        DIAG_TYPE_OFFSETS(&code, Diag_Invalid_Continue,  //
                          continue_statement, 0, u8"hello"_sv);
    Diag_Collector::Diag value(Diag_Invalid_Continue{
        .continue_statement = Source_Code_Span(&code[1], &code[4]),
    });
    EXPECT_EQ(get_matcher_message(matcher, value),
              "whose .continue_statement (1-4) doesn't equal 0-5");
  }

  {
    ::testing::Matcher<const Diag_Collector::Diag &> matcher =
        DIAG_TYPE_OFFSETS(&code, Diag_Invalid_Break,  //
                          break_statement, 0, u8"hello"_sv);
    Diag_Collector::Diag value(Diag_Invalid_Break{
        .break_statement = Source_Code_Span(&code[1], &code[4]),
    });
    EXPECT_EQ(get_matcher_message(matcher, value),
              "whose .break_statement (1-4) doesn't equal 0-5");
  }
}

TEST(Test_Diag_Matcher, match_offsets_of_2_fields_span) {
  Padded_String code(u8"...x,"_sv);

  ::testing::Matcher<const Diag_Collector::Diag &> matcher =
      DIAG_TYPE_2_OFFSETS(&code,
                          Diag_Comma_Not_Allowed_After_Spread_Parameter,  //
                          comma, u8"...x"_sv.size(), u8","_sv, spread, 0,
                          u8"..."_sv);
  EXPECT_TRUE(matcher.Matches(
      Diag_Collector::Diag(Diag_Comma_Not_Allowed_After_Spread_Parameter{
          .comma = Source_Code_Span(&code[4], &code[5]),
          .spread = Source_Code_Span(&code[0], &code[3]),
      })));
  EXPECT_FALSE(matcher.Matches(
      Diag_Collector::Diag(Diag_Comma_Not_Allowed_After_Spread_Parameter{
          .comma = Source_Code_Span(&code[3], &code[5]),
          .spread = Source_Code_Span(&code[0], &code[3]),
      })))
      << "when first doesn't match";
  EXPECT_FALSE(matcher.Matches(
      Diag_Collector::Diag(Diag_Comma_Not_Allowed_After_Spread_Parameter{
          .comma = Source_Code_Span(&code[4], &code[5]),
          .spread = Source_Code_Span(&code[1], &code[3]),
      })))
      << "when second doesn't match";
}

TEST(Test_Diag_Matcher, match_offsets_of_2_fields_message) {
  Padded_String code(u8"...x,"_sv);

  // Two wrong fields:
  {
    ::testing::Matcher<const Diag_Collector::Diag &> matcher =
        DIAG_TYPE_2_OFFSETS(&code,
                            Diag_Comma_Not_Allowed_After_Spread_Parameter,  //
                            comma, u8"...x"_sv.size(), u8","_sv, spread, 0,
                            u8"..."_sv);
    Diag_Collector::Diag value(Diag_Comma_Not_Allowed_After_Spread_Parameter{
        .comma = Source_Code_Span(&code[3], &code[5]),
        .spread = Source_Code_Span(&code[1], &code[3]),
    });
    EXPECT_EQ(get_matcher_message(matcher, value),
              "whose .comma (3-5) doesn't equal 4-5 and whose .spread (1-3) "
              "doesn't equal 0-3");
  }

  // Only first field is wrong:
  {
    ::testing::Matcher<const Diag_Collector::Diag &> matcher =
        DIAG_TYPE_2_OFFSETS(&code,
                            Diag_Comma_Not_Allowed_After_Spread_Parameter,  //
                            comma, u8"...x"_sv.size(), u8","_sv, spread, 0,
                            u8"..."_sv);
    Diag_Collector::Diag value(Diag_Comma_Not_Allowed_After_Spread_Parameter{
        .comma = Source_Code_Span(&code[3], &code[5]),
        .spread = Source_Code_Span(&code[0], &code[3]),
    });
    EXPECT_EQ(get_matcher_message(matcher, value),
              "whose .comma (3-5) doesn't equal 4-5 and whose .spread (0-3) "
              "equals 0-3");
  }

  // Only second field is wrong:
  {
    ::testing::Matcher<const Diag_Collector::Diag &> matcher =
        DIAG_TYPE_2_OFFSETS(&code,
                            Diag_Comma_Not_Allowed_After_Spread_Parameter,  //
                            comma, u8"...x"_sv.size(), u8","_sv, spread, 0,
                            u8"..."_sv);
    Diag_Collector::Diag value(Diag_Comma_Not_Allowed_After_Spread_Parameter{
        .comma = Source_Code_Span(&code[4], &code[5]),
        .spread = Source_Code_Span(&code[1], &code[3]),
    });
    EXPECT_EQ(get_matcher_message(matcher, value),
              "whose .comma (4-5) equals 4-5 and whose .spread (1-3) doesn't "
              "equal 0-3");
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
