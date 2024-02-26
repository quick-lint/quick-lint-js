// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/port/have.h>
#include <string>
#include <string_view>

namespace testing::internal {
template <>
void PrintTo(const char32_t &, std::ostream *);

#if QLJS_HAVE_CHAR8_T
template <>
void PrintTo(const std::basic_string<char8_t> &, std::ostream *);
template <>
void PrintTo(const std::basic_string_view<char8_t> &, std::ostream *);
#endif

template <>
inline void PrintTo(const char32_t &c, std::ostream *out) {
  PrintTo(static_cast<std::uint_least32_t>(c), out);
}

#if QLJS_HAVE_CHAR8_T
template <>
inline void PrintTo(const std::basic_string<char8_t> &s, std::ostream *out) {
  PrintTo(std::basic_string_view<char8_t>(s), out);
}

template <>
inline void PrintTo(const std::basic_string_view<char8_t> &s,
                    std::ostream *out) {
  PrintTo(std::string_view(reinterpret_cast<const char *>(s.data()), s.size()),
          out);
}
#endif

template <class Value>
std::string get_matcher_description(::testing::Matcher<const Value &> matcher) {
  std::ostringstream ss;
  matcher.DescribeTo(&ss);
  return ss.str();
}

template <class Value>
std::string get_matcher_message(::testing::Matcher<const Value &> matcher,
                                const Value &value) {
  ::testing::StringMatchResultListener listener;
  ExplainMatchResult(matcher, value, &listener);
  return listener.str();
}

// Like EXPECT_THAT, but using the 'caller' variable for source locations.
#define EXPECT_THAT_AT_CALLER(value, matcher)                                 \
  GTEST_PRED_FORMAT1_(                                                        \
      ::testing::internal::MakePredicateFormatterFromMatcher(matcher), value, \
      ADD_FAILURE_AT_CALLER)

// Like ASSERT_EQ, but using the 'caller' variable for source locations.
#define ASSERT_NE_AT_CALLER(lhs, rhs)                             \
  GTEST_PRED_FORMAT2_(::testing::internal::CmpHelperNE, lhs, rhs, \
                      GTEST_FAIL_AT_CALLER)

// Like EXPECT_EQ, but using the 'caller' variable for source locations.
#define EXPECT_EQ_AT_CALLER(lhs, rhs)                                   \
  GTEST_PRED_FORMAT2_(::testing::internal::EqHelper::Compare, lhs, rhs, \
                      ADD_FAILURE_AT_CALLER)

// Like EXPECT_TRUE, but using the 'caller' variable for source locations.
#define EXPECT_TRUE_AT_CALLER(value) EXPECT_EQ_AT_CALLER(value, true)

// Like ADD_FAILURE, but using the 'caller' variable for source locations.
#define ADD_FAILURE_AT_CALLER(message)   \
  ADD_FAILURE_OR_FAIL_AT_CALLER(message, \
                                ::testing::TestPartResult::kNonFatalFailure)

// Like GTEST_FAIL, but using the 'caller' variable for source locations.
#define GTEST_FAIL_AT_CALLER(message)    \
  ADD_FAILURE_OR_FAIL_AT_CALLER(message, \
                                ::testing::TestPartResult::kFatalFailure)

#define ADD_FAILURE_OR_FAIL_AT_CALLER(message, failure_kind)             \
  GTEST_MESSAGE_AT_(                                                     \
      (caller.valid() ? caller.file_name() : __FILE__),                  \
      (caller.valid() ? ::quick_lint_js::narrow_cast<int>(caller.line()) \
                      : __LINE__),                                       \
      message, failure_kind)
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
