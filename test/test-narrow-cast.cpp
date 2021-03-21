// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <limits>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
namespace {
using schar = signed char;
using llong = long long;

using uchar = unsigned char;
using ushort = unsigned short;
using uint = unsigned int;
using ulong = unsigned long;
using ullong = unsigned long long;

using schar_limits = std::numeric_limits<schar>;
using short_limits = std::numeric_limits<short>;
using int_limits = std::numeric_limits<int>;
using long_limits = std::numeric_limits<long>;
using llong_limits = std::numeric_limits<llong>;

using uchar_limits = std::numeric_limits<uchar>;
using ushort_limits = std::numeric_limits<ushort>;
using uint_limits = std::numeric_limits<uint>;
using ulong_limits = std::numeric_limits<ulong>;
using ullong_limits = std::numeric_limits<ullong>;

#if QLJS_HAVE_CHAR8_T
// NOTE(strager): Avoid using std::numeric_limits<char8_t> because it's buggy on
// some compilers.
using char8_t_limits = numeric_limits<char8_t>;
#endif

TEST(test_narrow_cast, same_type_signed_narrow_cast_never_fails) {
  EXPECT_EQ(narrow_cast<int>(int{0}), 0);
  EXPECT_EQ(narrow_cast<int>(int{1234}), 1234);
  EXPECT_EQ(narrow_cast<int>(int_limits::lowest()), int_limits::lowest());
  EXPECT_EQ(narrow_cast<int>(int_limits::max()), int_limits::max());

  EXPECT_EQ(narrow_cast<long>(long{0}), 0);
  EXPECT_EQ(narrow_cast<long>(long{1234}), 1234);
  EXPECT_EQ(narrow_cast<long>(long_limits::lowest()), long_limits::lowest());
  EXPECT_EQ(narrow_cast<long>(long_limits::max()), long_limits::max());

#if QLJS_HAVE_CHAR8_T
  EXPECT_EQ(narrow_cast<char8_t>(char8_t{0}), 0);
  EXPECT_EQ(narrow_cast<char8_t>(char8_t{123}), 123);
  EXPECT_EQ(narrow_cast<char8_t>(char8_t_limits::lowest()),
            char8_t_limits::lowest());
  EXPECT_EQ(narrow_cast<char8_t>(char8_t_limits::max()), char8_t_limits::max());
#endif
}

TEST(test_narrow_cast, same_type_unsigned_narrow_cast_never_fails) {
  EXPECT_EQ(narrow_cast<uint>(uint{0}), 0);
  EXPECT_EQ(narrow_cast<uint>(uint{1234}), 1234);
  EXPECT_EQ(narrow_cast<uint>(uint_limits::lowest()), uint_limits::lowest());
  EXPECT_EQ(narrow_cast<uint>(uint_limits::max()), uint_limits::max());

  EXPECT_EQ(narrow_cast<ulong>(ulong{0}), 0);
  EXPECT_EQ(narrow_cast<ulong>(ulong{1234}), 1234);
  EXPECT_EQ(narrow_cast<ulong>(ulong_limits::lowest()), ulong_limits::lowest());
  EXPECT_EQ(narrow_cast<ulong>(ulong_limits::max()), ulong_limits::max());
}

TEST(test_narrow_cast, signed_signed_narrow_cast_succeeds_if_in_range) {
  EXPECT_TRUE(in_range<schar>(short{schar_limits::lowest()}));
  EXPECT_TRUE(in_range<schar>(short{schar_limits::max()}));
  EXPECT_TRUE(in_range<schar>(int{schar_limits::lowest()}));
  EXPECT_TRUE(in_range<schar>(int{schar_limits::max()}));
  EXPECT_TRUE(in_range<schar>(long{schar_limits::lowest()}));
  EXPECT_TRUE(in_range<schar>(long{schar_limits::max()}));
  EXPECT_TRUE(in_range<schar>(llong{schar_limits::lowest()}));
  EXPECT_TRUE(in_range<schar>(llong{schar_limits::max()}));

  EXPECT_TRUE(in_range<short>(int{short_limits::lowest()}));
  EXPECT_TRUE(in_range<short>(int{short_limits::max()}));
  EXPECT_TRUE(in_range<short>(long{short_limits::lowest()}));
  EXPECT_TRUE(in_range<short>(long{short_limits::max()}));
  EXPECT_TRUE(in_range<short>(llong{short_limits::lowest()}));
  EXPECT_TRUE(in_range<short>(llong{short_limits::max()}));

  EXPECT_TRUE(in_range<int>(long{int_limits::lowest()}));
  EXPECT_TRUE(in_range<int>(long{int_limits::max()}));
  EXPECT_TRUE(in_range<int>(llong{int_limits::lowest()}));
  EXPECT_TRUE(in_range<int>(llong{int_limits::max()}));

  EXPECT_TRUE(in_range<long>(llong{long_limits::lowest()}));
  EXPECT_TRUE(in_range<long>(llong{long_limits::max()}));
}

TEST(test_narrow_cast, signed_signed_narrow_cast_fails_if_out_of_range) {
  static_assert(sizeof(schar) < sizeof(llong));
  EXPECT_FALSE(in_range<schar>(llong_limits::lowest()));
  EXPECT_FALSE(in_range<schar>(llong_limits::max()));
  static_assert(sizeof(schar) < sizeof(long));
  EXPECT_FALSE(in_range<schar>(long_limits::lowest()));
  EXPECT_FALSE(in_range<schar>(long_limits::max()));

  static_assert(sizeof(short) < sizeof(llong));
  EXPECT_FALSE(in_range<short>(llong_limits::lowest()));
  EXPECT_FALSE(in_range<short>(llong_limits::max()));
  static_assert(sizeof(short) < sizeof(long));
  EXPECT_FALSE(in_range<short>(long_limits::lowest()));
  EXPECT_FALSE(in_range<short>(long_limits::max()));

  static_assert(sizeof(int) < sizeof(llong));
  EXPECT_FALSE(in_range<int>(llong_limits::lowest()));
  EXPECT_FALSE(in_range<int>(llong_limits::max()));
}

TEST(test_narrow_cast, unsigned_unsigned_narrow_cast_succeeds_if_in_range) {
#if QLJS_HAVE_CHAR8_T
  EXPECT_TRUE(in_range<char8_t>(ushort{char8_t_limits::lowest()}));
  EXPECT_TRUE(in_range<char8_t>(ushort{char8_t_limits::max()}));
  EXPECT_TRUE(in_range<char8_t>(uint{char8_t_limits::lowest()}));
  EXPECT_TRUE(in_range<char8_t>(uint{char8_t_limits::max()}));
  EXPECT_TRUE(in_range<char8_t>(ulong{char8_t_limits::lowest()}));
  EXPECT_TRUE(in_range<char8_t>(ulong{char8_t_limits::max()}));
  EXPECT_TRUE(in_range<char8_t>(ullong{char8_t_limits::lowest()}));
  EXPECT_TRUE(in_range<char8_t>(ullong{char8_t_limits::max()}));
#endif

  EXPECT_TRUE(in_range<uchar>(ushort{uchar_limits::lowest()}));
  EXPECT_TRUE(in_range<uchar>(ushort{uchar_limits::max()}));
  EXPECT_TRUE(in_range<uchar>(uint{uchar_limits::lowest()}));
  EXPECT_TRUE(in_range<uchar>(uint{uchar_limits::max()}));
  EXPECT_TRUE(in_range<uchar>(ulong{uchar_limits::lowest()}));
  EXPECT_TRUE(in_range<uchar>(ulong{uchar_limits::max()}));
  EXPECT_TRUE(in_range<uchar>(ullong{uchar_limits::lowest()}));
  EXPECT_TRUE(in_range<uchar>(ullong{uchar_limits::max()}));

  EXPECT_TRUE(in_range<ushort>(uint{ushort_limits::lowest()}));
  EXPECT_TRUE(in_range<ushort>(uint{ushort_limits::max()}));
  EXPECT_TRUE(in_range<ushort>(ulong{ushort_limits::lowest()}));
  EXPECT_TRUE(in_range<ushort>(ulong{ushort_limits::max()}));
  EXPECT_TRUE(in_range<ushort>(ullong{ushort_limits::lowest()}));
  EXPECT_TRUE(in_range<ushort>(ullong{ushort_limits::max()}));

  EXPECT_TRUE(in_range<uint>(ulong{uint_limits::lowest()}));
  EXPECT_TRUE(in_range<uint>(ulong{uint_limits::max()}));
  EXPECT_TRUE(in_range<uint>(ullong{uint_limits::lowest()}));
  EXPECT_TRUE(in_range<uint>(ullong{uint_limits::max()}));

  EXPECT_TRUE(in_range<ulong>(ullong{ulong_limits::lowest()}));
  EXPECT_TRUE(in_range<ulong>(ullong{ulong_limits::max()}));
}

TEST(test_narrow_cast, unsigned_unsigned_narrow_cast_fails_if_out_of_range) {
  static_assert(sizeof(schar) < sizeof(llong));
  EXPECT_FALSE(in_range<schar>(llong_limits::max()));
  static_assert(sizeof(schar) < sizeof(long));
  EXPECT_FALSE(in_range<schar>(long_limits::max()));

  static_assert(sizeof(short) < sizeof(llong));
  EXPECT_FALSE(in_range<short>(llong_limits::max()));
  static_assert(sizeof(short) < sizeof(long));
  EXPECT_FALSE(in_range<short>(long_limits::lowest()));
  EXPECT_FALSE(in_range<short>(long_limits::max()));

  static_assert(sizeof(int) < sizeof(llong));
  EXPECT_FALSE(in_range<int>(llong_limits::max()));
}

TEST(test_narrow_cast, unsigned_signed_narrow_cast_succeeds_if_in_range) {
  EXPECT_TRUE(in_range<schar>(uchar{schar_limits::max()}));
  EXPECT_TRUE(in_range<schar>(ushort{schar_limits::max()}));
  EXPECT_TRUE(in_range<schar>(uint{schar_limits::max()}));
  EXPECT_TRUE(in_range<schar>(ulong{schar_limits::max()}));
  EXPECT_TRUE(in_range<schar>(ullong{schar_limits::max()}));

  EXPECT_TRUE(in_range<short>(ushort{short_limits::max()}));
  EXPECT_TRUE(in_range<short>(uint{short_limits::max()}));
  EXPECT_TRUE(in_range<short>(ulong{short_limits::max()}));
  EXPECT_TRUE(in_range<short>(ullong{short_limits::max()}));

  EXPECT_TRUE(in_range<int>(uint{int_limits::max()}));
  EXPECT_TRUE(in_range<int>(ulong{int_limits::max()}));
  EXPECT_TRUE(in_range<int>(ullong{int_limits::max()}));

  EXPECT_TRUE(in_range<long>(ulong{long_limits::max()}));
  EXPECT_TRUE(in_range<long>(ullong{long_limits::max()}));

  EXPECT_TRUE(in_range<llong>(ullong{llong_limits::max()}));
}

TEST(test_narrow_cast, unsigned_to_signed_narrow_cast_fails_if_out_of_range) {
  EXPECT_FALSE(in_range<schar>(uchar{uchar_limits::max()}));
  EXPECT_FALSE(in_range<schar>(ushort{uchar_limits::max()}));
  EXPECT_FALSE(in_range<schar>(uint{uchar_limits::max()}));
  EXPECT_FALSE(in_range<schar>(ulong{uchar_limits::max()}));
  EXPECT_FALSE(in_range<schar>(ullong{uchar_limits::max()}));

  EXPECT_FALSE(in_range<short>(ushort{ushort_limits::max()}));
  EXPECT_FALSE(in_range<short>(uint{ushort_limits::max()}));
  EXPECT_FALSE(in_range<short>(ulong{ushort_limits::max()}));
  EXPECT_FALSE(in_range<short>(ullong{ushort_limits::max()}));

  EXPECT_FALSE(in_range<int>(uint{uint_limits::max()}));
  EXPECT_FALSE(in_range<int>(ulong{uint_limits::max()}));
  EXPECT_FALSE(in_range<int>(ullong{uint_limits::max()}));

  EXPECT_FALSE(in_range<long>(ulong{ulong_limits::max()}));
  EXPECT_FALSE(in_range<long>(ullong{ulong_limits::max()}));

  EXPECT_FALSE(in_range<llong>(ullong{ullong_limits::max()}));
}

TEST(test_narrow_cast, signed_unsigned_narrow_cast_succeeds_if_in_range) {
#if QLJS_HAVE_CHAR8_T
  EXPECT_TRUE(in_range<char8_t>(schar{0}));
  EXPECT_TRUE(in_range<char8_t>(short{0}));
  EXPECT_TRUE(in_range<char8_t>(int{0}));
  EXPECT_TRUE(in_range<char8_t>(long{0}));
  EXPECT_TRUE(in_range<char8_t>(llong{0}));
#endif

  EXPECT_TRUE(in_range<uchar>(schar{0}));
  EXPECT_TRUE(in_range<uchar>(short{0}));
  EXPECT_TRUE(in_range<uchar>(int{0}));
  EXPECT_TRUE(in_range<uchar>(long{0}));
  EXPECT_TRUE(in_range<uchar>(llong{0}));

  EXPECT_TRUE(in_range<ushort>(schar{0}));
  EXPECT_TRUE(in_range<ushort>(short{0}));
  EXPECT_TRUE(in_range<ushort>(int{0}));
  EXPECT_TRUE(in_range<ushort>(long{0}));
  EXPECT_TRUE(in_range<ushort>(llong{0}));

  EXPECT_TRUE(in_range<uint>(schar{0}));
  EXPECT_TRUE(in_range<uint>(short{0}));
  EXPECT_TRUE(in_range<uint>(int{0}));
  EXPECT_TRUE(in_range<uint>(long{0}));
  EXPECT_TRUE(in_range<uint>(llong{0}));

  EXPECT_TRUE(in_range<ulong>(schar{0}));
  EXPECT_TRUE(in_range<ulong>(short{0}));
  EXPECT_TRUE(in_range<ulong>(int{0}));
  EXPECT_TRUE(in_range<ulong>(long{0}));
  EXPECT_TRUE(in_range<ulong>(llong{0}));

  EXPECT_TRUE(in_range<ullong>(schar{0}));
  EXPECT_TRUE(in_range<ullong>(short{0}));
  EXPECT_TRUE(in_range<ullong>(int{0}));
  EXPECT_TRUE(in_range<ullong>(long{0}));
  EXPECT_TRUE(in_range<ullong>(llong{0}));

#if QLJS_HAVE_CHAR8_T
  EXPECT_TRUE(in_range<char8_t>(schar{schar_limits::max()}));
#endif
  EXPECT_TRUE(in_range<uchar>(schar{schar_limits::max()}));
  EXPECT_TRUE(in_range<ushort>(short{short_limits::max()}));
  EXPECT_TRUE(in_range<uint>(int{int_limits::max()}));
  EXPECT_TRUE(in_range<ulong>(long{long_limits::max()}));
  EXPECT_TRUE(in_range<ullong>(llong{llong_limits::max()}));
}

TEST(test_narrow_cast,
     signed_to_unsigned_narrow_cast_fails_if_input_is_negative) {
#if QLJS_HAVE_CHAR8_T
  EXPECT_FALSE(in_range<char8_t>(schar{schar_limits::lowest()}));
  EXPECT_FALSE(in_range<char8_t>(short{short_limits::lowest()}));
  EXPECT_FALSE(in_range<char8_t>(int{int_limits::lowest()}));
  EXPECT_FALSE(in_range<char8_t>(long{long_limits::lowest()}));
  EXPECT_FALSE(in_range<char8_t>(llong{llong_limits::lowest()}));
#endif

  EXPECT_FALSE(in_range<uchar>(schar{schar_limits::lowest()}));
  EXPECT_FALSE(in_range<uchar>(short{short_limits::lowest()}));
  EXPECT_FALSE(in_range<uchar>(int{int_limits::lowest()}));
  EXPECT_FALSE(in_range<uchar>(long{long_limits::lowest()}));
  EXPECT_FALSE(in_range<uchar>(llong{llong_limits::lowest()}));

  EXPECT_FALSE(in_range<ushort>(schar{schar_limits::lowest()}));
  EXPECT_FALSE(in_range<ushort>(short{short_limits::lowest()}));
  EXPECT_FALSE(in_range<ushort>(int{int_limits::lowest()}));
  EXPECT_FALSE(in_range<ushort>(long{long_limits::lowest()}));
  EXPECT_FALSE(in_range<ushort>(llong{llong_limits::lowest()}));

  EXPECT_FALSE(in_range<uint>(schar{schar_limits::lowest()}));
  EXPECT_FALSE(in_range<uint>(short{short_limits::lowest()}));
  EXPECT_FALSE(in_range<uint>(int{int_limits::lowest()}));
  EXPECT_FALSE(in_range<uint>(long{long_limits::lowest()}));
  EXPECT_FALSE(in_range<uint>(llong{llong_limits::lowest()}));

  EXPECT_FALSE(in_range<ulong>(schar{schar_limits::lowest()}));
  EXPECT_FALSE(in_range<ulong>(short{short_limits::lowest()}));
  EXPECT_FALSE(in_range<ulong>(int{int_limits::lowest()}));
  EXPECT_FALSE(in_range<ulong>(long{long_limits::lowest()}));
  EXPECT_FALSE(in_range<ulong>(llong{llong_limits::lowest()}));

  EXPECT_FALSE(in_range<ullong>(schar{schar_limits::lowest()}));
  EXPECT_FALSE(in_range<ullong>(short{short_limits::lowest()}));
  EXPECT_FALSE(in_range<ullong>(int{int_limits::lowest()}));
  EXPECT_FALSE(in_range<ullong>(long{long_limits::lowest()}));
  EXPECT_FALSE(in_range<ullong>(llong{llong_limits::lowest()}));
}

TEST(test_narrow_cast, signed_to_unsigned_narrow_cast_fails_if_out_of_range) {
#if QLJS_HAVE_CHAR8_T
  static_assert(sizeof(short) > sizeof(char8_t));
  EXPECT_FALSE(in_range<char8_t>(short{short_limits::max()}));
  EXPECT_FALSE(in_range<char8_t>(int{int_limits::max()}));
  EXPECT_FALSE(in_range<char8_t>(long{long_limits::max()}));
  EXPECT_FALSE(in_range<char8_t>(llong{llong_limits::max()}));
#endif

  static_assert(sizeof(short) > sizeof(uchar));
  EXPECT_FALSE(in_range<uchar>(short{short_limits::max()}));
  EXPECT_FALSE(in_range<uchar>(int{int_limits::max()}));
  EXPECT_FALSE(in_range<uchar>(long{long_limits::max()}));
  EXPECT_FALSE(in_range<uchar>(llong{llong_limits::max()}));

  static_assert(sizeof(int) > sizeof(ushort));
  EXPECT_FALSE(in_range<ushort>(int{int_limits::max()}));
  EXPECT_FALSE(in_range<ushort>(long{long_limits::max()}));
  EXPECT_FALSE(in_range<ushort>(llong{llong_limits::max()}));

  if (sizeof(long) > sizeof(uint)) {
    EXPECT_FALSE(in_range<uint>(long{long_limits::max()}));
  }
  if (sizeof(llong) > sizeof(uint)) {
    EXPECT_FALSE(in_range<uint>(llong{llong_limits::max()}));
  }

  if (sizeof(llong) > sizeof(ulong)) {
    EXPECT_FALSE(in_range<ulong>(llong{llong_limits::max()}));
  }
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
