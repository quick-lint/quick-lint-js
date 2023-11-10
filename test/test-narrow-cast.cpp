// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <limits>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/cast.h>

namespace quick_lint_js {
namespace {
using SChar = signed char;
using LLong = long long;

using UChar = unsigned char;
using UShort = unsigned short;
using UInt = unsigned int;
using ULong = unsigned long;
using ULLong = unsigned long long;

using SChar_Limits = std::numeric_limits<SChar>;
using Short_Limits = std::numeric_limits<short>;
using Int_Limits = std::numeric_limits<int>;
using Long_Limits = std::numeric_limits<long>;
using LLong_Limits = std::numeric_limits<LLong>;

using UChar_Limits = std::numeric_limits<UChar>;
using UShort_Limits = std::numeric_limits<UShort>;
using UInt_Limits = std::numeric_limits<UInt>;
using ULong_Limits = std::numeric_limits<ULong>;
using ULLong_Limits = std::numeric_limits<ULLong>;

#if QLJS_HAVE_CHAR8_T
// NOTE(strager): Avoid using std::numeric_limits<char8_t> because it's buggy on
// some compilers.
using Char8_T_Limits = Numeric_Limits<char8_t>;
#endif

TEST(Test_Narrow_Cast, same_type_signed_narrow_cast_never_fails) {
  EXPECT_EQ(narrow_cast<int>(int{0}), 0);
  EXPECT_EQ(narrow_cast<int>(int{1234}), 1234);
  EXPECT_EQ(narrow_cast<int>(Int_Limits::lowest()), Int_Limits::lowest());
  EXPECT_EQ(narrow_cast<int>(Int_Limits::max()), Int_Limits::max());

  EXPECT_EQ(narrow_cast<long>(long{0}), 0);
  EXPECT_EQ(narrow_cast<long>(long{1234}), 1234);
  EXPECT_EQ(narrow_cast<long>(Long_Limits::lowest()), Long_Limits::lowest());
  EXPECT_EQ(narrow_cast<long>(Long_Limits::max()), Long_Limits::max());

#if QLJS_HAVE_CHAR8_T
  EXPECT_EQ(narrow_cast<char8_t>(char8_t{0}), 0);
  EXPECT_EQ(narrow_cast<char8_t>(char8_t{123}), 123);
  EXPECT_EQ(narrow_cast<char8_t>(Char8_T_Limits::lowest()),
            Char8_T_Limits::lowest());
  EXPECT_EQ(narrow_cast<char8_t>(Char8_T_Limits::max()), Char8_T_Limits::max());
#endif
}

TEST(Test_Narrow_Cast, same_type_unsigned_narrow_cast_never_fails) {
  EXPECT_EQ(narrow_cast<UInt>(UInt{0}), 0);
  EXPECT_EQ(narrow_cast<UInt>(UInt{1234}), 1234);
  EXPECT_EQ(narrow_cast<UInt>(UInt_Limits::lowest()), UInt_Limits::lowest());
  EXPECT_EQ(narrow_cast<UInt>(UInt_Limits::max()), UInt_Limits::max());

  EXPECT_EQ(narrow_cast<ULong>(ULong{0}), 0);
  EXPECT_EQ(narrow_cast<ULong>(ULong{1234}), 1234);
  EXPECT_EQ(narrow_cast<ULong>(ULong_Limits::lowest()), ULong_Limits::lowest());
  EXPECT_EQ(narrow_cast<ULong>(ULong_Limits::max()), ULong_Limits::max());
}

TEST(Test_Narrow_Cast, signed_signed_narrow_cast_succeeds_if_in_range) {
  EXPECT_TRUE(in_range<SChar>(short{SChar_Limits::lowest()}));
  EXPECT_TRUE(in_range<SChar>(short{SChar_Limits::max()}));
  EXPECT_TRUE(in_range<SChar>(int{SChar_Limits::lowest()}));
  EXPECT_TRUE(in_range<SChar>(int{SChar_Limits::max()}));
  EXPECT_TRUE(in_range<SChar>(long{SChar_Limits::lowest()}));
  EXPECT_TRUE(in_range<SChar>(long{SChar_Limits::max()}));
  EXPECT_TRUE(in_range<SChar>(LLong{SChar_Limits::lowest()}));
  EXPECT_TRUE(in_range<SChar>(LLong{SChar_Limits::max()}));

  EXPECT_TRUE(in_range<short>(int{Short_Limits::lowest()}));
  EXPECT_TRUE(in_range<short>(int{Short_Limits::max()}));
  EXPECT_TRUE(in_range<short>(long{Short_Limits::lowest()}));
  EXPECT_TRUE(in_range<short>(long{Short_Limits::max()}));
  EXPECT_TRUE(in_range<short>(LLong{Short_Limits::lowest()}));
  EXPECT_TRUE(in_range<short>(LLong{Short_Limits::max()}));

  EXPECT_TRUE(in_range<int>(long{Int_Limits::lowest()}));
  EXPECT_TRUE(in_range<int>(long{Int_Limits::max()}));
  EXPECT_TRUE(in_range<int>(LLong{Int_Limits::lowest()}));
  EXPECT_TRUE(in_range<int>(LLong{Int_Limits::max()}));

  EXPECT_TRUE(in_range<long>(LLong{Long_Limits::lowest()}));
  EXPECT_TRUE(in_range<long>(LLong{Long_Limits::max()}));
}

TEST(Test_Narrow_Cast, signed_signed_narrow_cast_fails_if_out_of_range) {
  static_assert(sizeof(SChar) < sizeof(LLong));
  EXPECT_FALSE(in_range<SChar>(LLong_Limits::lowest()));
  EXPECT_FALSE(in_range<SChar>(LLong_Limits::max()));
  static_assert(sizeof(SChar) < sizeof(long));
  EXPECT_FALSE(in_range<SChar>(Long_Limits::lowest()));
  EXPECT_FALSE(in_range<SChar>(Long_Limits::max()));

  static_assert(sizeof(short) < sizeof(LLong));
  EXPECT_FALSE(in_range<short>(LLong_Limits::lowest()));
  EXPECT_FALSE(in_range<short>(LLong_Limits::max()));
  static_assert(sizeof(short) < sizeof(long));
  EXPECT_FALSE(in_range<short>(Long_Limits::lowest()));
  EXPECT_FALSE(in_range<short>(Long_Limits::max()));

  static_assert(sizeof(int) < sizeof(LLong));
  EXPECT_FALSE(in_range<int>(LLong_Limits::lowest()));
  EXPECT_FALSE(in_range<int>(LLong_Limits::max()));
}

TEST(Test_Narrow_Cast, unsigned_unsigned_narrow_cast_succeeds_if_in_range) {
#if QLJS_HAVE_CHAR8_T
  EXPECT_TRUE(in_range<char8_t>(UShort{Char8_T_Limits::lowest()}));
  EXPECT_TRUE(in_range<char8_t>(UShort{Char8_T_Limits::max()}));
  EXPECT_TRUE(in_range<char8_t>(UInt{Char8_T_Limits::lowest()}));
  EXPECT_TRUE(in_range<char8_t>(UInt{Char8_T_Limits::max()}));
  EXPECT_TRUE(in_range<char8_t>(ULong{Char8_T_Limits::lowest()}));
  EXPECT_TRUE(in_range<char8_t>(ULong{Char8_T_Limits::max()}));
  EXPECT_TRUE(in_range<char8_t>(ULLong{Char8_T_Limits::lowest()}));
  EXPECT_TRUE(in_range<char8_t>(ULLong{Char8_T_Limits::max()}));
#endif

  EXPECT_TRUE(in_range<UChar>(UShort{UChar_Limits::lowest()}));
  EXPECT_TRUE(in_range<UChar>(UShort{UChar_Limits::max()}));
  EXPECT_TRUE(in_range<UChar>(UInt{UChar_Limits::lowest()}));
  EXPECT_TRUE(in_range<UChar>(UInt{UChar_Limits::max()}));
  EXPECT_TRUE(in_range<UChar>(ULong{UChar_Limits::lowest()}));
  EXPECT_TRUE(in_range<UChar>(ULong{UChar_Limits::max()}));
  EXPECT_TRUE(in_range<UChar>(ULLong{UChar_Limits::lowest()}));
  EXPECT_TRUE(in_range<UChar>(ULLong{UChar_Limits::max()}));

  EXPECT_TRUE(in_range<UShort>(UInt{UShort_Limits::lowest()}));
  EXPECT_TRUE(in_range<UShort>(UInt{UShort_Limits::max()}));
  EXPECT_TRUE(in_range<UShort>(ULong{UShort_Limits::lowest()}));
  EXPECT_TRUE(in_range<UShort>(ULong{UShort_Limits::max()}));
  EXPECT_TRUE(in_range<UShort>(ULLong{UShort_Limits::lowest()}));
  EXPECT_TRUE(in_range<UShort>(ULLong{UShort_Limits::max()}));

  EXPECT_TRUE(in_range<UInt>(ULong{UInt_Limits::lowest()}));
  EXPECT_TRUE(in_range<UInt>(ULong{UInt_Limits::max()}));
  EXPECT_TRUE(in_range<UInt>(ULLong{UInt_Limits::lowest()}));
  EXPECT_TRUE(in_range<UInt>(ULLong{UInt_Limits::max()}));

  EXPECT_TRUE(in_range<ULong>(ULLong{ULong_Limits::lowest()}));
  EXPECT_TRUE(in_range<ULong>(ULLong{ULong_Limits::max()}));
}

TEST(Test_Narrow_Cast, unsigned_unsigned_narrow_cast_fails_if_out_of_range) {
  static_assert(sizeof(SChar) < sizeof(LLong));
  EXPECT_FALSE(in_range<SChar>(LLong_Limits::max()));
  static_assert(sizeof(SChar) < sizeof(long));
  EXPECT_FALSE(in_range<SChar>(Long_Limits::max()));

  static_assert(sizeof(short) < sizeof(LLong));
  EXPECT_FALSE(in_range<short>(LLong_Limits::max()));
  static_assert(sizeof(short) < sizeof(long));
  EXPECT_FALSE(in_range<short>(Long_Limits::lowest()));
  EXPECT_FALSE(in_range<short>(Long_Limits::max()));

  static_assert(sizeof(int) < sizeof(LLong));
  EXPECT_FALSE(in_range<int>(LLong_Limits::max()));
}

TEST(Test_Narrow_Cast, unsigned_signed_narrow_cast_succeeds_if_in_range) {
  EXPECT_TRUE(in_range<SChar>(UChar{SChar_Limits::max()}));
  EXPECT_TRUE(in_range<SChar>(UShort{SChar_Limits::max()}));
  EXPECT_TRUE(in_range<SChar>(UInt{SChar_Limits::max()}));
  EXPECT_TRUE(in_range<SChar>(ULong{SChar_Limits::max()}));
  EXPECT_TRUE(in_range<SChar>(ULLong{SChar_Limits::max()}));

  EXPECT_TRUE(in_range<short>(UShort{Short_Limits::max()}));
  EXPECT_TRUE(in_range<short>(UInt{Short_Limits::max()}));
  EXPECT_TRUE(in_range<short>(ULong{Short_Limits::max()}));
  EXPECT_TRUE(in_range<short>(ULLong{Short_Limits::max()}));

  EXPECT_TRUE(in_range<int>(UInt{Int_Limits::max()}));
  EXPECT_TRUE(in_range<int>(ULong{Int_Limits::max()}));
  EXPECT_TRUE(in_range<int>(ULLong{Int_Limits::max()}));

  EXPECT_TRUE(in_range<long>(ULong{Long_Limits::max()}));
  EXPECT_TRUE(in_range<long>(ULLong{Long_Limits::max()}));

  EXPECT_TRUE(in_range<LLong>(ULLong{LLong_Limits::max()}));
}

TEST(Test_Narrow_Cast, unsigned_to_signed_narrow_cast_fails_if_out_of_range) {
  EXPECT_FALSE(in_range<SChar>(UChar{UChar_Limits::max()}));
  EXPECT_FALSE(in_range<SChar>(UShort{UChar_Limits::max()}));
  EXPECT_FALSE(in_range<SChar>(UInt{UChar_Limits::max()}));
  EXPECT_FALSE(in_range<SChar>(ULong{UChar_Limits::max()}));
  EXPECT_FALSE(in_range<SChar>(ULLong{UChar_Limits::max()}));

  EXPECT_FALSE(in_range<short>(UShort{UShort_Limits::max()}));
  EXPECT_FALSE(in_range<short>(UInt{UShort_Limits::max()}));
  EXPECT_FALSE(in_range<short>(ULong{UShort_Limits::max()}));
  EXPECT_FALSE(in_range<short>(ULLong{UShort_Limits::max()}));

  EXPECT_FALSE(in_range<int>(UInt{UInt_Limits::max()}));
  EXPECT_FALSE(in_range<int>(ULong{UInt_Limits::max()}));
  EXPECT_FALSE(in_range<int>(ULLong{UInt_Limits::max()}));

  EXPECT_FALSE(in_range<long>(ULong{ULong_Limits::max()}));
  EXPECT_FALSE(in_range<long>(ULLong{ULong_Limits::max()}));

  EXPECT_FALSE(in_range<LLong>(ULLong{ULLong_Limits::max()}));
}

TEST(Test_Narrow_Cast, signed_unsigned_narrow_cast_succeeds_if_in_range) {
#if QLJS_HAVE_CHAR8_T
  EXPECT_TRUE(in_range<char8_t>(SChar{0}));
  EXPECT_TRUE(in_range<char8_t>(short{0}));
  EXPECT_TRUE(in_range<char8_t>(int{0}));
  EXPECT_TRUE(in_range<char8_t>(long{0}));
  EXPECT_TRUE(in_range<char8_t>(LLong{0}));
#endif

  EXPECT_TRUE(in_range<UChar>(SChar{0}));
  EXPECT_TRUE(in_range<UChar>(short{0}));
  EXPECT_TRUE(in_range<UChar>(int{0}));
  EXPECT_TRUE(in_range<UChar>(long{0}));
  EXPECT_TRUE(in_range<UChar>(LLong{0}));

  EXPECT_TRUE(in_range<UShort>(SChar{0}));
  EXPECT_TRUE(in_range<UShort>(short{0}));
  EXPECT_TRUE(in_range<UShort>(int{0}));
  EXPECT_TRUE(in_range<UShort>(long{0}));
  EXPECT_TRUE(in_range<UShort>(LLong{0}));

  EXPECT_TRUE(in_range<UInt>(SChar{0}));
  EXPECT_TRUE(in_range<UInt>(short{0}));
  EXPECT_TRUE(in_range<UInt>(int{0}));
  EXPECT_TRUE(in_range<UInt>(long{0}));
  EXPECT_TRUE(in_range<UInt>(LLong{0}));

  EXPECT_TRUE(in_range<ULong>(SChar{0}));
  EXPECT_TRUE(in_range<ULong>(short{0}));
  EXPECT_TRUE(in_range<ULong>(int{0}));
  EXPECT_TRUE(in_range<ULong>(long{0}));
  EXPECT_TRUE(in_range<ULong>(LLong{0}));

  EXPECT_TRUE(in_range<ULLong>(SChar{0}));
  EXPECT_TRUE(in_range<ULLong>(short{0}));
  EXPECT_TRUE(in_range<ULLong>(int{0}));
  EXPECT_TRUE(in_range<ULLong>(long{0}));
  EXPECT_TRUE(in_range<ULLong>(LLong{0}));

#if QLJS_HAVE_CHAR8_T
  EXPECT_TRUE(in_range<char8_t>(SChar{SChar_Limits::max()}));
#endif
  EXPECT_TRUE(in_range<UChar>(SChar{SChar_Limits::max()}));
  EXPECT_TRUE(in_range<UShort>(short{Short_Limits::max()}));
  EXPECT_TRUE(in_range<UInt>(int{Int_Limits::max()}));
  EXPECT_TRUE(in_range<ULong>(long{Long_Limits::max()}));
  EXPECT_TRUE(in_range<ULLong>(LLong{LLong_Limits::max()}));
}

TEST(Test_Narrow_Cast,
     signed_to_unsigned_narrow_cast_fails_if_input_is_negative) {
#if QLJS_HAVE_CHAR8_T
  EXPECT_FALSE(in_range<char8_t>(SChar{SChar_Limits::lowest()}));
  EXPECT_FALSE(in_range<char8_t>(short{Short_Limits::lowest()}));
  EXPECT_FALSE(in_range<char8_t>(int{Int_Limits::lowest()}));
  EXPECT_FALSE(in_range<char8_t>(long{Long_Limits::lowest()}));
  EXPECT_FALSE(in_range<char8_t>(LLong{LLong_Limits::lowest()}));
#endif

  EXPECT_FALSE(in_range<UChar>(SChar{SChar_Limits::lowest()}));
  EXPECT_FALSE(in_range<UChar>(short{Short_Limits::lowest()}));
  EXPECT_FALSE(in_range<UChar>(int{Int_Limits::lowest()}));
  EXPECT_FALSE(in_range<UChar>(long{Long_Limits::lowest()}));
  EXPECT_FALSE(in_range<UChar>(LLong{LLong_Limits::lowest()}));

  EXPECT_FALSE(in_range<UShort>(SChar{SChar_Limits::lowest()}));
  EXPECT_FALSE(in_range<UShort>(short{Short_Limits::lowest()}));
  EXPECT_FALSE(in_range<UShort>(int{Int_Limits::lowest()}));
  EXPECT_FALSE(in_range<UShort>(long{Long_Limits::lowest()}));
  EXPECT_FALSE(in_range<UShort>(LLong{LLong_Limits::lowest()}));

  EXPECT_FALSE(in_range<UInt>(SChar{SChar_Limits::lowest()}));
  EXPECT_FALSE(in_range<UInt>(short{Short_Limits::lowest()}));
  EXPECT_FALSE(in_range<UInt>(int{Int_Limits::lowest()}));
  EXPECT_FALSE(in_range<UInt>(long{Long_Limits::lowest()}));
  EXPECT_FALSE(in_range<UInt>(LLong{LLong_Limits::lowest()}));

  EXPECT_FALSE(in_range<ULong>(SChar{SChar_Limits::lowest()}));
  EXPECT_FALSE(in_range<ULong>(short{Short_Limits::lowest()}));
  EXPECT_FALSE(in_range<ULong>(int{Int_Limits::lowest()}));
  EXPECT_FALSE(in_range<ULong>(long{Long_Limits::lowest()}));
  EXPECT_FALSE(in_range<ULong>(LLong{LLong_Limits::lowest()}));

  EXPECT_FALSE(in_range<ULLong>(SChar{SChar_Limits::lowest()}));
  EXPECT_FALSE(in_range<ULLong>(short{Short_Limits::lowest()}));
  EXPECT_FALSE(in_range<ULLong>(int{Int_Limits::lowest()}));
  EXPECT_FALSE(in_range<ULLong>(long{Long_Limits::lowest()}));
  EXPECT_FALSE(in_range<ULLong>(LLong{LLong_Limits::lowest()}));
}

TEST(Test_Narrow_Cast, signed_to_unsigned_narrow_cast_fails_if_out_of_range) {
#if QLJS_HAVE_CHAR8_T
  static_assert(sizeof(short) > sizeof(char8_t));
  EXPECT_FALSE(in_range<char8_t>(short{Short_Limits::max()}));
  EXPECT_FALSE(in_range<char8_t>(int{Int_Limits::max()}));
  EXPECT_FALSE(in_range<char8_t>(long{Long_Limits::max()}));
  EXPECT_FALSE(in_range<char8_t>(LLong{LLong_Limits::max()}));
#endif

  static_assert(sizeof(short) > sizeof(UChar));
  EXPECT_FALSE(in_range<UChar>(short{Short_Limits::max()}));
  EXPECT_FALSE(in_range<UChar>(int{Int_Limits::max()}));
  EXPECT_FALSE(in_range<UChar>(long{Long_Limits::max()}));
  EXPECT_FALSE(in_range<UChar>(LLong{LLong_Limits::max()}));

  static_assert(sizeof(int) > sizeof(UShort));
  EXPECT_FALSE(in_range<UShort>(int{Int_Limits::max()}));
  EXPECT_FALSE(in_range<UShort>(long{Long_Limits::max()}));
  EXPECT_FALSE(in_range<UShort>(LLong{LLong_Limits::max()}));

  if (sizeof(long) > sizeof(UInt)) {
    EXPECT_FALSE(in_range<UInt>(long{Long_Limits::max()}));
  }
  if (sizeof(LLong) > sizeof(UInt)) {
    EXPECT_FALSE(in_range<UInt>(LLong{LLong_Limits::max()}));
  }

  if (sizeof(LLong) > sizeof(ULong)) {
    EXPECT_FALSE(in_range<ULong>(LLong{LLong_Limits::max()}));
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
