// quicklint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <doctest/doctest.h>
#include <limits>
#include <quicklint-js/narrow-cast.h>

namespace quicklint_js {
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

TEST_CASE("same-type signed narrow_cast never fails") {
  CHECK(narrow_cast<int>(int{0}) == 0);
  CHECK(narrow_cast<int>(int{1234}) == 1234);
  CHECK(narrow_cast<int>(int_limits::lowest()) == int_limits::lowest());
  CHECK(narrow_cast<int>(int_limits::max()) == int_limits::max());

  CHECK(narrow_cast<long>(long{0}) == 0);
  CHECK(narrow_cast<long>(long{1234}) == 1234);
  CHECK(narrow_cast<long>(long_limits::lowest()) == long_limits::lowest());
  CHECK(narrow_cast<long>(long_limits::max()) == long_limits::max());
}

TEST_CASE("same-type unsigned narrow_cast never fails") {
  CHECK(narrow_cast<uint>(uint{0}) == 0);
  CHECK(narrow_cast<uint>(uint{1234}) == 1234);
  CHECK(narrow_cast<uint>(uint_limits::lowest()) == uint_limits::lowest());
  CHECK(narrow_cast<uint>(uint_limits::max()) == uint_limits::max());

  CHECK(narrow_cast<ulong>(ulong{0}) == 0);
  CHECK(narrow_cast<ulong>(ulong{1234}) == 1234);
  CHECK(narrow_cast<ulong>(ulong_limits::lowest()) == ulong_limits::lowest());
  CHECK(narrow_cast<ulong>(ulong_limits::max()) == ulong_limits::max());
}

TEST_CASE("signed->signed narrow_cast succeeds if in range") {
  CHECK(can_narrow_cast<schar>(short{schar_limits::lowest()}));
  CHECK(can_narrow_cast<schar>(short{schar_limits::max()}));
  CHECK(can_narrow_cast<schar>(int{schar_limits::lowest()}));
  CHECK(can_narrow_cast<schar>(int{schar_limits::max()}));
  CHECK(can_narrow_cast<schar>(long{schar_limits::lowest()}));
  CHECK(can_narrow_cast<schar>(long{schar_limits::max()}));
  CHECK(can_narrow_cast<schar>(llong{schar_limits::lowest()}));
  CHECK(can_narrow_cast<schar>(llong{schar_limits::max()}));

  CHECK(can_narrow_cast<short>(int{short_limits::lowest()}));
  CHECK(can_narrow_cast<short>(int{short_limits::max()}));
  CHECK(can_narrow_cast<short>(long{short_limits::lowest()}));
  CHECK(can_narrow_cast<short>(long{short_limits::max()}));
  CHECK(can_narrow_cast<short>(llong{short_limits::lowest()}));
  CHECK(can_narrow_cast<short>(llong{short_limits::max()}));

  CHECK(can_narrow_cast<int>(long{int_limits::lowest()}));
  CHECK(can_narrow_cast<int>(long{int_limits::max()}));
  CHECK(can_narrow_cast<int>(llong{int_limits::lowest()}));
  CHECK(can_narrow_cast<int>(llong{int_limits::max()}));

  CHECK(can_narrow_cast<long>(llong{long_limits::lowest()}));
  CHECK(can_narrow_cast<long>(llong{long_limits::max()}));
}

TEST_CASE("signed->signed narrow_cast fails if out of range") {
  static_assert(sizeof(schar) < sizeof(llong));
  CHECK(!can_narrow_cast<schar>(llong_limits::lowest()));
  CHECK(!can_narrow_cast<schar>(llong_limits::max()));
  static_assert(sizeof(schar) < sizeof(long));
  CHECK(!can_narrow_cast<schar>(long_limits::lowest()));
  CHECK(!can_narrow_cast<schar>(long_limits::max()));

  static_assert(sizeof(short) < sizeof(llong));
  CHECK(!can_narrow_cast<short>(llong_limits::lowest()));
  CHECK(!can_narrow_cast<short>(llong_limits::max()));
  static_assert(sizeof(short) < sizeof(long));
  CHECK(!can_narrow_cast<short>(long_limits::lowest()));
  CHECK(!can_narrow_cast<short>(long_limits::max()));

  static_assert(sizeof(int) < sizeof(llong));
  CHECK(!can_narrow_cast<int>(llong_limits::lowest()));
  CHECK(!can_narrow_cast<int>(llong_limits::max()));
}

TEST_CASE("unsigned->unsigned narrow_cast succeeds if in range") {
  CHECK(can_narrow_cast<uchar>(ushort{uchar_limits::lowest()}));
  CHECK(can_narrow_cast<uchar>(ushort{uchar_limits::max()}));
  CHECK(can_narrow_cast<uchar>(uint{uchar_limits::lowest()}));
  CHECK(can_narrow_cast<uchar>(uint{uchar_limits::max()}));
  CHECK(can_narrow_cast<uchar>(ulong{uchar_limits::lowest()}));
  CHECK(can_narrow_cast<uchar>(ulong{uchar_limits::max()}));
  CHECK(can_narrow_cast<uchar>(ullong{uchar_limits::lowest()}));
  CHECK(can_narrow_cast<uchar>(ullong{uchar_limits::max()}));

  CHECK(can_narrow_cast<ushort>(uint{ushort_limits::lowest()}));
  CHECK(can_narrow_cast<ushort>(uint{ushort_limits::max()}));
  CHECK(can_narrow_cast<ushort>(ulong{ushort_limits::lowest()}));
  CHECK(can_narrow_cast<ushort>(ulong{ushort_limits::max()}));
  CHECK(can_narrow_cast<ushort>(ullong{ushort_limits::lowest()}));
  CHECK(can_narrow_cast<ushort>(ullong{ushort_limits::max()}));

  CHECK(can_narrow_cast<uint>(ulong{uint_limits::lowest()}));
  CHECK(can_narrow_cast<uint>(ulong{uint_limits::max()}));
  CHECK(can_narrow_cast<uint>(ullong{uint_limits::lowest()}));
  CHECK(can_narrow_cast<uint>(ullong{uint_limits::max()}));

  CHECK(can_narrow_cast<ulong>(ullong{ulong_limits::lowest()}));
  CHECK(can_narrow_cast<ulong>(ullong{ulong_limits::max()}));
}

TEST_CASE("unsigned->unsigned narrow_cast fails if out of range") {
  static_assert(sizeof(schar) < sizeof(llong));
  CHECK(!can_narrow_cast<schar>(llong_limits::max()));
  static_assert(sizeof(schar) < sizeof(long));
  CHECK(!can_narrow_cast<schar>(long_limits::max()));

  static_assert(sizeof(short) < sizeof(llong));
  CHECK(!can_narrow_cast<short>(llong_limits::max()));
  static_assert(sizeof(short) < sizeof(long));
  CHECK(!can_narrow_cast<short>(long_limits::lowest()));
  CHECK(!can_narrow_cast<short>(long_limits::max()));

  static_assert(sizeof(int) < sizeof(llong));
  CHECK(!can_narrow_cast<int>(llong_limits::max()));
}

TEST_CASE("unsigned->signed narrow_cast succeeds if in range") {
  CHECK(can_narrow_cast<schar>(uchar{schar_limits::max()}));
  CHECK(can_narrow_cast<schar>(ushort{schar_limits::max()}));
  CHECK(can_narrow_cast<schar>(uint{schar_limits::max()}));
  CHECK(can_narrow_cast<schar>(ulong{schar_limits::max()}));
  CHECK(can_narrow_cast<schar>(ullong{schar_limits::max()}));

  CHECK(can_narrow_cast<short>(ushort{short_limits::max()}));
  CHECK(can_narrow_cast<short>(uint{short_limits::max()}));
  CHECK(can_narrow_cast<short>(ulong{short_limits::max()}));
  CHECK(can_narrow_cast<short>(ullong{short_limits::max()}));

  CHECK(can_narrow_cast<int>(uint{int_limits::max()}));
  CHECK(can_narrow_cast<int>(ulong{int_limits::max()}));
  CHECK(can_narrow_cast<int>(ullong{int_limits::max()}));

  CHECK(can_narrow_cast<long>(ulong{long_limits::max()}));
  CHECK(can_narrow_cast<long>(ullong{long_limits::max()}));

  CHECK(can_narrow_cast<llong>(ullong{llong_limits::max()}));
}

TEST_CASE("unsigned->signed narrow_cast fails if in range") {
  CHECK(!can_narrow_cast<schar>(uchar{uchar_limits::max()}));
  CHECK(!can_narrow_cast<schar>(ushort{uchar_limits::max()}));
  CHECK(!can_narrow_cast<schar>(uint{uchar_limits::max()}));
  CHECK(!can_narrow_cast<schar>(ulong{uchar_limits::max()}));
  CHECK(!can_narrow_cast<schar>(ullong{uchar_limits::max()}));

  CHECK(!can_narrow_cast<short>(ushort{ushort_limits::max()}));
  CHECK(!can_narrow_cast<short>(uint{ushort_limits::max()}));
  CHECK(!can_narrow_cast<short>(ulong{ushort_limits::max()}));
  CHECK(!can_narrow_cast<short>(ullong{ushort_limits::max()}));

  CHECK(!can_narrow_cast<int>(uint{uint_limits::max()}));
  CHECK(!can_narrow_cast<int>(ulong{uint_limits::max()}));
  CHECK(!can_narrow_cast<int>(ullong{uint_limits::max()}));

  CHECK(!can_narrow_cast<long>(ulong{ulong_limits::max()}));
  CHECK(!can_narrow_cast<long>(ullong{ulong_limits::max()}));

  CHECK(!can_narrow_cast<llong>(ullong{ullong_limits::max()}));
}
}  // namespace
}  // namespace quicklint_js
