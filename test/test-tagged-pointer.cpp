// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/tagged-pointer.h>
#include <quick-lint-js/warning.h>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-override")

namespace quick_lint_js {
namespace {
using test_tagged_pointer_types = ::testing::Types<
#if defined(__x86_64__) || defined(_M_X64)
    tagged_pointer_x86_64,
#endif
    tagged_pointer_portable>;
template <class T>
class test_tagged_pointer : public ::testing::Test {};
TYPED_TEST_SUITE(test_tagged_pointer, test_tagged_pointer_types,
                 ::testing::internal::DefaultNameGenerator);

TYPED_TEST(test_tagged_pointer, null_pointer) {
  for (bool payload : {false, true}) {
    SCOPED_TRACE(payload);
    TypeParam p(nullptr, payload);
    EXPECT_EQ(p.pointer(), nullptr);
    EXPECT_EQ(p.payload(), payload);
  }
}

TYPED_TEST(test_tagged_pointer, automatic_variable_pointer) {
  int dummy;
  for (bool payload : {false, true}) {
    SCOPED_TRACE(payload);
    TypeParam p(&dummy, payload);
    EXPECT_EQ(p.pointer(), &dummy);
    EXPECT_EQ(p.payload(), payload);
  }
}

int dummy_global;

TYPED_TEST(test_tagged_pointer, global_variable_pointer) {
  for (bool payload : {false, true}) {
    SCOPED_TRACE(payload);
    TypeParam p(&dummy_global, payload);
    EXPECT_EQ(p.pointer(), &dummy_global);
    EXPECT_EQ(p.payload(), payload);
  }
}

TYPED_TEST(test_tagged_pointer, string_literal_pointer) {
  for (bool payload : {false, true}) {
    SCOPED_TRACE(payload);
    const char* string = "hello, world!";
    TypeParam p(string, payload);
    EXPECT_EQ(p.pointer(), string);
    EXPECT_EQ(p.payload(), payload);
  }
}

TYPED_TEST(test_tagged_pointer, supports_any_alignment) {
  for (bool payload : {false, true}) {
    SCOPED_TRACE(payload);
    char data[16];
    for (std::size_t offset = 0; offset != std::size(data); ++offset) {
      SCOPED_TRACE(offset);
      TypeParam p(&data[offset], payload);
      EXPECT_EQ(p.pointer(), &data[offset]);
      EXPECT_EQ(p.payload(), payload);
    }
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
