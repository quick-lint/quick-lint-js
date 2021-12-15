// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gtest/gtest.h>
#include <quick-lint-js/hash-fnv.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(test_hash_fnv, fnv_1a_default_offset_basis) {
  // https://datatracker.ietf.org/doc/html/draft-eastlake-fnv-17.html#page-26
  EXPECT_EQ(hash_fnv_1a_64(""sv), 0xcbf29ce484222325ULL);
  EXPECT_EQ(hash_fnv_1a_64("\0"sv), 0xaf63bd4c8601b7dfULL);
  EXPECT_EQ(hash_fnv_1a_64("a"sv), 0xaf63dc4c8601ec8cULL);
  EXPECT_EQ(hash_fnv_1a_64("a\0"sv), 0x089be207b544f1e4ULL);
  EXPECT_EQ(hash_fnv_1a_64("foobar"sv), 0x85944171f73967e8ULL);
  EXPECT_EQ(hash_fnv_1a_64("foobar\0"sv), 0x34531ca7168b8f38ULL);
  // https://docs.aws.amazon.com/redshift/latest/dg/r_FNV_HASH.html
  EXPECT_EQ(hash_fnv_1a_64("Amazon Redshift"sv), 0x6c048340799c899eULL);
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
