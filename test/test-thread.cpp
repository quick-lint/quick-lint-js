// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <iostream>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>

namespace quick_lint_js {
namespace {
TEST(Test_Thread, print_thread_id) {
  // This is a manual test.
  std::cerr << "main thread ID: " << get_current_thread_id() << '\n';
}

TEST(Test_Thread, thread_id_is_stable) {
  std::uint64_t id = get_current_thread_id();
  std::uint64_t id2 = get_current_thread_id();
  EXPECT_EQ(id, id2) << "thread ID should not change between calls";
}

#if QLJS_HAVE_THREADS
TEST(Test_Thread, thread_ids_differ_between_threads) {
  std::uint64_t main_id = get_current_thread_id();
  std::uint64_t other_id;

  Thread other_thread([&] { other_id = get_current_thread_id(); });
  other_thread.join();

  EXPECT_NE(main_id, other_id) << "thread IDs should differ";
}
#endif
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
