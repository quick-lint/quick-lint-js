// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/crash.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/warning.h>

#if QLJS_HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

QLJS_WARNING_IGNORE_CLANG("-Wcovered-switch-default")

namespace quick_lint_js {
namespace {
#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST
TEST(test_crash, crash_allowing_core_dump) {
  auto check = [] { QLJS_CRASH_ALLOWING_CORE_DUMP(); };
  auto crashed = [](int status) -> bool {
#if QLJS_HAVE_SYS_WAIT_H
    return WIFSIGNALED(status);
#else
    return status != 0;
#endif
  };
  EXPECT_EXIT(check(), crashed, "");
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
