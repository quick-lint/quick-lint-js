// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gtest/gtest.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/crash.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/warning.h>

#if QLJS_HAVE_UNAME
#include <sys/utsname.h>
#endif

#if QLJS_HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <Windows.h>
#endif

#if defined(__SANITIZE_ADDRESS__)
#define SANITIZERS_ENABLED 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer) || __has_feature(thread_sanitizer)
#define SANITIZERS_ENABLED 1
#else
#define SANITIZERS_ENABLED 0
#endif
#else
#define SANITIZERS_ENABLED 0
#endif

QLJS_WARNING_IGNORE_CLANG("-Wcovered-switch-default")

namespace quick_lint_js {
namespace {
bool is_windows_subsystem_for_linux() {
#if QLJS_HAVE_UNAME && defined(__linux__)
  // See: https://github.com/Microsoft/WSL/issues/423#issuecomment-221627364
  // Example .release for WSL1: "4.4.0-18362-Microsoft"
  // Example .release for WSL2: "4.19.128-microsoft-standard"
  ::utsname system_name;
  int rc = ::uname(&system_name);
  QLJS_ALWAYS_ASSERT(rc == 0);
  // TODO(strager): See if this unintentionally matches other distributions
  // (such as Linux on Azure).
  return std::strstr(system_name.release, "icrosoft") ||
         std::strstr(system_name.release, "WSL");
#else
  return false;
#endif
}

#if QLJS_HAVE_UNISTD_H
bool file_exists(const char* path) { return ::access(path, F_OK) == 0; }
#elif QLJS_HAVE_WINDOWS_H
bool file_exists(const char* path) {
  return ::GetFileAttributesA(path) != INVALID_FILE_ATTRIBUTES;
}
#else
#error "Unknown platform"
#endif

bool is_windows_subsystem_for_linux_v1() {
  if (!is_windows_subsystem_for_linux()) {
    return false;
  }
  bool is_wsl_2 = file_exists("/run/WSL");
  return !is_wsl_2;
}

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

#if defined(GTEST_HAS_DEATH_TEST) && GTEST_HAS_DEATH_TEST
#if SANITIZERS_ENABLED
// On Linux, some versions of AddressSanitizer and ThreadSanitizer call
// setrlimit(RLIMIT_CORE) with rlim_max=0. When this happens, the
// disable_core_dumping function can't raise rlim_cur to 1. (See
// disable_core_dumping's implementation for why this is sometimes necessary).
// This causes our disable_core_dumping function to not work. Disable this test
// if running with ASAN or TSAN.
TEST(test_crash, DISABLED_crash_disallowing_core_dump) {
#else
TEST(test_crash, crash_disallowing_core_dump) {
#endif
  auto check = [] { QLJS_CRASH_DISALLOWING_CORE_DUMP(); };
  auto crashed_without_core_dump = [](int status) -> bool {
#if QLJS_HAVE_SYS_WAIT_H
    bool dumped_core = WCOREDUMP(status);
    if (is_windows_subsystem_for_linux_v1()) {
      // HACK(strager): WSL1 always claims it creates core dumps, but never
      // actually makes any core dumps. Assume no core dump was generated.
      dumped_core = false;
    }
    return WIFSIGNALED(status) && !dumped_core;
#else
    return status != 0;
#endif
  };
  EXPECT_EXIT(check(), crashed_without_core_dump, "");
}
#endif
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
