// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ASSERT_H
#define QUICK_LINT_JS_ASSERT_H

#include <cstdlib>
#include <quick-lint-js/crash.h>
#include <quick-lint-js/have.h>

#if QLJS_HAVE_DEBUGBREAK
#include <intrin.h>
#endif

#define QLJS_UNIMPLEMENTED() QLJS_ALWAYS_ASSERT(false)

#define QLJS_ALWAYS_ASSERT(...)                                               \
  do {                                                                        \
    if (__VA_ARGS__) {                                                        \
    } else {                                                                  \
      ::quick_lint_js::report_assertion_failure(__FILE__, __LINE__, __func__, \
                                                #__VA_ARGS__);                \
      QLJS_ASSERT_TRAP();                                                     \
    }                                                                         \
  } while (false)

#define QLJS_NEVER_ASSERT(...) \
  do {                         \
    if (false) {               \
      if (__VA_ARGS__) {       \
      }                        \
    }                          \
  } while (false)

#if defined(NDEBUG) && NDEBUG
#define QLJS_ASSERT(...) QLJS_NEVER_ASSERT(__VA_ARGS__)
#else
#define QLJS_ASSERT(...) QLJS_ALWAYS_ASSERT(__VA_ARGS__)
#endif

#define QLJS_ASSERT_TRAP() QLJS_CRASH_ALLOWING_CORE_DUMP()

namespace quick_lint_js {
void report_assertion_failure(const char *qljs_file_name, int qljs_line,
                              const char *qljs_function_name,
                              const char *message);
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
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
