// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/crash-handling.h>

#if QLJS_SUBLIME_TEXT_PLUGIN

#if QLJS_HAVE_SIGSETJMP
sigjmp_buf qljs_sublime_text_sigjmp_buf;
#else
jmp_buf qljs_sublime_text_jmp_buf;
#endif

void qljs_sublime_text_signal_handler(int signal_number) {
  QLJS_SUBLIME_TEXT_THROW();
}

#if QLJS_SUBLIME_TEXT_PLUGIN_CRASH_HANDLING_TEST
void qljs_sublime_text_test_crash_handling() {
  static int num = 0;
  if (num > 3) {
    num = 0;
  }
  switch (num++) {
  case 0:
    std::fputs("case 0: std::abort();", stderr);
    std::abort();
  case 1:
    std::fputs("case 1: assert(false);", stderr);
    assert(false);
  case 2:
    std::fputs("case 2: *((unsigned int*)0) = 0xDEAD;", stderr);
    *((unsigned int*)0) = 0xDEAD;
  case 3:
    std::fputs("case 3: raise(SIGSEGV);", stderr);
    raise(SIGSEGV);
  }
}
#endif

#endif  // QLJS_SUBLIME_TEXT_PLUGIN

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
