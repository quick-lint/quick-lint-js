// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_H
#define QUICK_LINT_JS_SUBLIME_TEXT_H

#include <csetjmp>
#include <csignal>

#if QLJS_HAVE_SIGSETJMP
#define QLJS_SUBLIME_TEXT_TRY() \
  if (sigsetjmp(qljs_sublime_text_sigjmp_buf, 1) == 0)
#define QLJS_SUBLIME_TEXT_CATCH() else
#define QLJS_SUBLIME_TEXT_THROW() siglongjmp(qljs_sublime_text_sigjmp_buf, 1)
#else
#define QLJS_SUBLIME_TEXT_TRY() if (setjmp(qljs_sublime_text_jmp_buf) == 0)
#define QLJS_SUBLIME_TEXT_CATCH() else
#define QLJS_SUBLIME_TEXT_THROW() ::std::longjmp(qljs_sublime_text_jmp_buf, 1)
#endif

#define QLJS_SUBLIME_TEXT_DEFINE_SIGNAL_HANDLER()       \
  do {                                                  \
    signal(SIGABRT, &qljs_sublime_text_signal_handler); \
    signal(SIGFPE, &qljs_sublime_text_signal_handler);  \
    signal(SIGILL, &qljs_sublime_text_signal_handler);  \
    signal(SIGINT, &qljs_sublime_text_signal_handler);  \
    signal(SIGSEGV, &qljs_sublime_text_signal_handler); \
    signal(SIGTERM, &qljs_sublime_text_signal_handler); \
  } while (false)

#if QLJS_HAVE_SIGSETJMP
extern sigjmp_buf qljs_sublime_text_sigjmp_buf;
#else
extern jmp_buf qljs_sublime_text_jmp_buf;
#endif

void qljs_sublime_text_signal_handler(int signal_number);

// For testing purposes only.
#if QLJS_SUBLIME_TEXT_PLUGIN_TEST
#include <cassert>
#include <cstdlib>
#include <ctime>

#define QLJS_SUBLIME_INITILIZE_TEST_CRASH() \
  srand(static_cast<unsigned int>(time(NULL)))

#define QLJS_SUBLIME_EXECUTE_TEST_CRASH()                                  \
  do {                                                                     \
    switch (static_cast<int>(rand() % (5 + 1))) {                          \
    case 0:                                                                \
      ::std::fputs("case 0: ::std::abort();", stderr);                     \
      ::std::abort();                                                      \
    case 1:                                                                \
      ::std::fputs("case 1: assert(false);", stderr);                      \
      assert(false);                                                       \
    case 2:                                                                \
      ::std::fputs("case 2: *((unsigned int*)0) = 0xDEAD;", stderr);       \
      *((unsigned int*)0) = 0xDEAD;                                        \
    case 3:                                                                \
      ::std::fputs("case 3: *((char*)NULL) = 0;", stderr);                 \
      *((char*)NULL) = 0;                                                  \
    case 4:                                                                \
      ::std::fputs("case 4: raise(SIGSEGV);", stderr);                     \
      raise(SIGSEGV);                                                      \
    case 5:                                                                \
      ::std::fputs("case 5: for (long long int i = 0; ++i; (&i)[i] = i);", \
                   stderr);                                                \
      for (long long int i = 0; ++i; (&i)[i] = i)                          \
        ;                                                                  \
    }                                                                      \
  } while (false)
#endif

#endif  // QUICK_LINT_JS_SUBLIME_TEXT_H

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
