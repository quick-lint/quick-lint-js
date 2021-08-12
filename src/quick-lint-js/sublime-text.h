// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_H
#define QUICK_LINT_JS_SUBLIME_TEXT_H

// In Linux, if there is for example a program's segmentation fault,
// the OS will send a signal to the program's process.
//
// The program has predefined signal handlers for each type of signal.
// These signal handlers are called when your specified signal is sent.
// The program can set your own signal handlers.
//
// If one of these handlers is called, and inside it, there is a longjmp
// (for the program to continue the execution), the next time this
// signal handler be called, the program's process will be killed
// (or the program will have undefined behavior), but if instead of
// longjmp a siglongjmp be used, the next time this signal handler
// be called, the process will not be killed.
//
// It's because the signal mask of the process not be restored when the
// longjmp is called but be restored when siglongjmp is called, so for
// the code to work as expected there will be a preference in
// sigsetjmp/siglongjmp over setjmp/longjmp.
//
// For more details:
// https://stackoverflow.com/q/7334595  | https://stackoverflow.com/q/1715413
// https://stackoverflow.com/q/14233464 | https://stackoverflow.com/q/20755260
// http://poincare.matf.bg.ac.rs/~ivana/courses/ps/sistemi_knjige/pomocno/apue/APUE/0201433079/ch10lev1sec15.html

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

#define QLJS_RANDRANGE(start, stop) ((rand() % (stop - start + 1)) + start)

#define QLJS_SUBLIME_INITILIZE_TEST_CRASH() \
  srand(static_cast<unsigned int>(time(NULL)))

#define QLJS_SUBLIME_EXECUTE_TEST_CRASH()                            \
  do {                                                               \
    switch (static_cast<int>(QLJS_RANDRANGE(0, 3))) {                \
    case 0:                                                          \
      ::std::fputs("case 0: ::std::abort();", stderr);               \
      ::std::abort();                                                \
    case 1:                                                          \
      ::std::fputs("case 1: assert(false);", stderr);                \
      assert(false);                                                 \
    case 2:                                                          \
      ::std::fputs("case 2: *((unsigned int*)0) = 0xDEAD;", stderr); \
      *((unsigned int*)0) = 0xDEAD;                                  \
    case 3:                                                          \
      ::std::fputs("case 3: raise(SIGSEGV);", stderr);               \
      raise(SIGSEGV);                                                \
    }                                                                \
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
