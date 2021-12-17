// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PROGRAM_REPORT_H
#define QUICK_LINT_JS_PROGRAM_REPORT_H

#if QLJS_SUBLIME_TEXT_PLUGIN

#include <cstring>
#include <quick-lint-js/force-inline.h>
#include <quick-lint-js/string-utilities.h>

extern char *qljs_sublime_text_program_error_reports;

// In the C language, separating two strings with space as in "s" "1" is
// exactly equivalent to having a single string "s1".

// %s is used for prepend.
#define QLJS_REPORT_PROGRAM_MESSAGE(fmt, ...)                                  \
  ::quick_lint_js::asprintf(&qljs_sublime_text_program_error_reports,          \
                            "%s" fmt, qljs_sublime_text_program_error_reports, \
                            ##__VA_ARGS__)

#define QLJS_REPORT_PROGRAM_FATAL_ERROR(...) \
  QLJS_REPORT_PROGRAM_MESSAGE(__VA_ARGS__)
#define QLJS_REPORT_PROGRAM_ERROR(...) QLJS_REPORT_PROGRAM_MESSAGE(__VA_ARGS__)

// Sublime Text plugin user does not need to receive debug messages and
// warning messages.
#define QLJS_REPORT_PROGRAM_WARNING(...) /* empty */
#define QLJS_REPORT_PROGRAM_DEBUG(...)   /* empty */

#define QLJS_SUBLIME_TEXT_CLEAR_PROGRAM_REPORT()                             \
  do {                                                                       \
    if (qljs_sublime_text_program_error_reports != nullptr &&                \
        qljs_sublime_text_program_error_reports != const_cast<char *>("")) { \
      ::std::free(qljs_sublime_text_program_error_reports);                  \
      qljs_sublime_text_program_error_reports = const_cast<char *>("");      \
    }                                                                        \
  } while (false)

#define QLJS_SUBLIME_TEXT_GET_PROGRAM_REPORT() \
  qljs_sublime_text_program_error_reports

#define QLJS_SUBLIME_TEXT_COPY_PROGRAM_REPORT() \
  strdup(qljs_sublime_text_program_error_reports)

#else

#include <cstdio>

#define QLJS_REPORT_PROGRAM_MESSAGE(...) ::std::fprintf(stderr, __VA_ARGS__)

#define QLJS_REPORT_PROGRAM_FATAL_ERROR(...) \
  QLJS_REPORT_PROGRAM_MESSAGE(__VA_ARGS__)
#define QLJS_REPORT_PROGRAM_ERROR(...) QLJS_REPORT_PROGRAM_MESSAGE(__VA_ARGS__)

#define QLJS_REPORT_PROGRAM_WARNING(...) \
  QLJS_REPORT_PROGRAM_MESSAGE(__VA_ARGS__)
#define QLJS_REPORT_PROGRAM_DEBUG(...) QLJS_REPORT_PROGRAM_MESSAGE(__VA_ARGS__)

#endif

#endif  // QUICK_LINT_JS_PROGRAM_REPORT_H

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
