// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PROGRAM_ERROR_H
#define QUICK_LINT_JS_PROGRAM_ERROR_H

#if QLJS_SUBLIME_TEXT_PLUGIN
#include <quick-lint-js/string-utilities.h>

// Appends report to qljs_sublime_text_program_error_reports
#define QLJS_REPORT_PROGRAM_ERROR(format, ...)               \
  ::quick_lint_js::asprintf(                                 \
      &qljs_sublime_text_program_error_reports, "%s" format, \
      qljs_sublime_text_program_error_reports, ##__VA_ARGS__)

#define QLJS_CLEAR_PROGRAM_ERROR() \
  qljs_sublime_text_program_error_reports = (char*)""

extern char* qljs_sublime_text_program_error_reports;
#else
#define QLJS_REPORT_PROGRAM_ERROR(...) ::std::fprintf(stderr, __VA_ARGS__)
#define QLJS_CLEAN_PROGRAM_ERROR() /* empty */
#endif

#endif  // QUICK_LINT_JS_PROGRAM_ERROR_H

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
