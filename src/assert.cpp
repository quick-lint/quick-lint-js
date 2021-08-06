// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <quick-lint-js/assert.h>

#if QLJS_SUBLIME_TEXT_PLUGIN
#include <cstdlib>
#include <quick-lint-js/sublime-text.h>
#endif

namespace quick_lint_js {
void report_assertion_failure(const char *qljs_file_name, int qljs_line,
                              const char *qljs_function_name,
                              const char *message) {
  constexpr static const char *format =
      "%s:%d: internal check failed in %s: %s\n";
#if QLJS_SUBLIME_TEXT_PLUGIN
  // Memory management in C and auto allocating sprintf() - asprintf():
  // https://insanecoding.blogspot.com/2014/06/memory-management-in-c-and-auto.html
  qljs_sublime_text_assertion_failure_report = (char *)std::malloc(
      std::snprintf(nullptr, 0, format, qljs_file_name, qljs_line,
                    qljs_function_name, message) +
      1);
  std::sprintf(qljs_sublime_text_assertion_failure_report, format,
               qljs_file_name, qljs_line, qljs_function_name, message);
#else
  std::fprintf(stderr, format, qljs_file_name, qljs_line, qljs_function_name,
               message);
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
