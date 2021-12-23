// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_H
#define QUICK_LINT_JS_SUBLIME_TEXT_H

#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct qljs_st_parser qljs_st_parser;

enum qljs_st_severity {
  qljs_st_severity_error = 1,
  qljs_st_severity_warning = 2,
};

struct qljs_st_text {
  const char* content;
  size_t length;
};

struct qljs_st_position {
  unsigned int line;
  unsigned int character;
};

#if QLJS_ST_PLUGIN_VERSION == 3
struct qljs_st_region {
  unsigned int start;
  unsigned int end;
};
#else
struct qljs_st_range {
  qljs_st_position start;
  qljs_st_position end;
};
#endif

struct qljs_st_diagnostic {
  const char* message;
  const char* code;
  qljs_st_severity_error severity;
#if QLJS_ST_PLUGIN_VERSION == 3
  const qljs_st_region* region;
#else
  const qljs_st_range* range;
#endif
};

qljs_st_parser* qljs_st_parser_new(void);

void qljs_st_parser_delete(qljs_st_parser* parser);

void qljs_st_parser_set_text(qljs_st_parser* parser, const qljs_st_text* text);

void qljs_st_parser_replace_text(qljs_st_parser* parser,
                                 const qljs_st_range* range,
                                 const qljs_st_text* text);

const qljs_st_diagnostic* qljs_st_parser_lint(qljs_st_parser* parser);

#if defined(__cplusplus)
}
#endif

#endif  // QUICK_LINT_JS_SUBLIME_TEXT_H

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
