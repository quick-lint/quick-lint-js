// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_C_API_H
#define QUICK_LINT_JS_C_API_H

#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif

typedef enum qljs_severity {
  qljs_severity_error = 1,
  qljs_severity_warning = 2,
} qljs_severity;

typedef struct qljs_vscode_parser qljs_vscode_parser;
struct qljs_vscode_diagnostic {
  const char* message;
  const char* code;
  qljs_severity severity;
  int start_line;
  int start_character;
  int end_line;
  int end_character;
};
qljs_vscode_parser* qljs_vscode_create_parser(void);
void qljs_vscode_destroy_parser(qljs_vscode_parser*);
void qljs_vscode_replace_text(qljs_vscode_parser*, int start_line,
                              int start_character, int end_line,
                              int end_character,
                              const void* replacement_text_utf_8,
                              size_t replacement_text_byte_count);
const qljs_vscode_diagnostic* qljs_vscode_lint(qljs_vscode_parser*);

typedef struct qljs_web_demo_parser qljs_web_demo_parser;
struct qljs_web_demo_diagnostic {
  const char* message;
  const char* code;
  qljs_severity severity;
  // Offsets count UTF-16 code units.
  int begin_offset;
  int end_offset;
};
qljs_web_demo_parser* qljs_web_demo_create_parser(void);
void qljs_web_demo_destroy_parser(qljs_web_demo_parser*);
void qljs_web_demo_set_text(qljs_web_demo_parser*, const void* text_utf_8,
                            size_t text_byte_count);
const qljs_web_demo_diagnostic* qljs_web_demo_lint(qljs_web_demo_parser*);

#if QLJS_SUBLIME_TEXT_PLUGIN
typedef struct qljs_sublime_text_3_parser qljs_sublime_text_3_parser;
struct qljs_sublime_text_3_diagnostic {
  const char* message;
  const char* code;
  qljs_severity severity;
  int begin_offset;
  int end_offset;
};
// There are three states for the structure below:
// 1. Without error: qljs_sublime_text_3_error{nullptr}
// 2. With error without message: qljs_sublime_text_3_error{""}
// 3. With error with message: qljs_sublime_text_3_error{"internal failure"}
struct qljs_sublime_text_3_error {
  const char* assertion_failure_report;
};
struct qljs_sublime_text_3_result {
  union {
    const qljs_sublime_text_3_diagnostic* diagnostics;
    const qljs_sublime_text_3_error error;
  } value;
  bool is_diagnostics;
};
qljs_sublime_text_3_parser* qljs_sublime_text_3_create_parser(void);
void qljs_sublime_text_3_destroy_parser(qljs_sublime_text_3_parser*);
qljs_sublime_text_3_error qljs_sublime_text_3_set_text(
    qljs_sublime_text_3_parser*, const void* text_utf_8,
    size_t text_byte_count);
const qljs_sublime_text_3_result* qljs_sublime_text_3_lint(
    qljs_sublime_text_3_parser*);

typedef struct qljs_sublime_text_4_parser qljs_sublime_text_4_parser;
struct qljs_sublime_text_4_diagnostic {
  const char* message;
  const char* code;
  qljs_severity severity;
  int start_line;
  int start_character;
  int end_line;
  int end_character;
};
// There are three states for the structure below:
// 1. Without error: qljs_sublime_text_4_error{nullptr}
// 2. With error without message: qljs_sublime_text_4_error{""}
// 3. With error with message: qljs_sublime_text_4_error{"internal failure"}
struct qljs_sublime_text_4_error {
  const char* assertion_failure_report;
};
struct qljs_sublime_text_4_result {
  union {
    const qljs_sublime_text_4_diagnostic* diagnostics;
    const qljs_sublime_text_4_error error;
  } value;
  bool is_diagnostics;
};
qljs_sublime_text_4_parser* qljs_sublime_text_4_create_parser(void);
void qljs_sublime_text_4_destroy_parser(qljs_sublime_text_4_parser*);
qljs_sublime_text_4_error qljs_sublime_text_4_replace_text(
    qljs_sublime_text_4_parser* p, int start_line, int start_character,
    int end_line, int end_character, const void* replacement_text_utf_8,
    size_t replacement_text_byte_count);
const qljs_sublime_text_4_result* qljs_sublime_text_4_lint(
    qljs_sublime_text_4_parser* p);
#endif

#if defined(__cplusplus)
}
#endif

#endif

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
