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

#ifndef QUICK_LINT_JS_VSCODE_H
#define QUICK_LINT_JS_VSCODE_H

#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct qljs_vscode_parser qljs_vscode_parser;

typedef enum qljs_vscode_severity {
  qljs_vscode_severity_error = 1,
  qljs_vscode_severity_warning = 2,
} qljs_vscode_severity;

qljs_vscode_parser* qljs_vscode_create_parser(void);
void qljs_vscode_destroy_parser(qljs_vscode_parser*);

void qljs_vscode_replace_text(qljs_vscode_parser*, int start_line,
                              int start_character, int end_line,
                              int end_character,
                              const void* replacement_text_utf_8,
                              size_t replacement_text_byte_count);

struct qljs_vscode_diagnostic {
  const char* message;
  const char* code;
  qljs_vscode_severity severity;
  int start_line;
  int start_character;
  int end_line;
  int end_character;
};

const qljs_vscode_diagnostic* qljs_vscode_lint(qljs_vscode_parser*);

#if defined(__cplusplus)
}
#endif

#endif
