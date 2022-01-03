// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_H
#define QUICK_LINT_JS_SUBLIME_TEXT_H

#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif

typedef struct st_document st_document;

typedef struct st_diagnostic {
  const st_range* range;
  st_severity severity;
  const char* code;
  const char* message;
} st_diagnostic;

typedef struct st_text {
  const char* content;
  size_t length;
} st_text;

typedef enum st_severity {
  st_severity_error = 1,
  st_severity_warning = 2,
} st_severity;

st_document* st_document_new(void);

void st_document_delete(st_document* self);

void st_document_set_text(st_document* self, const st_text* text);

void st_document_replace_text(st_document* self, const st_range* range, const st_text* text);

const st_diagnostic* st_document_lint(st_document* document);

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
