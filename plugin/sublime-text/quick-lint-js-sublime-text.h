// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                 Sublime Text C interface for quick-lint-js                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_H
#define QUICK_LINT_JS_SUBLIME_TEXT_H

#include <quick-lint-js-sublime-text-have.h>
#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif

//==============================================================================
// Offset

typedef unsigned int qljs_sublime_text_offset;

//== Severity ==================================================================

typedef enum qljs_sublime_text_severity {
  qljs_sublime_text_severity_error = 1,
  qljs_sublime_text_severity_warning = 2,
} qljs_sublime_text_severity;

//== Diagnostic ================================================================

typedef struct qljs_sublime_text_diagnostic {
  const qljs_sublime_text_range *range;
  qljs_sublime_text_severity severity;
  const char *code;
  const char *message;
} qljs_sublime_text_diagnostic;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
//== Position ==================================================================
typedef struct qljs_sublime_text_position {
  qljs_sublime_text_offset line;
  qljs_sublime_text_offset character;
} qljs_sublime_text_position;

//== Range =====================================================================
typedef struct qljs_sublime_text_range {
  qljs_sublime_text_position start;
  qljs_sublime_text_position end;
} qljs_sublime_text_range;
#else
typedef struct qljs_sublime_text_range {
  qljs_sublime_text_offset begin;
  qljs_sublime_text_offset end;
} qljs_sublime_text_range;

qljs_sublime_text_range *qljs_sublime_text_range_new(qljs_sublime_text_offset begin,
                                                     qljs_sublime_text_offset end);
#endif

void qljs_sublime_text_range_delete(qljs_sublime_text_range *r);

//== Text ======================================================================
typedef struct qljs_sublime_text_text {
  const char *content;
  size_t length;
} qljs_sublime_text_text;

qljs_sublime_text_text *qljs_sublime_text_text_new(const char *content, size_t length);

void qljs_sublime_text_text_delete(qljs_sublime_text_text *t);

//== Document ==================================================================
typedef struct qljs_sublime_text_document {
} qljs_sublime_text_document;

qljs_sublime_text_document *qljs_sublime_text_document_new(void);

void qljs_sublime_text_document_delete(qljs_sublime_text_document *d);

void qljs_sublime_text_document_set_text(qljs_sublime_text_document *d,
                                         qljs_sublime_text_text *text);

#if QLJS_ST_HAVE_INCREMENTAL_CHANGES
void qljs_sublime_text_document_replace_text(qljs_sublime_text_document *d,
                                             const qljs_sublime_text_range *range,
                                             const qljs_sublime_text_text *text);
#endif

const qljs_sublime_text_diagnostic *
qljs_sublime_text_document_lint(qljs_sublime_text_document *d);

#if defined(__cplusplus)
}
#endif

#endif // QUICK_LINT_JS_SUBLIME_TEXT_H

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
