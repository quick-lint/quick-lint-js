// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

//==============================================================================
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          sublime text c interface                          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_INTERFACE_H
#define QUICK_LINT_JS_SUBLIME_TEXT_INTERFACE_H

#include <quick-lint-js/sublime-text-have.h>
#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif

//==============================================================================
//------------------------------------------------------------------------------
// offset

typedef unsigned int qljs_sublime_text_offset;

//==============================================================================
//------------------------------------------------------------------------------
// severity

typedef enum qljs_sublime_text_severity {
  qljs_sublime_text_severity_error = 1,
  qljs_sublime_text_severity_warning = 2,
} qljs_sublime_text_severity;

//==============================================================================
//------------------------------------------------------------------------------
// text

typedef struct qljs_sublime_text_text {
  char *content;
  size_t length;
} qljs_sublime_text_text;

//==============================================================================
//------------------------------------------------------------------------------
// region

typedef struct qljs_sublime_text_region {
  qljs_sublime_text_offset start;
  qljs_sublime_text_offset end;
} qljs_sublime_text_region;

//==============================================================================
//------------------------------------------------------------------------------
// position

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
typedef struct qljs_sublime_text_position {
  qljs_sublime_text_offset line;
  qljs_sublime_text_offset character;
} qljs_sublime_text_position;
#else
typedef qljs_sublime_text_offset qljs_sublime_text_position;
#endif

//==============================================================================
//------------------------------------------------------------------------------
// range

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
typedef struct qljs_sublime_text_range {
  qljs_sublime_text_position start;
  qljs_sublime_text_position end;
} qljs_sublime_text_range;
#else
typedef qljs_sublime_text_region qljs_sublime_text_range;
#endif

//==============================================================================
//------------------------------------------------------------------------------
// diagnostic

typedef struct qljs_sublime_text_diagnostic {
  const qljs_sublime_text_range *range;
  qljs_sublime_text_severity severity;
  const char *code;
  const char *message;
} qljs_sublime_text_diagnostic;

//==============================================================================
//------------------------------------------------------------------------------
// document

typedef struct qljs_sublime_text_document qljs_sublime_text_document;

qljs_sublime_text_document *qljs_sublime_text_document_new(void);

void qljs_sublime_text_document_delete(qljs_sublime_text_document *document);

void qljs_sublime_text_document_set_text(qljs_sublime_text_document *document,
                                         qljs_sublime_text_text text);

void qljs_sublime_text_document_replace_text(
    qljs_sublime_text_document *document, const qljs_sublime_text_range range,
    const qljs_sublime_text_text text);

const qljs_sublime_text_diagnostic *qljs_sublime_text_document_lint(
    qljs_sublime_text_document *document);

#if defined(__cplusplus)
}
#endif

#endif  // QUICK_LINT_JS_SUBLIME_TEXT_INTERFACE_H

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
