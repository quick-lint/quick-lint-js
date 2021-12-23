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

typedef struct qljs_web_demo_document qljs_web_demo_document;
struct qljs_web_demo_diagnostic {
  const char* message;
  const char* code;
  qljs_severity severity;
  // Offsets count UTF-16 code units.
  int begin_offset;
  int end_offset;
};
qljs_web_demo_document* qljs_web_demo_create_document(void);
void qljs_web_demo_destroy_document(qljs_web_demo_document*);
void qljs_web_demo_set_text(qljs_web_demo_document*, const void* text_utf_8,
                            size_t text_byte_count);
void qljs_web_demo_set_config_text(qljs_web_demo_document*,
                                   const void* text_utf_8,
                                   size_t text_byte_count);
const qljs_web_demo_diagnostic* qljs_web_demo_lint(qljs_web_demo_document*);
const qljs_web_demo_diagnostic* qljs_web_demo_lint_as_config_file(
    qljs_web_demo_document*);

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
