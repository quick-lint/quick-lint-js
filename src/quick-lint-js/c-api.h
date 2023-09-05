// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <stddef.h>

#if defined(__cplusplus)
extern "C" {
#endif

// A bit set (i.e. flags) which tell qljs_web_demo_lint how to interpret a
// QLJS_Web_Demo_Document's text.
//
// To associate options with a document, call
// qljs_web_demo_set_language_options.
typedef enum QLJS_Language_Options {
  // If set, parse JSX syntax. JSX is a JavaScript language extension.
  //
  // If unset, report a diagnostic if JSX syntax is encounted (e.g. E0177 or
  // E0306).
  //
  // Ignored if qljs_language_options_config_json_bit is set.
  qljs_language_options_jsx_bit = 1 << 0,

  // If set, parse TypeScript instead of JavaScript.
  //
  // If unset, parse JavaScript, and report a diagnostic if TypeScript-specific
  // syntax is encountered (e.g. E0222 or E0281).
  //
  // Ignored if qljs_language_options_config_json_bit is set.
  qljs_language_options_typescript_bit = 1 << 1,

  // If set, parse a quick-lint-js.config file instead of JavaScript.
  //
  // If unset, parse JavaScript or TypeScript.
  qljs_language_options_config_json_bit = 1 << 2,
} QLJS_Language_Options;

typedef enum QLJS_Severity {
  qljs_severity_error = 1,
  qljs_severity_warning = 2,
} QLJS_Severity;

// A QLJS_Web_Demo_Document is a text document.
//
// A QLJS_Web_Demo_Document contains the following state:
//
// * Text, changed using qljs_web_demo_set_text
// * Language options, changed using qljs_web_demo_set_language_options
// * Configuration document, changed using qljs_web_demo_set_config
// * Locale, changed using qljs_web_demo_set_locale
// * Output diagnostics, changed using qljs_web_demo_lint
//
// QLJS_Web_Demo_Document objects are allocated dynamically. To create a
// QLJS_Web_Demo_Document, call qljs_web_demo_create_document. When you are
// finished using a QLJS_Web_Demo_Document, call qljs_web_demo_destroy_document
// to free resources.
//
// NOTE[QLJS_Web_Demo_Document threads]: In general, qljs_web_demo_* functions
// can be called from multiple threads without synchronization. However, for a
// given QLJS_Web_Demo_Document, functions accepting that QLJS_Web_Demo_Document
// *cannot* be called from multiple threads without synchronization.
//
// In other words, you can create documents A, B, and C, and use document A on
// thread 1, document B on thread 2, and document C on thread 3, with no
// synchronization. However, if instead you want to call
// qljs_web_demo_set_text(A, ...) on thread 1, then call qljs_web_demo_lint(A)
// on thread 2, then these calls must be synchronized by you.
//
// A mutex is sufficient synchronization.
typedef struct QLJS_Web_Demo_Document QLJS_Web_Demo_Document;

struct QLJS_Web_Demo_Diagnostic {
  const char* message;
  char code[6];  // null-terminated
  QLJS_Severity severity;
  // Offsets count UTF-16 code units.
  int begin_offset;
  int end_offset;
};

// Create a new document.
//
// The new document ('d') has the following state:
//
// * No text, as if by qljs_web_demo_set_text(d, "", 0)
// * No language options set, as if by qljs_web_demo_set_language_options(d, 0)
// * No configuration document, as if by qljs_web_demo_set_config(d, NULL)
// * A default locale, as if by qljs_web_demo_set_locale(d, default_locale)
//   * TODO(strager): What is default_locale?
// * Unspecified output diagnostics
//
// Thread safety: Thread-safe. Not async-signal-safe.
//
// Postcondition: The returned value is not null.
QLJS_Web_Demo_Document* qljs_web_demo_create_document(void);

// Free resources which were allocated for the given document.
//
// After calling qljs_web_demo_destroy_document, the document pointer should
// never be used.
//
// Thread safety: See NOTE[QLJS_Web_Demo_Document threads].
//
// Precondition: qljs_web_demo_create_document() previously returned document.
// Precondition: qljs_web_demo_destroy_document(document) was not previously
//               called.
void qljs_web_demo_destroy_document(QLJS_Web_Demo_Document* document);

// Make qljs_web_demo_lint use this text.
//
// qljs_web_demo_set_text makes an internal copy of the given array. To change
// the document's text, you cannot just modify the array pointed to by
// text_utf_8; you must call qljs_web_demo_set_text again.
//
// If qljs_web_demo_set_config(js_document, document) was previously called,
// then in order for the new config to take effect for js_document,
// qljs_web_demo_lint(js_document) must be called. (You couldn't notice without
// calling qljs_web_demo_lint anyway...)
//
// Thread safety: See NOTE[QLJS_Web_Demo_Document threads].
//
// Precondition: qljs_web_demo_create_document() returned document, and
//               qljs_web_demo_destroy_document(document) has not been called.
// Precondition: text_utf_8 points to an array of at least text_byte_count
//               bytes.
// Precondition: text_utf_8 is not null, even if text_byte_count is 0.
void qljs_web_demo_set_text(QLJS_Web_Demo_Document* document,
                            const void* text_utf_8, size_t text_byte_count);

// When running qljs_web_demo_lint(js_document), treat config_document's text as
// if it was js_document's associated quick-lint-js.config file.
//
// config_document's language options are ignored.
//
// config_document is optional. If null, reverts to the default config.
//
// Thread safety: See NOTE[QLJS_Web_Demo_Document threads].
//
// Precondition: qljs_web_demo_create_document() returned js_document, and
//               qljs_web_demo_destroy_document(js_document) has not been
//               called.
// Precondition: config_document is null, or: qljs_web_demo_create_document()
//               returned config_document, and
//               qljs_web_demo_destroy_document(config_document) has not been
//               called.
void qljs_web_demo_set_config(QLJS_Web_Demo_Document* js_document,
                              QLJS_Web_Demo_Document* config_document);

// Change how qljs_web_demo_lint(document) parses and interprets document's
// text.
//
// options is a bit set. See QLJS_Language_Options for details.
//
// Thread safety: See NOTE[QLJS_Web_Demo_Document threads].
//
// Precondition: qljs_web_demo_create_document() returned document, and
//               qljs_web_demo_destroy_document(document) has not been called.
// Precondition: options is a bitwise-or of zero or more QLJS_Language_Options
//               members. (options==0 is permitted.)
void qljs_web_demo_set_language_options(QLJS_Web_Demo_Document* document,
                                        QLJS_Language_Options options);

// Change the human language which qljs_web_demo_lint(document) uses for its
// diagnostics.
//
// locale can compare equal to a string returned by qljs_list_locales, or it can
// be any other string.
//
// If locale matches no supported locales, then this sets document's locale to
// the default locale (which corresponds to professional US English).
//
// Thread safety: See NOTE[QLJS_Web_Demo_Document threads].
//
// Precondition: qljs_web_demo_create_document() returned document, and
//               qljs_web_demo_destroy_document(document) has not been called.
// Precondition: locale points to a C string.
// Precondition: locale is not null.
void qljs_web_demo_set_locale(QLJS_Web_Demo_Document* document,
                              const char* locale);

// Parse and lint document's text [1], according to its language options [2] and
// config [3], and return a list of diagnostics according to document's
// locale [4].
//
// The returned pointer refers to an array of QLJS_Web_Demo_Diagnostic objects.
// The array is terminated by an item where:
// * QLJS_Web_Demo_Diagnostic::message is null, and
// * QLJS_Web_Demo_Diagnostic::code is an empty string.
//
// The returned pointer is valid until either the next call to
// qljs_web_demo_lint(document) or a call to
// qljs_web_demo_destroy_document(document), whichever comes first.
//
// [1] qljs_web_demo_set_text
// [2] qljs_web_demo_set_language_options
// [3] qljs_web_demo_set_config
// [4] qljs_web_demo_set_locale
//
// Thread safety: See NOTE[QLJS_Web_Demo_Document threads].
//
// Precondition: qljs_web_demo_create_document() returned document, and
//               qljs_web_demo_destroy_document(document) has not been called.
// Precondition: qljs_web_demo_destroy_document(config_document) has not been
//               called, where config_document is the QLJS_Web_Demo_Document
//               associated with this document via qljs_web_demo_set_config.
// Postcondition: The returned value is not null.
const QLJS_Web_Demo_Diagnostic* qljs_web_demo_lint(
    QLJS_Web_Demo_Document* document);

// Returns a null-terminated array of null-terminated strings.
//
// Every call to qljs_list_locales will return the same pointer (for a given
// process).
//
// Thread safety: Thread-safe. Not async-signal-safe.
//
// Postcondition: The returned value is not null.
// Postcondition: The returned array contains at least one non-empty string.
const char* const* qljs_list_locales();

#if defined(__cplusplus)
}
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
