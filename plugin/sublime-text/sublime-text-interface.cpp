// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

//==============================================================================
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          sublime text c interface                          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#include <quick-lint-js/c-api-error-reporter.h>
#include <quick-lint-js/document-base.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/sublime-text-interface.h>
#include <quick-lint-js/sublime-text-location.h>

//==============================================================================
//------------------------------------------------------------------------------
// text

qljs_sublime_text_text *qljs_sublime_text_text_new(char *content, size_t len) {
  return new qljs_sublime_text_text{.content = content, .length = length};
}

void qljs_sublime_text_text_delete(qljs_sublime_text_text *text) {
  delete text;
}

//==============================================================================
//------------------------------------------------------------------------------
// region

qljs_sublime_text_region *qljs_sublime_text_region_new(
    qljs_sublime_text_offset start, qljs_sublime_text_offset end) {
  return new qljs_sublime_text_region{.start = start, .end = end};
}

void qljs_sublime_text_region_delete(qljs_sublime_text_region *region) {
  delete region;
}

//==============================================================================
//------------------------------------------------------------------------------
// position

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
qljs_sublime_text_position *qljs_sublime_text_position_new(
    qljs_sublime_text_offset line, qljs_sublime_text_offset character) {
  return new qljs_sublime_text_position{.line = line, .character = character};
}

void qljs_sublime_text_position_delete(qljs_sublime_text_position *position) {
  delete position;
}
#endif

//==============================================================================
//------------------------------------------------------------------------------
// range

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
qljs_sublime_text_range *qljs_sublime_text_range_new(
    qljs_sublime_text_position start, qljs_sublime_text_position end) {
  return new qljs_sublime_text_range{.start = start, .end = end};
}

void qljs_sublime_text_range_delete(qljs_sublime_text_range *range) {
  delete range;
}
#endif

//==============================================================================
//------------------------------------------------------------------------------
// diagnostic

namespace quick_lint_js {
namespace {

struct sublime_text_diagnostic final : public qljs_sublime_text_diagnostic {};

}  // namespace
}  // namespace quick_lint_js

//==============================================================================
//------------------------------------------------------------------------------
// document

using qljs_sublime_text_document_base =
    quick_lint_js::document_base<quick_lint_js::sublime_text_locator,
                                 quick_lint_js::c_api_error_reporter,
                                 quick_lint_js::sublime_text_diagnostic>;

struct qljs_sublime_text_document final
    : public qljs_sublime_text_document_base {
 public:
  using range_type = typename quick_lint_js::sublime_text_locator::range_type;

  void set_text(quick_lint_js::string8_view replacement) {
    this->document_.set_text(replacement);
  }

  void replace_text(range_type range, quick_lint_js::string8_view replacement) {
    this->document_.replace_text(range, replacement);
  }
};

qljs_sublime_text_document *qljs_sublime_text_document_new(void) {
  return new qljs_sublime_text_document();
}

void qljs_sublime_text_document_delete(qljs_sublime_text_document *document) {
  delete document;
}

void qljs_sublime_text_document_set_text(qljs_sublime_text_document *document,
                                         const qljs_sublime_text_text *text) {
  using char_type = quick_lint_js::char8;

  auto cpp_content = reinterpret_cast<const char_type *>(text->content);
  auto cpp_replacement = quick_lint_js::string8_view(cpp_content, text->length);
  document->set_text(cpp_replacement);
}

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
void qljs_sublime_text_document_replace_text(
    qljs_sublime_text_document *document, const qljs_sublime_text_range *range,
    const qljs_sublime_text_text *text) {
  using range_type = qljs_sublime_text_document::range_type;
  using char_type = quick_lint_js::char8;

  auto cpp_range = reinterpret_cast<range_type *>(range);
  auto cpp_content = reinterpret_cast<const char_type *>(text->content);
  auto cpp_replacement = quick_lint_js::string8_view(cpp_content, text->length);
  document->replace_text(*cpp_range, cpp_replacement);
}
#endif

const qljs_sublime_text_diagnostic *qljs_sublime_text_document_lint(
    qljs_sublime_text_document *document) {
  return document->lint();
}

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
