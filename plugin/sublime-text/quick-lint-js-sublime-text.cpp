// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js-sublime-text-location.h>
#include <quick-lint-js-sublime-text.h>
#include <quick-lint-js/document-base.h>

namespace quick_lint_js {
namespace {
// NOTE: Sublime Text 4 uses quick_lint_js::lsp_locator

using sublime_text_document =
    document_base<sublime_text_locator, c_api_error_reporter,
                  qljs_st_diagnostic>;
}  // namespace
}  // namespace quick_lint_js

struct qljs_st_document final : public quick_lint_js::sublime_text_document {
 public:
  void set_text(quick_lint_js::string8_view replacement) {
    this->document_.set_text(replacement);
  }

  void replace_text(quick_lint_js::lsp_range range,
                    quick_lint_js::string8_view replacement) {
    this->document_.replace_text(range, replacement);
  }
};

qljs_st_document* qljs_st_document_new(void) { return new qljs_st_document(); }

void qljs_st_document_delete(qljs_st_document* document) { delete document; }

void qljs_st_document_set_text(qljs_st_document* document, const qljs_st_text* text) {
  auto content8 = reinterpret_cast<const quick_lint_js::char8*>(text->content);
  auto replacement8 = quick_lint_js::string8_view(content, text->length);
  document->set_text(replacement8);
}

void qljs_st_document_replace_text(qljs_st_document* document,
                                 const qljs_st_range* range,
                                 const qljs_st_text* text) {
  auto lrange = reinterpret_cast<const quick_lint_js::lsp_range*>(range);
  auto content8 = reinterpret_cast<const quick_lint_js::char8*>(text->content);
  auto replacement8 = quick_lint_js::string8_view(content8, text->length);
  document->replace_text(lrange, replacement8);
}

const qljs_st_diagnostic* qljs_st_document_lint(qljs_st_document* document) {
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
