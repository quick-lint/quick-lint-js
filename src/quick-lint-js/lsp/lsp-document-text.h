// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_LSP_DOCUMENT_TEXT_H
#define QUICK_LINT_JS_LSP_LSP_DOCUMENT_TEXT_H

#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
class double_buffered_padded_string {
 public:
  void set_text(string8_view new_text);

  // to_replace must be a substring of active_buffer.
  void replace_text(string8_view to_replace, string8_view replacement_text);

  padded_string_view string() const;

 private:
  padded_string& active_buffer();
  padded_string& inactive_buffer();
  void swap_buffers();

  int active_content_buffer_ = 0;
  padded_string content_buffers_[2];
};

class lsp_document_text {
 public:
  explicit lsp_document_text();

  void set_text(string8_view new_text);
  void replace_text(lsp_range range, string8_view replacement_text);

  padded_string_view string() noexcept;
  const lsp_locator& locator() noexcept;

 private:
  double_buffered_padded_string buffers_;
  lsp_locator locator_;
};
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
