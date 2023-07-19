// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_LSP_DOCUMENT_TEXT_H
#define QUICK_LINT_JS_LSP_LSP_DOCUMENT_TEXT_H

#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
class Double_Buffered_Padded_String {
 public:
  void set_text(String8_View new_text);

  // to_replace must be a substring of active_buffer.
  void replace_text(String8_View to_replace, String8_View replacement_text);

  Padded_String_View string() const;

 private:
  Padded_String& active_buffer();
  Padded_String& inactive_buffer();
  void swap_buffers();

  int active_content_buffer_ = 0;
  Padded_String content_buffers_[2];
};

class LSP_Document_Text {
 public:
  explicit LSP_Document_Text();

  void set_text(String8_View new_text);
  void replace_text(LSP_Range range, String8_View replacement_text);

  Padded_String_View string() noexcept;
  const LSP_Locator& locator() noexcept;

 private:
  Double_Buffered_Padded_String buffers_;
  LSP_Locator locator_;
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
