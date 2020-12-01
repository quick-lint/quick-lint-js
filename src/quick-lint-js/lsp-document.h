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

#ifndef QUICK_LINT_JS_LSP_DOCUMENT_H
#define QUICK_LINT_JS_LSP_DOCUMENT_H

#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
class lsp_document {
 public:
  explicit lsp_document();

  void set_text(string8_view new_text);
  void replace_text(lsp_range range, string8_view replacement_text);

  padded_string_view string() noexcept;

 private:
  int active_content_buffer_ = 0;
  padded_string content_buffers_[2];
  lsp_locator locator_;
};
}

#endif
