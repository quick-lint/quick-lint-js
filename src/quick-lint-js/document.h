// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DOCUMENT_H
#define QUICK_LINT_JS_DOCUMENT_H

#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
template <class Locator>
class document {
 public:
  explicit document();

  void set_text(string8_view new_text);
  void replace_text(typename Locator::range_type range,
                    string8_view replacement_text);

  padded_string_view string() noexcept;
  const Locator& locator() noexcept;

 private:
  int active_content_buffer_ = 0;
  padded_string content_buffers_[2];
  Locator locator_;
};

extern template class document<lsp_locator>;
}

#endif

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
