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

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-document.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
void lsp_document::set_text(string8_view new_text) {
  padded_string result;
  this->content_.resize(narrow_cast<int>(new_text.size()));
  std::memcpy(this->content_.data(), new_text.data(), new_text.size());
}

void lsp_document::replace_text(lsp_range range,
                                string8_view replacement_text) {
  lsp_locator content_locator(&this->content_);
  char8* start = content_locator.from_position(range.start);
  char8* end = content_locator.from_position(range.end);

  padded_string new_content;
  new_content.resize(
      narrow_cast<int>((start - this->content_.begin()) +
                       narrow_cast<int>(replacement_text.size()) +
                       (this->content_.end() - end)));
  char8* out = new_content.data();
  out = std::copy(this->content_.begin(), start, out);
  out = std::copy(replacement_text.begin(), replacement_text.end(), out);
  out = std::copy(end, this->content_.end(), out);
  QLJS_ASSERT(out == new_content.end());

  this->content_ = new_content;
}

padded_string_view lsp_document::string() noexcept { return &this->content_; }
}
