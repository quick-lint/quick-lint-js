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
lsp_document::lsp_document()
    : locator_(&this->content_buffers_[this->active_content_buffer_]) {}

void lsp_document::set_text(string8_view new_text) {
  padded_string& content = this->content_buffers_[this->active_content_buffer_];
  content.resize(narrow_cast<int>(new_text.size()));
  std::memcpy(content.data(), new_text.data(), new_text.size());
  this->locator_ = lsp_locator(&content);
}

void lsp_document::replace_text(lsp_range range,
                                string8_view replacement_text) {
  padded_string& old_content =
      this->content_buffers_[this->active_content_buffer_];
  padded_string& new_content =
      this->content_buffers_[1 - this->active_content_buffer_];

  const char8* start = this->locator_.from_position(range.start);
  const char8* end = this->locator_.from_position(range.end);

  new_content.resize(narrow_cast<int>(
      (start - old_content.begin()) +
      narrow_cast<int>(replacement_text.size()) + (old_content.end() - end)));
  char8* out = new_content.data();
  out = std::copy(old_content.cbegin(), start, out);
  out = std::copy(replacement_text.begin(), replacement_text.end(), out);
  out = std::copy(end, old_content.cend(), out);
  QLJS_ASSERT(out == new_content.end());

  this->locator_.replace_text(range, replacement_text, &new_content);
  this->active_content_buffer_ = 1 - this->active_content_buffer_;
}

padded_string_view lsp_document::string() noexcept {
  return &this->content_buffers_[this->active_content_buffer_];
}

const lsp_locator& lsp_document::locator() noexcept { return this->locator_; }
}
