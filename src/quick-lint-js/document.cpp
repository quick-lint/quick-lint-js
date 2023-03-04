// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/web-demo-location.h>

QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=noreturn")

namespace quick_lint_js {
void double_buffered_padded_string::set_text(string8_view new_text) {
  padded_string& content = this->active_buffer();
  content.resize(narrow_cast<int>(new_text.size()));
  std::memcpy(content.data(), new_text.data(), new_text.size());
}

void double_buffered_padded_string::replace_text(
    string8_view to_replace, string8_view replacement_text) {
  const char8* to_replace_begin = to_replace.data();
  const char8* to_replace_end = to_replace_begin + to_replace.size();
  padded_string& old_content = this->active_buffer();
  padded_string& new_content = this->inactive_buffer();

  new_content.resize(
      narrow_cast<int>((to_replace_begin - old_content.begin()) +
                       narrow_cast<int>(replacement_text.size()) +
                       (old_content.end() - to_replace_end)));
  char8* out = new_content.data();
  out = std::copy(old_content.cbegin(), to_replace_begin, out);
  out = std::copy(replacement_text.begin(), replacement_text.end(), out);
  out = std::copy(to_replace_end, old_content.cend(), out);
  QLJS_ASSERT(out == new_content.end());

  this->swap_buffers();
}

padded_string_view double_buffered_padded_string::string() const {
  return &const_cast<double_buffered_padded_string*>(this)->active_buffer();
}

padded_string& double_buffered_padded_string::active_buffer() {
  return this->content_buffers_[this->active_content_buffer_];
}

padded_string& double_buffered_padded_string::inactive_buffer() {
  return this->content_buffers_[1 - this->active_content_buffer_];
}

void double_buffered_padded_string::swap_buffers() {
  this->active_content_buffer_ = 1 - this->active_content_buffer_;
}

template <class Locator>
document<Locator>::document() : locator_(this->buffers_.string()) {}

template <class Locator>
void document<Locator>::set_text(string8_view new_text) {
  this->buffers_.set_text(new_text);
  this->locator_ = Locator(this->buffers_.string());
}

template <class Locator>
void document<Locator>::replace_text(typename Locator::range_type range,
                                     string8_view replacement_text) {
  this->buffers_.replace_text(
      make_string_view(this->locator_.from_position(range.start),
                       this->locator_.from_position(range.end)),
      replacement_text);
  this->locator_.replace_text(range, replacement_text, this->buffers_.string());
}

template <class Locator>
padded_string_view document<Locator>::string() noexcept {
  return this->buffers_.string();
}

template <class Locator>
const Locator& document<Locator>::locator() noexcept {
  return this->locator_;
}

template class document<lsp_locator>;
}

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
