// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <ostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/emacs-location.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/utf-8.h>

namespace quick_lint_js {
std::ostream &operator<<(std::ostream &out, const emacs_source_position &p) {
  out << "emacs_source_position{" << p.offset << '}';
  return out;
}

emacs_source_position emacs_source_range::begin() const noexcept {
  return this->begin_;
}

emacs_source_position emacs_source_range::end() const noexcept {
  return this->end_;
}

emacs_locator::emacs_locator(padded_string_view input) noexcept
    : input_(input) {}

emacs_source_range emacs_locator::range(source_code_span span) const {
  emacs_source_position begin = this->position(span.begin());
  emacs_source_position end = this->position(span.end());
  return emacs_source_range(begin, end);
}

emacs_source_position emacs_locator::position(const char8 *source) const
    noexcept {
  emacs_source_position::offset_type offset = this->offset(source);
  // Emacs point starts at 1
  return this->position(offset + 1);
}

emacs_source_position::offset_type emacs_locator::offset(
    const char8 *source) const noexcept {
  std::size_t offset = narrow_cast<std::size_t>(source - this->input_.data());
  return narrow_cast<emacs_source_position::offset_type>(
      count_utf_8_characters(this->input_, offset));
}

emacs_source_position emacs_locator::position(
    emacs_source_position::offset_type offset) const noexcept {
  return emacs_source_position{offset};
}
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
