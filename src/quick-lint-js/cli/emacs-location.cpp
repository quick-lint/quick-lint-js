// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/emacs-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/utf-8.h>

namespace quick_lint_js {
Emacs_Source_Position Emacs_Source_Range::begin() const noexcept {
  return this->begin_;
}

Emacs_Source_Position Emacs_Source_Range::end() const noexcept {
  return this->end_;
}

Emacs_Locator::Emacs_Locator(Padded_String_View input) noexcept
    : input_(input) {}

Emacs_Source_Range Emacs_Locator::range(Source_Code_Span span) const {
  Emacs_Source_Position begin = this->position(span.begin());
  Emacs_Source_Position end = this->position(span.end());
  return Emacs_Source_Range(begin, end);
}

Emacs_Source_Position Emacs_Locator::position(const Char8 *source) const
    noexcept {
  Emacs_Source_Position::Offset_Type offset = this->offset(source);
  // Emacs point starts at 1
  return this->position(offset + 1);
}

Emacs_Source_Position::Offset_Type Emacs_Locator::offset(
    const Char8 *source) const noexcept {
  std::size_t offset = narrow_cast<std::size_t>(source - this->input_.data());
  return narrow_cast<Emacs_Source_Position::Offset_Type>(
      count_utf_8_characters(this->input_, offset));
}

Emacs_Source_Position Emacs_Locator::position(
    Emacs_Source_Position::Offset_Type offset) const noexcept {
  return Emacs_Source_Position{offset};
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
