// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/vim-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/cast.h>

namespace quick_lint_js {
Vim_Locator::Vim_Locator(Padded_String_View input) : input_(input) {}

Vim_Source_Range Vim_Locator::range(Source_Code_Span span) const {
  Vim_Source_Position begin = this->position(span.begin());
  Vim_Source_Position end = this->position(span.end());
  return Vim_Source_Range{.begin = begin, .end = end};
}

Vim_Source_Position Vim_Locator::position(const Char8 *source) const {
  Offset_Type offset = this->offset(source);
  int line_number = this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

[[gnu::noinline]] void Vim_Locator::cache_offsets_of_lines() const {
  auto add_beginning_of_line = [this](const Char8 *beginning_of_line) -> void {
    this->offset_of_lines_.push_back(
        narrow_cast<Offset_Type>(beginning_of_line - this->input_.data()));
  };
  this->offset_of_lines_.push_back(0);
  for (const Char8 *c = this->input_.data();
       c != this->input_.null_terminator();) {
    if (*c == '\n' || *c == '\r') {
      if (c[0] == '\r' && c[1] == '\n') {
        c += 2;
        add_beginning_of_line(c);
      } else {
        c += 1;
        add_beginning_of_line(c);
      }
    } else {
      c += 1;
    }
  }
}

int Vim_Locator::find_line_at_offset(Offset_Type offset) const {
  if (this->offset_of_lines_.empty()) {
    this->cache_offsets_of_lines();
  }
  QLJS_ASSERT(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::upper_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<int>((offset_of_following_line_it - 1) -
                          this->offset_of_lines_.begin()) +
         1;
}

Vim_Locator::Offset_Type Vim_Locator::offset(const Char8 *source) const {
  return narrow_cast<Offset_Type>(source - this->input_.data());
}

Vim_Source_Position Vim_Locator::position(int line_number,
                                          Offset_Type offset) const {
  Offset_Type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number - 1)];
  int col = narrow_cast<int>(offset - beginning_of_line_offset) + 1;
  return Vim_Source_Position{line_number, col};
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
