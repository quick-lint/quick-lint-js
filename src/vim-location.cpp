// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <ostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/vim-location.h>

namespace quick_lint_js {
std::ostream &operator<<(std::ostream &out, const vim_source_position &p) {
  out << "vim_source_position{" << p.lnum << ',' << p.col << '}';
  return out;
}

vim_locator::vim_locator(padded_string_view input) noexcept : input_(input) {}

vim_source_range vim_locator::range(source_code_span span) const {
  vim_source_position begin = this->position(span.begin());
  vim_source_position end = this->position(span.end());
  return vim_source_range{.begin = begin, .end = end};
}

vim_source_position vim_locator::position(const char8 *source) const noexcept {
  offset_type offset = this->offset(source);
  int line_number = this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

[[gnu::noinline]] void vim_locator::cache_offsets_of_lines() const {
  auto add_beginning_of_line = [this](const char8 *beginning_of_line) -> void {
    this->offset_of_lines_.push_back(
        narrow_cast<offset_type>(beginning_of_line - this->input_.data()));
  };
  this->offset_of_lines_.push_back(0);
  for (const char8 *c = this->input_.data();
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

int vim_locator::find_line_at_offset(offset_type offset) const {
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

vim_locator::offset_type vim_locator::offset(const char8 *source) const
    noexcept {
  return narrow_cast<offset_type>(source - this->input_.data());
}

vim_source_position vim_locator::position(int line_number,
                                          offset_type offset) const noexcept {
  offset_type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number - 1)];
  int col = narrow_cast<int>(offset - beginning_of_line_offset) + 1;
  return vim_source_position{line_number, col};
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
