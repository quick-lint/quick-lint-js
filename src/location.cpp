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
#include <ostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
std::ostream &operator<<(std::ostream &out, const source_position &p) {
  out << "source_position{" << p.line_number << ',' << p.column_number << ','
      << p.offset << '}';
  return out;
}

source_position source_range::begin() const noexcept { return this->begin_; }

source_position source_range::end() const noexcept { return this->end_; }

bool operator==(source_code_span x, string8_view y) noexcept {
  return x.string_view() == y;
}

bool operator!=(source_code_span x, string8_view y) noexcept {
  return !(x == y);
}

locator::locator(padded_string_view input) noexcept : input_(input) {}

source_range locator::range(source_code_span span) const {
  source_position begin = this->position(span.begin());
  source_position end = this->position(span.end());
  return source_range(begin, end);
}

source_position locator::position(const char8 *source) const noexcept {
  source_position::offset_type offset = this->offset(source);
  source_position::line_number_type line_number =
      this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

void locator::cache_offsets_of_lines() const {
  auto add_beginning_of_line = [this](const char8 *beginning_of_line) -> void {
    this->offset_of_lines_.push_back(narrow_cast<source_position::offset_type>(
        beginning_of_line - this->input_.data()));
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
    } else if (static_cast<unsigned char>(c[0]) == 0xe2 &&
               static_cast<unsigned char>(c[1]) == 0x80) {
      switch (static_cast<unsigned char>(c[2])) {
        case 0xa8:  // U+2028 Line Separator
        case 0xa9:  // U+2029 Paragraph Separator
          c += 3;
          add_beginning_of_line(c);
          break;

        default:
          c += 1;
          break;
      }
    } else {
      c += 1;
    }
  }
}

source_position::line_number_type locator::find_line_at_offset(
    source_position::offset_type offset) const {
  if (this->offset_of_lines_.empty()) {
    this->cache_offsets_of_lines();
  }
  QLJS_ASSERT(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::upper_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<source_position::line_number_type>(
             (offset_of_following_line_it - 1) -
             this->offset_of_lines_.begin()) +
         1;
}

source_position::offset_type locator::offset(const char8 *source) const
    noexcept {
  return narrow_cast<source_position::offset_type>(source -
                                                   this->input_.data());
}

source_position locator::position(source_position::line_number_type line_number,
                                  source_position::offset_type offset) const
    noexcept {
  source_position::offset_type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number - 1)];
  int column_number = narrow_cast<int>(offset - beginning_of_line_offset) + 1;
  return source_position{line_number, column_number, offset};
}
}
