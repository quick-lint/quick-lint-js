// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
CLI_Source_Position CLI_Source_Range::begin() const { return this->begin_; }

CLI_Source_Position CLI_Source_Range::end() const { return this->end_; }

CLI_Locator::CLI_Locator(Padded_String_View input) : input_(input) {}

CLI_Source_Range CLI_Locator::range(Source_Code_Span span) const {
  CLI_Source_Position begin = this->position(span.begin());
  CLI_Source_Position end = this->position(span.end());
  return CLI_Source_Range(begin, end);
}

CLI_Source_Position CLI_Locator::position(const Char8 *source) const {
  CLI_Source_Position::Offset_Type offset = this->offset(source);
  CLI_Source_Position::Line_Number_Type line_number =
      this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

[[gnu::noinline]] void CLI_Locator::cache_offsets_of_lines() const {
  auto add_beginning_of_line = [this](const Char8 *beginning_of_line) -> void {
    this->offset_of_lines_.push_back(
        narrow_cast<CLI_Source_Position::Offset_Type>(beginning_of_line -
                                                      this->input_.data()));
  };
  this->offset_of_lines_.push_back(0);
  for (const Char8 *c = this->input_.data();
       c != this->input_.null_terminator();) {
    if (*c == '\n' || *c == '\r') {
      c += (c[0] == '\r' && c[1] == '\n') ? 2 : 1;
      add_beginning_of_line(c);
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

CLI_Source_Position::Line_Number_Type CLI_Locator::find_line_at_offset(
    CLI_Source_Position::Offset_Type offset) const {
  if (this->offset_of_lines_.empty()) {
    this->cache_offsets_of_lines();
  }
  QLJS_ASSERT(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::upper_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<CLI_Source_Position::Line_Number_Type>(
             (offset_of_following_line_it - 1) -
             this->offset_of_lines_.begin()) +
         1;
}

CLI_Source_Position::Offset_Type CLI_Locator::offset(
    const Char8 *source) const {
  return narrow_cast<CLI_Source_Position::Offset_Type>(source -
                                                       this->input_.data());
}

CLI_Source_Position CLI_Locator::position(
    CLI_Source_Position::Line_Number_Type line_number,
    CLI_Source_Position::Offset_Type offset) const {
  CLI_Source_Position::Offset_Type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number - 1)];
  int column_number = narrow_cast<int>(offset - beginning_of_line_offset) + 1;
  return CLI_Source_Position{line_number, column_number, offset};
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
