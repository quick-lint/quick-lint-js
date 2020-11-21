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
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
bool operator==(const lsp_position &lhs, const lsp_position &rhs) noexcept {
  return lhs.line == rhs.line && lhs.character == rhs.character;
}

bool operator!=(const lsp_position &lhs, const lsp_position &rhs) noexcept {
  return !(lhs == rhs);
}

std::ostream &operator<<(std::ostream &stream, const lsp_position &position) {
  stream << "line " << position.line << " character " << position.character;
  return stream;
}

lsp_locator::lsp_locator(padded_string_view input) noexcept : input_(input) {}

lsp_range lsp_locator::range(source_code_span span) const {
  lsp_position start = this->position(span.begin());
  lsp_position end = this->position(span.end());
  return lsp_range{.start = start, .end = end};
}

lsp_position lsp_locator::position(const char8 *source) const noexcept {
  offset_type offset = this->offset(source);
  int line_number = this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

char8 *lsp_locator::from_position(lsp_position position) const noexcept {
  int line = position.line;
  int character = position.character;
  if (line < 0 || character < 0) {
    return nullptr;
  }

  if (this->offset_of_lines_.empty()) {
    this->cache_offsets_of_lines();
  }
  int number_of_lines = narrow_cast<int>(this->offset_of_lines_.size());
  if (line >= number_of_lines) {
    return nullptr;
  }

  offset_type line_begin_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line)];
  bool is_last_line = line == number_of_lines - 1;
  if (is_last_line) {
    offset_type line_length = this->input_.size() - line_begin_offset;
    if (character > line_length) {
      return &this->input_[this->input_.size()];
    } else {
      return &this->input_[line_begin_offset + character];
    }
  } else {
    offset_type line_end_offset =
        this->offset_of_lines_[narrow_cast<std::size_t>(line + 1)];
    offset_type line_length_including_terminator =
        line_end_offset - line_begin_offset;
    bool character_is_out_of_bounds =
        character >= line_length_including_terminator - 1;
    if (character_is_out_of_bounds) {
      if (line_length_including_terminator >= 2 &&
          this->input_[line_end_offset - 2] == u8'\r' &&
          this->input_[line_end_offset - 1] == u8'\n') {
        // Return the "\r\n".
        return &this->input_[line_end_offset - 2];
      } else {
        // Return the "\n" or the "\r".
        return &this->input_[line_end_offset - 1];
      }
    } else {
      return &this->input_[line_begin_offset + character];
    }
  }
}

void lsp_locator::cache_offsets_of_lines() const {
  auto add_beginning_of_line = [this](const char8 *beginning_of_line) -> void {
    this->offset_of_lines_.push_back(
        narrow_cast<offset_type>(beginning_of_line - this->input_.data()));
  };
  this->offset_of_lines_.push_back(0);
  for (const char8 *c = this->input_.data();
       c != this->input_.null_terminator();) {
    if (*c == u8'\n' || *c == u8'\r') {
      if (c[0] == u8'\r' && c[1] == u8'\n') {
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

int lsp_locator::find_line_at_offset(offset_type offset) const {
  if (this->offset_of_lines_.empty()) {
    this->cache_offsets_of_lines();
  }
  QLJS_ASSERT(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::upper_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<int>((offset_of_following_line_it - 1) -
                          this->offset_of_lines_.begin());
}

lsp_locator::offset_type lsp_locator::offset(const char8 *source) const
    noexcept {
  return narrow_cast<offset_type>(source - this->input_.data());
}

lsp_position lsp_locator::position(int line_number, offset_type offset) const
    noexcept {
  offset_type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number)];
  int column_number = narrow_cast<int>(offset - beginning_of_line_offset);
  return lsp_position{.line = line_number, .character = column_number};
}
}
