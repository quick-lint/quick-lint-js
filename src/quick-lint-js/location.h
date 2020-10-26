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

#ifndef QUICK_LINT_JS_LOCATION_H
#define QUICK_LINT_JS_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
struct source_position {
  using line_number_type = int;
  using offset_type = std::size_t;

  line_number_type line_number;
  int column_number;
  offset_type offset;

  bool operator==(const source_position& other) const noexcept {
    return this->line_number == other.line_number &&
           this->column_number == other.column_number &&
           this->offset == other.offset;
  }

  bool operator!=(const source_position& other) const noexcept {
    return !(*this == other);
  }
};

std::ostream& operator<<(std::ostream&, const source_position&);

class source_range {
 public:
  using offset = source_position::offset_type;

  explicit source_range(source_position begin, source_position end) noexcept
      : begin_(begin), end_(end) {}

  offset begin_offset() const noexcept { return this->begin_.offset; }
  source_position begin() const noexcept;

  offset end_offset() const noexcept { return this->end_.offset; }
  source_position end() const noexcept;

 private:
  source_position begin_;
  source_position end_;
};

class source_code_span {
 public:
  explicit source_code_span(const char8* begin, const char8* end) noexcept
      : begin_(begin), end_(end) {}

  const char8* begin() const noexcept { return this->begin_; }

  const char8* end() const noexcept { return this->end_; }

  string8_view string_view() const noexcept {
    return string8_view(this->begin(),
                        narrow_cast<std::size_t>(this->end() - this->begin()));
  }

  bool operator<(const source_code_span& other) const noexcept;

 private:
  const char8* begin_;
  const char8* end_;
};

bool operator==(source_code_span, string8_view) noexcept;
bool operator!=(source_code_span, string8_view) noexcept;

class locator {
 public:
  explicit locator(padded_string_view input) noexcept;

  source_range range(source_code_span) const;
  source_position position(const char8*) const noexcept;

 private:
  void cache_offsets_of_lines() const;

  source_position::line_number_type find_line_at_offset(
      source_position::offset_type offset) const;

  source_position::offset_type offset(const char8*) const noexcept;

  source_position position(source_position::line_number_type line_number,
                           source_position::offset_type offset) const noexcept;

  padded_string_view input_;
  mutable std::vector<source_position::offset_type> offset_of_lines_;
};
}

#endif
