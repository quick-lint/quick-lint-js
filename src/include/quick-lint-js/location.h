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
#include <quick-lint-js/narrow-cast.h>
#include <string_view>

namespace quick_lint_js {
struct source_position {
  using offset_type = std::size_t;

  int line_number;
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
  explicit source_code_span(const char* begin, const char* end) noexcept
      : begin_(begin), end_(end) {}

  const char* begin() const noexcept { return this->begin_; }

  const char* end() const noexcept { return this->end_; }

  std::string_view string_view() const noexcept {
    return std::string_view(
        this->begin(), narrow_cast<std::size_t>(this->end() - this->begin()));
  }

 private:
  const char* begin_;
  const char* end_;
};

bool operator==(source_code_span, std::string_view) noexcept;
bool operator!=(source_code_span, std::string_view) noexcept;

class locator {
 public:
  explicit locator(const char* input) noexcept;

  source_range range(source_code_span) const;
  source_position position(const char*) const noexcept;

 private:
  const char* input_;

  mutable const char* last_position_source_;
  mutable const char* last_position_last_line_terminator_;
  mutable int last_position_number_of_line_terminators_;
};
}  // namespace quick_lint_js

#endif
