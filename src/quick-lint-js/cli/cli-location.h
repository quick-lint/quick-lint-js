// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CLI_CLI_LOCATION_H
#define QUICK_LINT_JS_CLI_CLI_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
struct CLI_Source_Position {
  using Line_Number_Type = int;
  using Offset_Type = std::size_t;

  Line_Number_Type line_number;
  int column_number;
  Offset_Type offset;

  bool operator==(const CLI_Source_Position& other) const {
    return this->line_number == other.line_number &&
           this->column_number == other.column_number &&
           this->offset == other.offset;
  }

  bool operator!=(const CLI_Source_Position& other) const {
    return !(*this == other);
  }
};

std::ostream& operator<<(std::ostream&, const CLI_Source_Position&);

class CLI_Source_Range {
 public:
  using Offset = CLI_Source_Position::Offset_Type;

  explicit CLI_Source_Range(CLI_Source_Position begin, CLI_Source_Position end)
      : begin_(begin), end_(end) {}

  Offset begin_offset() const { return this->begin_.offset; }
  CLI_Source_Position begin() const;

  Offset end_offset() const { return this->end_.offset; }
  CLI_Source_Position end() const;

 private:
  CLI_Source_Position begin_;
  CLI_Source_Position end_;
};

class CLI_Locator {
 public:
  explicit CLI_Locator(Padded_String_View input);

  CLI_Source_Range range(Source_Code_Span) const;
  CLI_Source_Position position(const Char8*) const;

 private:
  void cache_offsets_of_lines() const;

  CLI_Source_Position::Line_Number_Type find_line_at_offset(
      CLI_Source_Position::Offset_Type offset) const;

  CLI_Source_Position::Offset_Type offset(const Char8*) const;

  CLI_Source_Position position(
      CLI_Source_Position::Line_Number_Type line_number,
      CLI_Source_Position::Offset_Type offset) const;

  Padded_String_View input_;
  mutable std::vector<CLI_Source_Position::Offset_Type> offset_of_lines_;
};
}

#endif

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
