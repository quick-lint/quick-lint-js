// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
class Source_Code_Span;

struct Vim_Source_Position {
  int lnum;
  int col;

  bool operator==(const Vim_Source_Position& other) const {
    return this->lnum == other.lnum && this->col == other.col;
  }

  bool operator!=(const Vim_Source_Position& other) const {
    return !(*this == other);
  }
};

std::ostream& operator<<(std::ostream&, const Vim_Source_Position&);

struct Vim_Source_Range {
  Vim_Source_Position begin;
  Vim_Source_Position end;
};

class Vim_Locator {
 public:
  explicit Vim_Locator(Padded_String_View input);

  Vim_Source_Range range(Source_Code_Span) const;
  Vim_Source_Position position(const Char8*) const;

 private:
  using Offset_Type = int;

  void cache_offsets_of_lines() const;

  int find_line_at_offset(Offset_Type offset) const;

  Offset_Type offset(const Char8*) const;

  Vim_Source_Position position(int line_number, Offset_Type offset) const;

  Padded_String_View input_;
  mutable std::vector<Offset_Type> offset_of_lines_;
};
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
