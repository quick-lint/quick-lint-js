// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VIM_LOCATION_H
#define QUICK_LINT_JS_VIM_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
class source_code_span;

struct vim_source_position {
  int lnum;
  int col;

  bool operator==(const vim_source_position& other) const noexcept {
    return this->lnum == other.lnum && this->col == other.col;
  }

  bool operator!=(const vim_source_position& other) const noexcept {
    return !(*this == other);
  }
};

std::ostream& operator<<(std::ostream&, const vim_source_position&);

struct vim_source_range {
  vim_source_position begin;
  vim_source_position end;
};

class vim_locator {
 public:
  explicit vim_locator(padded_string_view input) noexcept;

  vim_source_range range(source_code_span) const;
  vim_source_position position(const char8*) const noexcept;

 private:
  using offset_type = int;

  void cache_offsets_of_lines() const;

  int find_line_at_offset(offset_type offset) const;

  offset_type offset(const char8*) const noexcept;

  vim_source_position position(int line_number,
                               offset_type offset) const noexcept;

  padded_string_view input_;
  mutable std::vector<offset_type> offset_of_lines_;
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
