// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CLI_LOCATION_H
#define QUICK_LINT_JS_CLI_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
struct cli_source_position {
  using line_number_type = int;
  using offset_type = std::size_t;

  line_number_type line_number;
  int column_number;
  offset_type offset;

  bool operator==(const cli_source_position& other) const noexcept {
    return this->line_number == other.line_number &&
           this->column_number == other.column_number &&
           this->offset == other.offset;
  }

  bool operator!=(const cli_source_position& other) const noexcept {
    return !(*this == other);
  }
};

std::ostream& operator<<(std::ostream&, const cli_source_position&);

class cli_source_range {
 public:
  using offset = cli_source_position::offset_type;

  explicit cli_source_range(cli_source_position begin,
                            cli_source_position end) noexcept
      : begin_(begin), end_(end) {}

  offset begin_offset() const noexcept { return this->begin_.offset; }
  cli_source_position begin() const noexcept;

  offset end_offset() const noexcept { return this->end_.offset; }
  cli_source_position end() const noexcept;

 private:
  cli_source_position begin_;
  cli_source_position end_;
};

class cli_locator {
 public:
  explicit cli_locator(padded_string_view input) noexcept;

  cli_source_range range(source_code_span) const;
  cli_source_position position(const char8*) const noexcept;

 private:
  void cache_offsets_of_lines() const;

  cli_source_position::line_number_type find_line_at_offset(
      cli_source_position::offset_type offset) const;

  cli_source_position::offset_type offset(const char8*) const noexcept;

  cli_source_position position(
      cli_source_position::line_number_type line_number,
      cli_source_position::offset_type offset) const noexcept;

  padded_string_view input_;
  mutable std::vector<cli_source_position::offset_type> offset_of_lines_;
};
}

#endif

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
