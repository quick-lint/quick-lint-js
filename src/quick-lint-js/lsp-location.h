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

#ifndef QUICK_LINT_JS_LSP_LOCATION_H
#define QUICK_LINT_JS_LSP_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
struct lsp_position {
  int line;
  int character;

  friend bool operator==(const lsp_position &, const lsp_position &) noexcept;
  friend bool operator!=(const lsp_position &, const lsp_position &) noexcept;

  friend std::ostream &operator<<(std::ostream &, const lsp_position &);
};

struct lsp_range {
  lsp_position start;
  lsp_position end;
};

class lsp_locator {
 private:
  using offset_type = int;

 public:
  explicit lsp_locator(padded_string_view input) noexcept;

  lsp_range range(source_code_span) const;
  lsp_position position(const char8 *) const noexcept;

  char8 *from_position(lsp_position) const noexcept;

 private:
  void cache_offsets_of_lines();

  int find_line_at_offset(offset_type offset) const;

  offset_type offset(const char8 *) const noexcept;

  lsp_position position(int line_number, offset_type offset) const noexcept;

  padded_string_view input_;
  std::vector<offset_type> offset_of_lines_;
};
}

#endif
