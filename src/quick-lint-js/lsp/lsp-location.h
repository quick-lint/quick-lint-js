// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_LSP_LOCATION_H
#define QUICK_LINT_JS_LSP_LSP_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
struct LSP_Position {
  int line;
  int character;

  friend bool operator==(const LSP_Position &, const LSP_Position &);
  friend bool operator!=(const LSP_Position &, const LSP_Position &);

  friend std::ostream &operator<<(std::ostream &, const LSP_Position &);
};

struct LSP_Range {
  LSP_Position start;
  LSP_Position end;
};

class LSP_Locator {
 private:
  using Offset_Type = int;

 public:
  using Range_Type = LSP_Range;

  explicit LSP_Locator(Padded_String_View input);

  LSP_Range range(Source_Code_Span) const;
  LSP_Position position(const Char8 *) const;

  const Char8 *from_position(LSP_Position) const;

  void replace_text(LSP_Range, String8_View replacement_text,
                    Padded_String_View new_input);

  void validate_caches_debug() const;

 private:
  void cache_offsets_of_lines();
  void compute_offsets_of_lines(const Char8 *begin, const Char8 *end,
                                bool *out_last_line_is_ascii);

  int find_line_at_offset(Offset_Type offset) const;

  Offset_Type offset(const Char8 *) const;

  LSP_Position position(int line_number, Offset_Type offset) const;

  Padded_String_View input_;
  std::vector<Offset_Type> offset_of_lines_;
  std::vector<unsigned char> line_is_ascii_;

  // old_offset_of_lines_ and old_line_is_ascii_ are used for double buffering
  // of offset_of_lines_ and line_is_ascii_. This reduces allocations.
  std::vector<Offset_Type> old_offset_of_lines_;
  std::vector<unsigned char> old_line_is_ascii_;
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
