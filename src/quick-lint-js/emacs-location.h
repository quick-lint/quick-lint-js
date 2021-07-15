// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_EMACS_LOCATION_H
#define QUICK_LINT_JS_EMACS_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
struct emacs_source_position {
  using offset_type = std::size_t;

  offset_type offset;

  bool operator==(const emacs_source_position& other) const noexcept {
    return this->offset == other.offset;
  }

  bool operator!=(const emacs_source_position& other) const noexcept {
    return !(*this == other);
  }
};

std::ostream& operator<<(std::ostream&, const emacs_source_position&);

class emacs_source_range {
 public:
  using offset = emacs_source_position::offset_type;

  explicit emacs_source_range(emacs_source_position begin,
                              emacs_source_position end) noexcept
      : begin_(begin), end_(end) {}

  offset begin_offset() const noexcept { return this->begin_.offset; }
  emacs_source_position begin() const noexcept;

  offset end_offset() const noexcept { return this->end_.offset; }
  emacs_source_position end() const noexcept;

 private:
  emacs_source_position begin_;
  emacs_source_position end_;
};

class emacs_locator {
 public:
  explicit emacs_locator(padded_string_view input) noexcept;

  emacs_source_range range(source_code_span) const;
  emacs_source_position position(const char8*) const noexcept;

 private:
  emacs_source_position::offset_type offset(const char8*) const noexcept;
  emacs_source_position position(
      emacs_source_position::offset_type offset) const noexcept;
  padded_string_view input_;
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
