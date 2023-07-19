// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CLI_EMACS_LOCATION_H
#define QUICK_LINT_JS_CLI_EMACS_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <vector>

namespace quick_lint_js {
struct Emacs_Source_Position {
  using Offset_Type = std::size_t;

  Offset_Type offset;

  bool operator==(const Emacs_Source_Position& other) const noexcept {
    return this->offset == other.offset;
  }

  bool operator!=(const Emacs_Source_Position& other) const noexcept {
    return !(*this == other);
  }
};

std::ostream& operator<<(std::ostream&, const Emacs_Source_Position&);

class Emacs_Source_Range {
 public:
  using Offset = Emacs_Source_Position::Offset_Type;

  explicit Emacs_Source_Range(Emacs_Source_Position begin,
                              Emacs_Source_Position end) noexcept
      : begin_(begin), end_(end) {}

  Offset begin_offset() const noexcept { return this->begin_.offset; }
  Emacs_Source_Position begin() const noexcept;

  Offset end_offset() const noexcept { return this->end_.offset; }
  Emacs_Source_Position end() const noexcept;

 private:
  Emacs_Source_Position begin_;
  Emacs_Source_Position end_;
};

class Emacs_Locator {
 public:
  explicit Emacs_Locator(Padded_String_View input) noexcept;

  Emacs_Source_Range range(Source_Code_Span) const;
  Emacs_Source_Position position(const Char8*) const noexcept;

 private:
  Emacs_Source_Position::Offset_Type offset(const Char8*) const noexcept;
  Emacs_Source_Position position(
      Emacs_Source_Position::Offset_Type offset) const noexcept;
  Padded_String_View input_;
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
