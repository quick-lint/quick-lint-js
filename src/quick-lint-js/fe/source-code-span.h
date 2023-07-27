// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_SOURCE_CODE_SPAN_H
#define QUICK_LINT_JS_FE_SOURCE_CODE_SPAN_H

#include <iosfwd>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
class Source_Code_Span {
 public:
  // A Source_Code_Span with no contained characters.
  static Source_Code_Span unit(const Char8* c) {
    return Source_Code_Span(c, c);
  }

  explicit Source_Code_Span(const Char8* begin, const Char8* end)
      : begin_(begin), end_(end) {}

  const Char8* begin() const { return this->begin_; }

  const Char8* end() const { return this->end_; }

  String8_View string_view() const {
    return make_string_view(this->begin(), this->end());
  }

  int size() const { return narrow_cast<int>(this->end() - this->begin()); }

 private:
  const Char8* begin_;
  const Char8* end_;
};

// Returns true of the given Source_Code_Span-s refer to the same span of code
// (i.e. are completely identical).
bool same_pointers(Source_Code_Span, Source_Code_Span);

std::ostream& operator<<(std::ostream&, Source_Code_Span);
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
