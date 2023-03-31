// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_SOURCE_CODE_SPAN_H
#define QUICK_LINT_JS_FE_SOURCE_CODE_SPAN_H

#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
class source_code_span {
 public:
  // A source_code_span with no contained characters.
  static source_code_span unit(const char8* c) noexcept {
    return source_code_span(c, c);
  }

  explicit source_code_span(const char8* begin, const char8* end) noexcept
      : begin_(begin), end_(end) {}

  const char8* begin() const noexcept { return this->begin_; }

  const char8* end() const noexcept { return this->end_; }

  string8_view string_view() const noexcept {
    return make_string_view(this->begin(), this->end());
  }

  int size() const noexcept {
    return narrow_cast<int>(this->end() - this->begin());
  }

 private:
  const char8* begin_;
  const char8* end_;
};

// Returns true of the given source_code_span-s refer to the same span of code
// (i.e. are completely identical).
bool same_pointers(source_code_span, source_code_span) noexcept;
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
