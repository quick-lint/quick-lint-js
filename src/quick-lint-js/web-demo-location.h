// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_WEB_DEMO_LOCATION_H
#define QUICK_LINT_JS_WEB_DEMO_LOCATION_H

#include <cstdint>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
class Source_Code_Span;

using Web_Demo_Source_Offset = std::uint32_t;

struct Web_Demo_Source_Range {
  Web_Demo_Source_Offset begin;
  Web_Demo_Source_Offset end;
};

class Web_Demo_Locator {
 public:
  using Range_Type = Web_Demo_Source_Range;

  explicit Web_Demo_Locator(Padded_String_View input) noexcept;

  Web_Demo_Source_Range range(Source_Code_Span) const;
  Web_Demo_Source_Offset position(const Char8*) const noexcept;

 private:
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
