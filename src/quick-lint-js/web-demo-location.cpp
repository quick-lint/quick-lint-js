// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/utf-8.h>
#include <quick-lint-js/web-demo-location.h>

namespace quick_lint_js {
Web_Demo_Locator::Web_Demo_Locator(Padded_String_View input) : input_(input) {}

Web_Demo_Source_Range Web_Demo_Locator::range(Source_Code_Span span) const {
  return Web_Demo_Source_Range{
      .begin = this->position(span.begin()),
      .end = this->position(span.end()),
  };
}

Web_Demo_Source_Offset Web_Demo_Locator::position(const Char8* c) const {
  int byte_offset = narrow_cast<int>(c - this->input_.data());
  return narrow_cast<Web_Demo_Source_Offset>(
      count_lsp_characters_in_utf_8(this->input_, byte_offset));
}
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
