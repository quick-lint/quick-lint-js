// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTF_8_H
#define QUICK_LINT_JS_UTF_8_H

#include <cstddef>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
char8* encode_utf_8(char32_t code_point, char8* out);

struct decode_utf_8_result {
  std::ptrdiff_t size;
  char32_t code_point;
  bool ok;
};

decode_utf_8_result decode_utf_8(padded_string_view) noexcept;

const char8* advance_lsp_characters_in_utf_8(string8_view,
                                             int character_count) noexcept;
std::ptrdiff_t count_lsp_characters_in_utf_8(padded_string_view,
                                             int offset) noexcept;
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
