// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_UTF_8_H
#define QUICK_LINT_JS_UTIL_UTF_8_H

#include <cstddef>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
Char8* encode_utf_8(char32_t code_point, Char8* out);

// There are three cases with Decode_UTF8_Result:
//
// 1. The input string is empty. In this case, .size == 0 && .ok == false.
//    .code_point is unspecified.
//
// 2. The input string starts with a valid character sequence.
//    .ok == true && .size > 0. .code_point refers to the first Unicode code
//    point in the input. .size is the number of Char8-s in the first character
//    sequence.
//
// 3. The input string starts with an invalid character sequence.
//    .ok == false && .size > 0. .code_point is unspecified. .size is the number
//    of Char8-s you should skip.
struct Decode_UTF8_Result {
  // Invariant: !(this->ok && this->size == 0)
  std::ptrdiff_t size;
  // Valid only if this->ok == true.
  char32_t code_point;
  bool ok;
};

Decode_UTF8_Result decode_utf_8(Padded_String_View) noexcept;
std::size_t count_utf_8_characters(Padded_String_View, std::size_t) noexcept;

const Char8* advance_lsp_characters_in_utf_8(String8_View,
                                             int character_count) noexcept;
std::ptrdiff_t count_lsp_characters_in_utf_8(Padded_String_View,
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
