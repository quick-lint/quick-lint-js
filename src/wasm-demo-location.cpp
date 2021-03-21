// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-8.h>
#include <quick-lint-js/wasm-demo-location.h>

namespace quick_lint_js {
wasm_demo_locator::wasm_demo_locator(padded_string_view input) noexcept
    : input_(input) {}

wasm_demo_source_range wasm_demo_locator::range(source_code_span span) const {
  return wasm_demo_source_range{
      .begin = this->position(span.begin()),
      .end = this->position(span.end()),
  };
}

wasm_demo_source_offset wasm_demo_locator::position(const char8* c) const
    noexcept {
  int byte_offset = narrow_cast<int>(c - this->input_.data());
  return narrow_cast<wasm_demo_source_offset>(
      count_lsp_characters_in_utf_8(this->input_, byte_offset));
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
