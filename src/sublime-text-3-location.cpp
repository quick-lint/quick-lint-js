// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/sublime-text-3-location.h>
#include <quick-lint-js/utf-8.h>

namespace quick_lint_js {
sublime_text_3_locator::sublime_text_3_locator(
    padded_string_view input) noexcept
    : input_(input) {}

sublime_text_3_source_range sublime_text_3_locator::range(
    source_code_span span) const {
  return sublime_text_3_source_range{
      .begin = this->position(span.begin()),
      .end = this->position(span.end()),
  };
}

sublime_text_3_source_offset sublime_text_3_locator::position(
    const char8* c) const noexcept {
  std::size_t byte_offset = narrow_cast<std::size_t>(c - this->input_.data());
  return narrow_cast<sublime_text_3_source_offset>(
      count_utf_8_characters(this->input_, byte_offset));
}
}  // namespace quick_lint_js

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
