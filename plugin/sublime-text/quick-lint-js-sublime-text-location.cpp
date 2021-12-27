// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js-sublime-text-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-8.h>

#if QLJS_ST_PLUGIN_VERSION == 3
qljs_st_locator::qljs_st_locator(padded_string_view input) noexcept
    : input_(input) {}

qljs_st_range qljs_st_locator::range(source_code_span span) const {
  auto begin = this->position(span.begin());
  auto end = this->position(span.end());
  return qljs_st_range{.begin = begin, .end = end};
}

qljs_st_offset qljs_st_locator::position(const char8* c) const noexcept {
  std::size_t byte_offset = narrow_cast<std::size_t>(c - this->input_.data());
  std::size_t count = count_utf_8_characters(this->input_, byte_offset);
  return narrow_cast<qljs_st_offset>(count);
}
#else
qljs_st_locator::qljs_st_locator(padded_string_view input) noexcept {
  quick_lint_js::lsp_locator(input)
}
#endif

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
