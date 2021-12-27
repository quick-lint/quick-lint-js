// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js-sublime-text-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-8.h>

namespace qljs = quick_lint_js;

#if QLJS_ST_PLUGIN_VERSION == 3
qljs_st_locator::qljs_st_locator(qljs::padded_string_view input) noexcept
    : input_(input) {}

qljs_st_range qljs_st_locator::range(qljs::source_code_span span) const {
  auto begin = this->position(span.begin());
  auto end = this->position(span.end());
  return qljs_st_range{.begin = begin, .end = end};
}

qljs_st_offset qljs_st_locator::position(const qljs::char8* ch) const noexcept {
  auto byte_offset = qljs::narrow_cast<std::size_t>(ch - this->input_.data());
  std::size_t count = qljs::count_utf_8_characters(this->input_, byte_offset);
  return qljs::narrow_cast<qljs_st_offset>(count);
}
#else
qljs_st_locator::qljs_st_locator(qljs::padded_string_view input) noexcept {
  return lsp_locator(input);
}

qljs_st_locator::range_type qljs_st_locator::range(
    qljs::source_code_span span) const {
  return narrow_cast<qljs_st_locator::range_type>(lsp_locator::range(span));
}

qljs_st_locator::position_type qljs_st_locator::position(
    const qljs::char8 *source) const noexcept {
  auto lposition = lsp_locator::position(span);
  return narrow_cast<qljs_st_locator::position_type>(lposition);
}

const char8 *from_position(position_type position) const noexcept {
  auto lposition = narrow_cast<lsp_locator::position_type>(position);
  return lsp_locator::from_position(lposition);
}

void replace_text(range_type range, qljs::string8_view replacement_text,
                  qljs::padded_string_view new_input) {
  auto lrange = narrow_cast<lsp_locator::range_type>(lsp_locator::range(span));
  lsp_locator::replace_text(lrange, replacement_text, new_input)
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
