// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_LOCATION_H
#define QUICK_LINT_JS_SUBLIME_TEXT_LOCATION_H

#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>


#if QLJS_ST_PLUGIN_VERSION == 3

struct qljs_st_locator {
 public:
  using range_type = qljs_st_range;
  using offset_type = qljs_st_offset;

  explicit qljs_st_locator(qljs::padded_string_view input) noexcept;

  range_type range(qljs::source_code_span span) const;
  offset_type position(const qljs::char8* ch) const noexcept;

 private:
  qljs::padded_string_view input_;
};
#else
#include <quick-lint-js/lsp-location.h>


struct qljs_st_locator final : public qljs::lsp_locator {
 public:
  using range_type = qljs_st_range;
  using offset_type = qljs_st_offset;
  using position_type = qljs_st_position;

  explicit qljs_st_locator(qljs::padded_string_view input) noexcept;

  range_type range(qljs::source_code_span span) const;

  position_type position(const qljs::char8 *source) const noexcept;

  const qljs::char8 *from_position(position_type position) const noexcept;

  void replace_text(range_type range, qljs::string8_view replacement_text,
                    qljs::padded_string_view new_input);
};
#endif

#if defined(__cplusplus)
}
#endif

#endif  // QUICK_LINT_JS_SUBLIME_TEXT_LOCATION_H

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
