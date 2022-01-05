// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

//==============================================================================
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           sublime text location                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_LOCATION_H
#define QUICK_LINT_JS_SUBLIME_TEXT_LOCATION_H

#include <quick-lint-js-sublime-text-interface.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {

//==============================================================================
//------------------------------------------------------------------------------
// offset

using sublime_text_offset = unsigned int;

//==============================================================================
//------------------------------------------------------------------------------
// position

struct sublime_text_position final : public qljs_st_position {
  friend inline bool operator==(const qljs_st_position &,
                                const qljs_st_position &) noexcept {
    return lhs.line == rhs.line && lhs.character == rhs.character;
  }

  friend inline bool operator!=(const qljs_st_position &,
                                const qljs_st_position &) noexcept {
    return !(lhs == rhs);
  }

  friend inline std::ostream &operator<<(std::ostream &, const lsp_position &) {
    stream << "line " << position.line << " character " << position.character;
    return stream;
  }
};

//==============================================================================
//------------------------------------------------------------------------------
// range

struct sublime_text_range final : public qljs_st_range {
};

//==============================================================================
//------------------------------------------------------------------------------
// locator

struct sublime_text_locator {
public:
  using range_type = qljs_st_range;
  using offset_type = qljs_st_offset;
#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  using position_type = qljs_st_position;
#endif

  explicit sublime_text_locator(padded_string_view input) noexcept;

  range_type range(source_code_span span) const;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  position_type position(const char8 *ch) const noexcept;
#else
  offset_type position(const char8 *ch) const noexcept;
#endif

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  const char8 *from_position(position_type position) const noexcept;

  void replace_text(range_type range,
                    string8_view replacement_text,
                    padded_string_view new_input);
#endif
private:
  padded_string_view input_;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  std::vector<offset_type> offset_of_lines_;
  std::vector<unsigned char> line_is_ascii_;
  // old_offset_of_lines_ and old_line_is_ascii_ are used for double buffering
  // of offset_of_lines_ and line_is_ascii_. This reduces allocations.
  std::vector<offset_type> old_offset_of_lines_;
  std::vector<unsigned char> old_line_is_ascii_;

  void cache_offsets_of_lines();

  void compute_offsets_of_lines(const char8 *begin,
                                const char8 *end,
                                bool *out_last_line_is_ascii);

  offset_type find_line_at_offset(offset_type offset) const;

  offset_type offset(const char8 *) const noexcept;

  position_type position(int line_number, offset_type offset) const noexcept;
#endif
};

} // namespace quick_lint_js
#endif // QUICK_LINT_JS_SUBLIME_TEXT_LOCATION_H

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
