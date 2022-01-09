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

#include <cstddef>
#include <cstdint>
#include <quick-lint-js-sublime-text-interface.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

namespace quick_lint_js {
namespace sublime_text {

//==============================================================================
//------------------------------------------------------------------------------
// offset

using offset = unsigned int;

//==============================================================================
//------------------------------------------------------------------------------
// position

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
struct position final : public qljs_sublime_text_position {
public:
  friend inline bool operator==(const position &lhs, const position &rhs) noexcept {
    return lhs.line == rhs.line && lhs.character == rhs.character;
  }

  friend inline bool operator!=(const position &lhs, const position &rhs) noexcept {
    return !(lhs == rhs);
  }

  friend inline std::ostream &operator<<(std::ostream &stream, const position &pos) {
    stream << "line " << pos.line << " character " << pos.character;
    return stream;
  }
};
#else
using position = offset;
#endif

//==============================================================================
//------------------------------------------------------------------------------
// range

struct range final : public qljs_sublime_text_range {};

//==============================================================================
//------------------------------------------------------------------------------
// character

struct character {
public:
  static bool is_ascii(const char8 ch) { return static_cast<std::uint8_t>(ch) <= 127; }

  static bool is_newline(const char8 ch) { return ch == u8'\n' || ch == u8'\r'; }

  static bool is_wide_newline(const char8 *ch) {
    return is_newline(ch[0]) && is_newline(ch[1]);
  }

  static bool is_microsoft_newline(const char8 *ch) {
    return ch[0] == u8'\r' && ch[1] == u8'\n';
  }
};

//==============================================================================
//------------------------------------------------------------------------------
// lines

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
struct lines {
public:
  void compute(const char8 *begin, const char8 *end, const char8 *input);

  std::vector<std::uint8_t> is_ascii_;
  std::vector<offset> offset_begin_;
};
#endif

//==============================================================================
//------------------------------------------------------------------------------
// locator

struct locator {
public:
  using range_type = range;
  using position_type = position;
  using offset_type = offset;

  explicit locator(padded_string_view input) noexcept;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  void replace_text(range_type range,
                    string8_view replacement_text,
                    padded_string_view new_input);

  const char8 *from_position(position_type position) const noexcept;
#endif

  range_type range(source_code_span span) const;

  position_type position(const char8 *ch) const noexcept;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  position_type position(int line_number, offset_type offset) const noexcept;

  offset_type offset(const char8 *) const noexcept;
#endif

private:
  padded_string_view input_;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  lines new_lines;
  // old_lines are used for double buffering of new_lines.
  // This reduces allocations.
  lines old_lines;
  offset_type find_line_at_offset(offset_type offset) const;
#endif
};

} // namespace sublime_text
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
