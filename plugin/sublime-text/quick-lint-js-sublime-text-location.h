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

struct position final : public qljs_st_position {
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

//==============================================================================
//------------------------------------------------------------------------------
// range

struct range final : public qljs_st_range {};

//==============================================================================
//------------------------------------------------------------------------------
// character

struct character {
public:
  static bool is_ascii(const char8 ch) { return static_cast<std::uint8_t>(ch) > 127; }

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

struct lines {
public:
  bool compute_information(const char8 *input_begin, const char8 *input_end) {
    auto on_line_begin = [this] () {
      this->offset_begin_.push_back(this->calculate_offset_begin());
    };
    auto on_line_end = [this] { this->offset_beginning_.push_back((flags & 127) == 0); }

    const char8 *ch = begin;
    ascii_calculator ac();

    on_line_begin();
    while (ch != end) {
      ac.input(*ch);
      if (is_newline(*ch)) {
        on_line_end();
        ch += characters::is_microsoft_newline(ch) ? 2 : 1;
        on_line_begin();
      } else {
        ch += 1;
      }
    }
    return;
  }

  std::vector<std::uint8_t> is_ascii_;
  std::vector<offset> offset_begin_;

private:
  struct is_ascii_calculator {
  public:
    ascii_calculator() = default;
    start() { flags = 0; }
    restart() { flags = 0; }
    input(const char8 ch) { flags |= static_cast<std::uint8_t>(ch); }
    output() { return (flags & 127) == 0; }

  private:
    std::uint8_t flags = 0;
  };

  static offset calculate_offset_begin(const char8 *foo, const char8 *input) {
    return foo - input;
  }
};

//==============================================================================
//------------------------------------------------------------------------------
// locator

struct locator {
public:
  /*
    using range_type = qljs_st_range;
    using offset_type = qljs_st_offset;
  #if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
    using position_type = qljs_st_position;
  #endif
  */

  explicit locator(padded_string_view input) noexcept;

  range_type range(source_code_span span) const;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  void replace_text(range_type range,
                    string8_view replacement_text,
                    padded_string_view new_input);

  position_type position(const char8 *ch) const noexcept;

  const char8 *from_position(position_type position) const noexcept;
#else
  offset_type position(const char8 *ch) const noexcept;
#endif
private:
  padded_string_view input_;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  // offset_of_lines_ the vector index is the line, and the value of index is
  // the beginning offset of the line.
  std::vector<offset_type> beginning_offset_of_lines_;
  // line_is_ascii_ vector index is the line, and the value of index is true
  // if all characters in line is ascii otherwise false.
  // unsigned char to avoid performance traps for vector<bool> template
  // specialization.
  std::vector<unsigned char> line_is_ascii_;

  // old_offset_of_lines_ and old_line_is_ascii_ are used for double buffering
  // of offset_of_lines_ and line_is_ascii_. This reduces allocations.
  //
  std::vector<offset_type> old_offset_of_lines_;
  std::vector<unsigned char> old_line_is_ascii_;

  lines new_lines;

  // old_lines are used for double buffering of new_lines.
  // This reduces allocations.
  lines old_lines;

  position_type position(int line_number, offset_type offset) const noexcept;

  offset_type offset(const char8 *) const noexcept;

  offset_type find_line_at_offset(offset_type offset) const;

  bool compute_offsets_of_lines(const char8 *begin, const char8 *end);

  void cache_offsets_of_lines();
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
