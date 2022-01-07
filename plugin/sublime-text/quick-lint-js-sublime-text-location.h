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

struct sublime_text_range final : public qljs_st_range {};

//==============================================================================
//------------------------------------------------------------------------------
// lines

struct lines {
public:
  using offset_type = qljs_st_offset;

  void compute_information() {
    std::uint8_t flags = 0;
    auto is_line_ascii = [&flags]() -> bool { return (flags & 0x80) == 0; };
    auto add_end_of_line = [&]() -> void {
      this->line_is_ascii_.push_back(is_line_ascii());
      flags = 0;
    };

    for (const char8 *c = begin; c != end;) {
      if (is_ascii) {}
      flags |= static_cast<std::uint8_t>(*c);
      if (is_line_end(*c)) {
        c = is_microsoft_newline(c) ? 2 : 1;
        add_is_ascii();
        add_offset_beginning(c, input_beginning);
        flag = 0;
      } else {
        c += 1;
      }
    }
    return is_line_ascii();
  }

  std::vector<offset_type> offset_beginning_;
  std::vector<std::uint8_t> is_ascii_;

private:
  void add_offset_beginning(const char8 *line_beginning, const char8 *input_beginning) {
    offset_type offset = narrow_cast<offset_type>(line_beginning - input_beginning);
    this->offset_beginning_.push_back(offset);
  }

  void add_is_ascii() {
  }

  void is_newline(const char8 c) {
    return c == u8'\n' || c == u8'\r';
  }

  void is_wide_newline(const char8* c) {
    return is_newline(c[0]) && is_newline(c[1]);
  }

  void is_microsoft_newline(const char8* c) {
    return c[0] == u8'\r' && c[1] == u8'\n';
  }

  void is_character_ascii(const char8 c) {
    return static_cast<std::uint8_t>(c) > 127;
  }
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
