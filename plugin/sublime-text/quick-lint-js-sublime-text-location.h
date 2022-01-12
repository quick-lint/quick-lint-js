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

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <quick-lint-js-sublime-text-interface.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <vector>

// TODO: Add constructors range from locator to constructor from range
// TODO: Create region structure
// TODO: Move back_insert_transform to vector.h

namespace quick_lint_js {
namespace sublime_text {

//==============================================================================
//------------------------------------------------------------------------------
// offset

using offset = unsigned int;

//==============================================================================
//------------------------------------------------------------------------------
// region

struct region final : public qljs_sublime_text_region {
}
//==============================================================================
//------------------------------------------------------------------------------
// position

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
struct position final : public qljs_sublime_text_position {
 public:
  friend inline bool operator==(const position &lhs,
                                const position &rhs) noexcept {
    return lhs.line == rhs.line && lhs.character == rhs.character;
  }

  friend inline bool operator!=(const position &lhs,
                                const position &rhs) noexcept {
    return !(lhs == rhs);
  }

  friend inline std::ostream &operator<<(std::ostream &stream,
                                         const position &pos) {
    stream << "line " << pos.line << " character " << pos.character;
    return stream;
  }
};
#endif

//==============================================================================
//------------------------------------------------------------------------------
// range

struct range final : public qljs_sublime_text_range {};

//==============================================================================
//------------------------------------------------------------------------------
// characters

struct characters {
 public:
  static bool is_newline(const char8 ch) {
    return ch == u8'\n' || ch == u8'\r';
  }

  static bool is_wide_newline(const char8 *ch) {
    return is_newline(ch[0]) && is_newline(ch[1]);
  }

  static bool is_microsoft_newline(const char8 *ch) {
    return ch[0] == u8'\r' && ch[1] == u8'\n';
  }

  static bool is_ascii(const char8 ch) {
    return static_cast<std::uint8_t>(ch) <= 127;
  }
};

//==============================================================================
//------------------------------------------------------------------------------
// lines

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
struct lines {
 public:
  using offset_type = offset;
  using size_type = std::size_t;

  std::vector<offset_type> offset_begin_;
  std::vector<std::uint8_t> is_ascii_;

  void extend(lines &other, offset_type begin, offset_type end) {
    this->offset_begin_.insert(this->offset_begin_.end(),
                               other->offset_begin_.begin() + begin,
                               other->offset_begin_.begin() + end);
    this->is_ascii_.insert(this->is_ascii_.end(),
                           other->is_ascii_.begin() + begin,
                           other->is_ascii_.begin() + end);
  }

  void compute(const char8 *input, const char8 *begin, const char8 *end);

  void compute(padded_string_view input, offset_type begin, offset_type end) {
    this->compute(&input, &input[begin], &input[end]);
  }

  void swap(lines &other) {
    std::swap(this->offset_begin_, other->offset_begin_);
    std::swap(this->is_ascii_, other->is_ascii_);
  }

  void reserve(lines &other) {
    this->offset_begin_.reserve(other->offset_begin_.size());
    this->is_ascii_.reserve(other->offset_begin_.size());
  }

  void clear() {
    this->offset_begin_.clear();
    this->is_ascii_.clear();
  }

  size_type size() {
    QLJS_ASSERT(this->offset_begin_.size() == this->is_ascii_.size());
    return this->offset_begin_.size();
  }

  offset_type find_line(offset_type offset) {
    QLJS_ASSERT(!this->offset_begin_.empty());
    auto begin = this->offset_begin_.begin();
    auto end = this->offset_begin_.end();
    auto line_it = std::upper_bound(begin + 1, end, offset) - 1;
    auto line = line_it - this->offset_begin_.begin();
    return narrow_cast<offset_type>(line);
  }

  bool is_last_line(offset_type line) { return line == this->size() - 1; }
};
#endif

//==============================================================================
//------------------------------------------------------------------------------
// locator

struct locator {
 public:
  using range_type = range;
  using position_type = position;
  using region_type = region;
  using offset_type = offset;

  /*
    using replacement_type = string8_view;
    using input_type = padded_string_view;
    using span_type = source_code_span;
  */

  explicit locator(padded_string_view input) noexcept;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  void replace_text(range_type range, replacement_type replacement_text,
                    input_type new_input);

  const char8 *from_position(position_type position) const noexcept;
#endif

  range_type range(source_code_span span) const;

  position_type position(const char8 *ch) const noexcept;

  /*TODO: region()*/

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  position_type position(int line_number, offset_type offset) const noexcept;

  offset_type offset(const char8 *) const noexcept;

  // TODO: Implement this declaration
  // HINT: this->from_position inside implementation
  offset_type offset(position_type *) const noexcept;
#endif

 private:
  padded_string_view input_;

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
  lines new_lines;
  // old_lines are used for double buffering of new_lines.
  // This reduces allocations.
  lines old_lines;
#endif
};

}  // namespace sublime_text
}  // namespace quick_lint_js
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
