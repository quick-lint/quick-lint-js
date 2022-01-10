// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

//==============================================================================
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           sublime text location                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#include <algorithm>
#include <quick-lint-js-sublime-text-location.h>
#include <quick-lint-js-sublime-text-utils.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-8.h>

namespace quick_lint_js {
namespace sublime_text {

//==============================================================================
//------------------------------------------------------------------------------
// lines

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
void lines::compute(const char8 *input, const char8 *begin, const char8 *end) {
  std::uint8_t flags = 0;
  auto on_line_begin = [this](const char8 *line_begin) {
    this->offset_begin_.push_back(line_begin - input);
  };
  auto on_character = [flags](const char8 *character) {
    flags |= static_cast<std::uint8_t>(*character);
  };
  auto on_line_end = [flags, this](const char8 * /*line_end*/) {
    this->is_ascii_.push_back(characters::is_ascii(static_cast<char8>(flags)));
  };

  const char8 *ch = begin + 1;
  on_line_begin(ch);
  while (ch != end) {
    on_character(ch);
    if (characters::is_newline(*ch)) {
      on_line_end(ch);
      ch += characters::is_microsoft_newline(ch) ? 2 : 1;
      on_line_begin(ch);
    } else {
      ch += 1;
    }
  }
  on_line_end(ch);
}
#endif

//==============================================================================
//------------------------------------------------------------------------------
// locator

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
locator::locator(padded_string_view input) noexcept : input_(input) {
  this->new_lines.compute(this->input_, 0, this->input_.size());
}

// NOTE: should range be a reference? `&`
void locator::replace_text(range_type range, string8_view replacement,
                           padded_string_view new_input) {
  QLJS_ASSERT(!this->new_lines.offset_begin_.empty());

  region_type region = this->region(range);

  this->input_ = new_input;
  this->new_lines.swap(&this->old_lines);
  this->new_lines.reserve(&this->old_lines);
  this->new_lines.clear();

  // Before replacement: do not adjust.
  this->new_lines.extend(&this->old_lines, 0, range.start.line);

  // Within replacement: re-parse newlines.
  this->new_lines.compute(this->input_, region.begin, region.end);
  {
    if (this->new_lines.is_ascii_.size() > range.start.line) {
      this->line_is_ascii_[range.start.line] =
          this->new_lines.is_ascii_[range.start.line] &&
          this->old_lines.is_ascii_[range.start.line];
    }
    this->new_lines.is_ascii_[range.end.line] =
        this->new_lines.is_ascii_[range.end.line] &&
        this->old_lines.is_ascii_[range.end.line];
  }

  // After replacement: adjust with a fixed offset.
  auto after_replacement = region.end + 1;
  {
    auto adjust_offset = [](offset_type offset) {
      static int adjust = replacement.size() - (region.end - region.begin);
      return offset + adjust;
    };
    insert_back_transform(
        this->old_lines.offset_begin_.begin() + after_replacement,
        this->old_lines.offset_begin_.end(), this->new_lines, adjust_offset);
  }
  {
    this->new_lines.is_ascii_.insert(this->new_lines.is_ascii_.end(),
                                     this->old_lines.is_ascii_.begin() +
                                         after_replacement,
                                     this->old_lines->is_ascii_.end());
  }

  QLJS_ASSERT(std::is_sorted(this->new_lines.offset_begin_.begin(),
                             this->new_lines.offset_begin_.end()));
  QLJS_ASSERT(this->new_lines.offset_begin_.size() ==
              this->new_lines.is_ascii_.size());
}

const char8 *locator::from_position(position_type position) const noexcept {
  auto is_last_line =
      [](offset_type line, offset_type number_of_lines) {
        return line == number_of_lines - 1;
      }

  auto current_line = position.line;
  auto current_character = position.character;
  auto number_of_lines = this->offset_of_lines_.size();
  if (line >= number_of_lines) {
    return this->input_.null_terminator();
  }

  auto current_line_begin_offset = this->offset_of_lines_[line];
  bool current_line_is_ascii = this->line_is_ascii_[line];
  if (is_last_line(line, number_of_lines)) {
    auto line_length = this->input_.size() - line_begin_offset;
    if (line_is_ascii) {
      if (character > line_length) {
        return &this->input_[this->input_.size()];
      } else {
        return &this->input_[line_begin_offset + character];
      }
    } else {
      string8_view line_string(&this->input_[line_begin_offset], line_length);
      return advance_lsp_characters_in_utf_8(line_string, character);
    }
  } else {
    auto line_end_offset = this->offset_of_lines_[line + 1];
    auto line_length_with_terminator = line_end_offset - line_begin_offset;
    if (line_is_ascii) {
      // Character is out of bounds.
      if (character >= line_length_with_terminator - 1) {
        if (line_length_with_terminator >= 2 &&
            this->input_[line_end_offset - 2] == u8'\r' &&
            this->input_[line_end_offset - 1] == u8'\n') {
          // Return the "\r\n".
          return &this->input_[line_end_offset - 2];
        } else {
          // Return the "\n" or the "\r".
          return &this->input_[line_end_offset - 1];
        }
      } else {
        return &this->input_[line_begin_offset + character];
      }
    } else {
      offset_type line_terminator_length;
      if (line_length_with_terminator >= 2 &&
          this->input_[line_end_offset - 2] == u8'\r' &&
          this->input_[line_end_offset - 1] == u8'\n') {
        line_terminator_length = 2;
      } else {
        line_terminator_length = 1;
      }
      auto line_length = line_length_with_terminator - line_terminator_length;
      string8_view line_string(&this->input_[line_begin_offset], line_length);
      return advance_lsp_characters_in_utf_8(line_string, character);
    }
  }
}

typename locator::range_type locator::range(source_code_span span) const {
  position_type start = this->position(span.begin());
  position_type end = this->position(span.end());
  return range_type{.start = start, .end = end};
}

typename locator::position_type locator::position(const char8 *source) const
    noexcept {
  offset_type offset = this->offset(source);
  offset_type line_number = this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

typename locator::offset_type locator::offset(const char8 *source) const
    noexcept {
  return narrow_cast<offset_type>(source - this->input_.data());
}

typename locator::offset_type
locator::find_line_at_offset(offset_type offset) const {
  QLJS_ASSERT(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::upper_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<offset_type>((offset_of_following_line_it - 1) -
                                  this->offset_of_lines_.begin());
}

typename locator::position_type locator::position(int line_number,
                                                  offset_type offset) const
    noexcept {
  offset_type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number)];
  bool line_is_ascii =
      this->line_is_ascii_[narrow_cast<std::size_t>(line_number)];

  offset_type character;
  if (line_is_ascii) {
    character = offset - beginning_of_line_offset;
  } else {
    character = count_lsp_characters_in_utf_8(
        this->input_.substr(beginning_of_line_offset),
        offset - beginning_of_line_offset);
  }

  return position_type{.line = line_number, .character = character};
}
#else
locator::locator(padded_string_view input) noexcept : input_(input) {}

typename locator::range_type locator::range(source_code_span span) const {
  position_type start = this->position(span.begin());
  position_type end = this->position(span.end());
  return range_type{.start = start, .end = end};
}

typename locator::position_type locator::position(const char8 *ch) const
    noexcept {
  std::size_t byte_offset = narrow_cast<std::size_t>(ch - this->input_.data());
  std::size_t count = count_utf_8_characters(this->input_, byte_offset);
  return narrow_cast<position_type>(count);
}
#endif

} // namespace sublime_text
} // namespace quick_lint_js

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
