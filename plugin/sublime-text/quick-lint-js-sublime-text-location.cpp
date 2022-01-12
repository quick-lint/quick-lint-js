// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

//==============================================================================
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           sublime text location                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#include <quick-lint-js-sublime-text-location.h>
#include <quick-lint-js-sublime-text-utils.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-8.h>

namespace quick_lint_js {

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
// sublime_text_locator

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
void sublime_text_locator::replace_text(range_type range,
                                        string8_view replacement,
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
  offset_type after_replacement = region.end + 1;
  {
    auto adjust_offset = [](offset_type offset) {
      static auto adjust = replacement.size() - (region.end - region.begin);
      return offset + adjust;
    };
    insert_back_transform(
        this->old_lines.offset_begin_.begin() + after_replacement,
        this->old_lines.offset_begin_.end(), this->new_lines, adjust_offset);
  }
  {
    this->new_lines.is_ascii_.insert(
        this->new_lines.is_ascii_.end(),
        this->old_lines.is_ascii_.begin() + after_replacement,
        this->old_lines->is_ascii_.end());
  }

  QLJS_ASSERT(std::is_sorted(this->new_lines.offset_begin_.begin(),
                             this->new_lines.offset_begin_.end()));
  QLJS_ASSERT(this->new_lines.offset_begin_.size() ==
              this->new_lines.is_ascii_.size());
}

const char8 *sublime_text_locator::from_position(position_type position) const
    noexcept {
  offset_type line = position.line;
  offset_type line_offset_begin = this->new_lines.offset_begin_[line];
  offset_type line_offset_end = this->new_lines.offset_begin_[line + 1];
  bool line_is_last = this->new_lines.is_last_line(line);
  bool line_is_ascii = this->new_lines.is_ascii[line];
  offset_type character = position.character;

  auto is_microsoft_newline = [this, line_offset_end] {
    return characters::is_microsoft_newline(&this->input_[line_offset_end - 2]);
  };

  if (line >= this->new_lines.size()) {
    return &this->input_[this->input_.size()];
  }

  if (line_is_last) {
    offset_type line_length = this->input_.size() - line_offset_begin;
    if (line_is_ascii) {
      if (character > line_length) {
        return &this->input_[this->input_.size()];
      } else {
        return &this->input_[line_offset_begin + character];
      }
    } else {
      string8_view line_string(&this->input_[line_begin_offset], line_length);
      return advance_lsp_characters_in_utf_8(line_string, character);
    }
  } else {
    offset_type line_length_with_newline = line_end_offset - line_begin_offset;
    bool character_is_out_of_bounds = character >= line_length_with_newline - 1;
    if (line_is_ascii) {
      if (character_is_out_of_bounds) {
        if (line_length_with_newline >= 2 && is_microsoft_newline()) {
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
      offset_type line_length;
      if (line_length_with_newline >= 2 && is_microsoft_newline()) {
        line_length = line_length_with_newline - 2;
      } else {
        line_length = line_length_with_newline - 1;
      }
      string8_view line_string(&this->input_[line_begin_offset], line_length);
      return advance_lsp_characters_in_utf_8(line_string, character);
    }
  }
}

typename sublime_text_locator::range_type sublime_text_locator::range(
    source_code_span span) const {
  position_type start = this->position(span.begin());
  position_type end = this->position(span.end());
  return range_type{.start = start, .end = end};
}

typename sublime_text_locator::position_type sublime_text_locator::position(
    const char8 *source) const noexcept {
  offset_type offset = this->offset(source);
  offset_type line_number =  // this->find_line_at_offset(offset);
      return this->position(line_number, offset);
}

typename sublime_text_locator::offset_type sublime_text_locator::offset(
    const char8 *source) const noexcept {
  return narrow_cast<offset_type>(source - this->input_.data());
}

typename sublime_text_locator::position_type sublime_text_locator::position(
    int line_number, offset_type offset) const noexcept {
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
sublime_text_locator::sublime_text_locator(padded_string_view input) noexcept
    : input_(input) {}

typename sublime_text_locator::range_type sublime_text_locator::range(
    source_code_span span) const {
  position_type start = this->position(span.begin());
  position_type end = this->position(span.end());
  return range_type{.start = start, .end = end};
}

typename sublime_text_locator::offset_type sublime_text_locator::position(
    const char8 *ch) const noexcept {
  std::size_t byte_offset = narrow_cast<std::size_t>(ch - this->input_.data());
  std::size_t count = count_utf_8_characters(this->input_, byte_offset);
  return narrow_cast<position_type>(count);
}
#endif

}  // namespace quick_lint_js

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
