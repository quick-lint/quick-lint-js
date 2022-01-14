// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

//==============================================================================
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           sublime text location                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#include <quick-lint-js/assert.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/sublime-text-location.h>
#include <quick-lint-js/utf-8.h>
#include <quick-lint-js/vector.h>

namespace quick_lint_js {

//==============================================================================
//------------------------------------------------------------------------------
// lines

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
void sublime_text_lines::compute(const char8 *input, const char8 *begin,
                                 const char8 *end) {
  std::uint8_t flags = 0;
  auto on_line_begin = [this](const char8 *line_begin) {
    this->offset_begin_.push_back(line_begin - input);
  };
  auto on_character = [flags](const char8 *character) {
    flags |= static_cast<std::uint8_t>(*character);
  };
  auto on_line_end = [flags, this](const char8 * /*line_end*/) {
    this->is_ascii_.push_back(
        sublime_text_characters::is_ascii(static_cast<char8>(flags)));
    flags = 0;
  };

  const char8 *ch = begin + 1;
  on_line_begin(ch);
  while (ch != end) {
    on_character(ch);
    if (sublime_text_characters::is_newline(*ch)) {
      on_line_end(ch);
      ch += sublime_text_characters::is_microsoft_newline(ch) ? 2 : 1;
      on_line_begin(ch);
    } else {
      ch += 1;
    }
  }
  on_line_end(ch);
}

typename sublime_text_lines::offset_type sublime_text_lines::find_line(
    offset_type offset) const {
  QLJS_ASSERT(!this->offset_begin_.empty());
  auto begin_it = this->offset_begin_.begin();
  auto end_it = this->offset_begin_.end();
  auto line_it = std::upper_bound(begin_it + 1, end_it, offset) - 1;
  auto line = line_it - this->offset_begin_.begin();
  return narrow_cast<offset_type>(line);
}
#endif

//==============================================================================
//------------------------------------------------------------------------------
// locator

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
explicit sublime_text_locator(padded_string_view input) noexcept
    : input_(input) {
  this->new_lines.compute(this->input_, 0, this->input_.size());
}

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
    offset_type size_without_end = this->new_lines.is_ascii_.size() - 1;
    if (size_without_end > range.start.line) {
      this->new_lines.is_ascii_[range.start.line] =
          this->new_lines.is_ascii_[range.start.line] &&
          this->old_lines.is_ascii_[range.start.line];
    }
  }
  {
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
    return sublime_text_characters::is_microsoft_newline(
        &this->input_[line_offset_end - 2]);
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

typename sublime_text_locator::range_type range(source_code_span span) const {
  position_type start = this->position(span.begin());
  position_type end = this->position(span.end());
  return range_type{.start = start, .end = end};
}

typename sublime_text_locator::position_type sublime_text_locator::position(
    const char8 *source) const noexcept {
  offset_type offset = this->offset(source);
  offset_type line = this.new_lines.find_line(offset);
  return this->position(line, offset);
}

typename sublime_text_locator::position_type sublime_text_locator::position(
    offset_type line, offset_type offset) const noexcept {
  offset_type line_offset_begin = this->new_lines.offset_begin_[line];
  bool line_is_ascii = this->new_lines.is_ascii_[line];
  offset_type character;

  if (line_is_ascii) {
    character = offset - line_offset_begin;
  } else {
    character = count_lsp_characters_in_utf_8(
        this->input_.substr(line_offset_begin), offset - line_offset_begin);
  }

  return position_type{.line = line, .character = character};
}

typename sublime_text_locator::region_type sublime_text_locator::region(
    range_type range) const noexcept {
  offset_type begin = this->offset(range.start);
  offset_type end = this->offset(range.end);
  return region_type{.begin = begin, .end = end};
}

typename sublime_text_locator::offset_type sublime_text_locator::offset(
    const char8 *source) const noexcept {
  return narrow_cast<offset_type>(source - this->input_.data());
}

typename sublime_text_locator::offset_type sublime_text_locator::offset(
    position_type position) const noexcept {
  const char8 *source = this->from_position(position);
  return this->offset(source);
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

typename sublime_text_locator::position_type sublime_text_locator::position(
    const char8 *source) const noexcept {
  const char8 *input_data = this->input_.data();
  std::size_t byte_offset = narrow_cast<std::size_t>(source - input_data);
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
