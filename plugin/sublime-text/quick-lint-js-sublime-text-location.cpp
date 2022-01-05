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
// locator

#if QLJS_SUBLIME_TEXT_HAVE_INCREMENTAL_CHANGES
sublime_text_locator::sublime_text_locator(padded_string_view input) noexcept
    : input_(input) {
  this->cache_offsets_of_lines();
}

void sublime_text_locator::replace_text(range_type range,
                                        string8_view replacement_text,
                                        padded_string_view new_input) {
  offset_type start_offset = this->offset(this->from_position(range.start));
  offset_type end_offset = this->offset(this->from_position(range.end));
  offset_type replacement_text_size = narrow_cast<offset_type>(replacement_text.size());

  QLJS_ASSERT(!this->offset_of_lines_.empty());
  std::size_t start_line = narrow_cast<std::size_t>(range.start.line);
  std::size_t end_line = narrow_cast<std::size_t>(range.end.line);
  end_line = std::min(this->offset_of_lines_.size() - 1, end_line);

  this->input_ = new_input;
  std::swap(this->old_offset_of_lines_, this->offset_of_lines_);
  std::swap(this->old_line_is_ascii_, this->line_is_ascii_);
  this->offset_of_lines_.reserve(this->old_offset_of_lines_.size());
  this->offset_of_lines_.clear();
  this->line_is_ascii_.reserve(this->old_line_is_ascii_.size());
  this->line_is_ascii_.clear();

  // Offsets before replacement: do not adjust.

  // Why range.start.line + 1 and not just range.start.line?
  // Re: The offset of the edited line can't change because the offset is the
  // beginning of line offset, so we use the offset already computed.
  auto old_offset_of_lines_before_replacement_iterator =
      this->old_offset_of_lines_.begin() + range.start.line + 1;
  auto old_line_is_ascii_before_replacement_iterator =
      this->old_line_is_ascii_.begin() + range.start.line;
  this->offset_of_lines_.insert(this->offset_of_lines_.end(),
                                this->old_offset_of_lines_.begin(),
                                old_offset_of_lines_before_replacement_iterator);
  this->line_is_ascii_.insert(this->line_is_ascii_.end(),
                              this->old_line_is_ascii_.begin(),
                              old_line_is_ascii_before_replacement_iterator);

  // Offsets within replacement: re-parse newlines.
  offset_type replacement_end_offset = start_offset + replacement_text_size;
  bool last_line_of_replacement_is_ascii = this->compute_offsets_of_lines(
      &this->input_[start_offset], &this->input_[replacement_end_offset]);
  if (this->line_is_ascii_.size() > start_line) {
    this->line_is_ascii_[start_line] =
        this->line_is_ascii_[start_line] && this->old_line_is_ascii_[start_line];
  }
  this->line_is_ascii_.push_back(last_line_of_replacement_is_ascii &&
                                 this->old_line_is_ascii_[end_line]);

  // Offsets after replacement: adjust with a fixed offset.
  auto net_bytes_added = replacement_text_size - (end_offset - start_offset);
  insert_back_transform(
      this->old_offset_of_lines_.begin() + narrow_cast<std::ptrdiff_t>(end_line) + 1,
      this->old_offset_of_lines_.end(),
      this->offset_of_lines_,
      [&](offset_type offset) -> offset_type { return offset + net_bytes_added; });
  this->line_is_ascii_.insert(this->line_is_ascii_.end(),
                              this->old_line_is_ascii_.begin() +
                                  narrow_cast<std::ptrdiff_t>(end_line) + 1,
                              this->old_line_is_ascii_.end());

  QLJS_ASSERT(
      std::is_sorted(this->offset_of_lines_.begin(), this->offset_of_lines_.end()));
  QLJS_ASSERT(this->offset_of_lines_.size() == this->line_is_ascii_.size());
}

void sublime_text_locator::cache_offsets_of_lines() {
  QLJS_ASSERT(this->offset_of_lines_.empty());
  QLJS_ASSERT(this->line_is_ascii_.empty());

  this->offset_of_lines_.push_back(0);
  bool last_line_is_ascii = this->compute_offsets_of_lines(
      this->input_.data(), this->input_.null_terminator());
  this->line_is_ascii_.push_back(last_line_is_ascii);
}

typename sublime_text_locator::range_type
sublime_text_locator::range(source_code_span span) const {
  position_type start = this->position(span.begin());
  position_type end = this->position(span.end());
  return range_type{.start = start, .end = end};
}

typename sublime_text_locator::position_type
sublime_text_locator::position(const char8 *source) const noexcept {
  offset_type offset = this->offset(source);
  offset_type line_number = this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

const char8 *sublime_text_locator::from_position(position_type position) const
    noexcept {
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

bool sublime_text_locator::compute_offsets_of_lines(const char8 *begin,
                                                    const char8 *end) {
  std::uint8_t flags = 0;
  auto is_line_ascii = [&flags]() -> bool { return (flags & 0x80) == 0; };
  auto add_beginning_of_line = [this](const char8 *beginning_of_line) -> void {
    this->offset_of_lines_.push_back(
        narrow_cast<offset_type>(beginning_of_line - this->input_.data()));
  };
  auto add_end_of_line = [&]() -> void {
    this->line_is_ascii_.push_back(is_line_ascii());
    flags = 0;
  };

  for (const char8 *c = begin; c != end;) {
    flags |= static_cast<std::uint8_t>(*c);
    if (*c == u8'\n' || *c == u8'\r') {
      if (c[0] == u8'\r' && c[1] == u8'\n') {
        c += 2;
        add_end_of_line();
        add_beginning_of_line(c);
      } else {
        c += 1;
        add_end_of_line();
        add_beginning_of_line(c);
      }
    } else {
      c += 1;
    }
  }
  return is_line_ascii();
}

typename sublime_text_locator::offset_type
sublime_text_locator::find_line_at_offset(offset_type offset) const {
  QLJS_ASSERT(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::upper_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<offset_type>((offset_of_following_line_it - 1) -
                                  this->offset_of_lines_.begin());
}

typename sublime_text_locator::offset_type
sublime_text_locator::offset(const char8 *source) const noexcept {
  return narrow_cast<offset_type>(source - this->input_.data());
}

typename sublime_text_locator::position_type
sublime_text_locator::position(int line_number, offset_type offset) const noexcept {
  offset_type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number)];
  bool line_is_ascii = this->line_is_ascii_[narrow_cast<std::size_t>(line_number)];

  offset_type character;
  if (line_is_ascii) {
    character = offset - beginning_of_line_offset;
  } else {
    character =
        count_lsp_characters_in_utf_8(this->input_.substr(beginning_of_line_offset),
                                      offset - beginning_of_line_offset);
  }

  return position_type{.line = line_number, .character = character};
}
#else
sublime_text_locator::sublime_text_locator(padded_string_view input) noexcept
    : input_(input) {}

typename sublime_text_locator::range_type
sublime_text_locator::range(source_code_span span) const {
  offset_type begin = this->position(span.begin());
  offset_type end = this->position(span.end());
  return range_type{.begin = begin, .end = end};
}

typename sublime_text_locator::offset_type
sublime_text_locator::position(const char8 *ch) const noexcept {
  std::size_t byte_offset = narrow_cast<std::size_t>(ch - this->input_.data());
  std::size_t count = count_utf_8_characters(this->input_, byte_offset);
  return narrow_cast<offset_type>(count);
}
#endif

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
