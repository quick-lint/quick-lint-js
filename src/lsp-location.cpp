// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <ostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/utf-8.h>

namespace quick_lint_js {
namespace {
// Like std::transform with an std::back_insert_iterator, but more efficient for
// std::vector<int>.
template <class InputIt, class Output, class Transformer>
void insert_back_transform(InputIt input_begin, InputIt input_end,
                           Output &output, Transformer &&transformer) {
  using difference_type = typename Output::difference_type;
  std::size_t original_size = output.size();
  std::size_t final_size =
      original_size + narrow_cast<std::size_t>(input_end - input_begin);
  output.resize(final_size);
  auto output_it = output.begin() + narrow_cast<difference_type>(original_size);
  output_it = std::transform(input_begin, input_end, output_it, transformer);
  QLJS_ASSERT(output_it == output.end());
}
}

bool operator==(const lsp_position &lhs, const lsp_position &rhs) noexcept {
  return lhs.line == rhs.line && lhs.character == rhs.character;
}

bool operator!=(const lsp_position &lhs, const lsp_position &rhs) noexcept {
  return !(lhs == rhs);
}

std::ostream &operator<<(std::ostream &stream, const lsp_position &position) {
  stream << "line " << position.line << " character " << position.character;
  return stream;
}

lsp_locator::lsp_locator(padded_string_view input) noexcept : input_(input) {
  this->cache_offsets_of_lines();
}

lsp_range lsp_locator::range(source_code_span span) const {
  lsp_position start = this->position(span.begin());
  lsp_position end = this->position(span.end());
  return lsp_range{.start = start, .end = end};
}

lsp_position lsp_locator::position(const char8 *source) const noexcept {
  offset_type offset = this->offset(source);
  int line_number = this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

const char8 *lsp_locator::from_position(lsp_position position) const noexcept {
  int line = position.line;
  int character = position.character;
  if (line < 0 || character < 0) {
    return nullptr;
  }

  int number_of_lines = narrow_cast<int>(this->offset_of_lines_.size());
  if (line >= number_of_lines) {
    return this->input_.null_terminator();
  }

  offset_type line_begin_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line)];
  bool line_is_ascii = this->line_is_ascii_[narrow_cast<std::size_t>(line)];
  bool is_last_line = line == number_of_lines - 1;
  if (is_last_line) {
    offset_type line_length = this->input_.size() - line_begin_offset;
    if (line_is_ascii) {
      if (character > line_length) {
        return &this->input_[this->input_.size()];
      } else {
        return &this->input_[line_begin_offset + character];
      }
    } else {
      string8_view line_string(&this->input_[line_begin_offset],
                               narrow_cast<std::size_t>(line_length));
      return advance_lsp_characters_in_utf_8(line_string, character);
    }
  } else {
    offset_type line_end_offset =
        this->offset_of_lines_[narrow_cast<std::size_t>(line + 1)];
    offset_type line_length_including_terminator =
        line_end_offset - line_begin_offset;
    if (line_is_ascii) {
      bool character_is_out_of_bounds =
          character >= line_length_including_terminator - 1;
      if (character_is_out_of_bounds) {
        if (line_length_including_terminator >= 2 &&
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
      offset_type line_terminator_length =
          line_length_including_terminator >= 2 &&
                  this->input_[line_end_offset - 2] == u8'\r' &&
                  this->input_[line_end_offset - 1] == u8'\n'
              ? 2
              : 1;
      offset_type line_length =
          line_length_including_terminator - line_terminator_length;
      string8_view line_string(&this->input_[line_begin_offset],
                               narrow_cast<std::size_t>(line_length));
      return advance_lsp_characters_in_utf_8(line_string, character);
    }
  }
}

void lsp_locator::replace_text(lsp_range range, string8_view replacement_text,
                               padded_string_view new_input) {
  offset_type start_offset = narrow_cast<offset_type>(
      this->from_position(range.start) - this->input_.data());
  offset_type end_offset = narrow_cast<offset_type>(
      this->from_position(range.end) - this->input_.data());
  offset_type replacement_text_size =
      narrow_cast<offset_type>(replacement_text.size());

  QLJS_ASSERT(!this->offset_of_lines_.empty());
  std::size_t start_line = narrow_cast<std::size_t>(range.start.line);
  std::size_t end_line = std::min(this->offset_of_lines_.size() - 1,
                                  narrow_cast<std::size_t>(range.end.line));

  this->input_ = new_input;
  std::swap(this->old_offset_of_lines_, this->offset_of_lines_);
  std::swap(this->old_line_is_ascii_, this->line_is_ascii_);
  this->offset_of_lines_.reserve(this->old_offset_of_lines_.size());
  this->offset_of_lines_.clear();
  this->line_is_ascii_.reserve(this->old_line_is_ascii_.size());
  this->line_is_ascii_.clear();

  // Offsets before replacement: do not adjust.
  this->offset_of_lines_.insert(
      this->offset_of_lines_.end(), this->old_offset_of_lines_.begin(),
      this->old_offset_of_lines_.begin() + range.start.line + 1);
  this->line_is_ascii_.insert(
      this->line_is_ascii_.end(), this->old_line_is_ascii_.begin(),
      this->old_line_is_ascii_.begin() + range.start.line);

  // Offsets within replacement: re-parse newlines.
  bool last_line_of_replacement_is_ascii;
  this->compute_offsets_of_lines(
      /*begin=*/&this->input_[start_offset],
      /*end=*/&this->input_[start_offset + replacement_text_size],
      /*out_last_line_is_ascii=*/&last_line_of_replacement_is_ascii);
  if (this->line_is_ascii_.size() > start_line) {
    this->line_is_ascii_[start_line] = this->line_is_ascii_[start_line] &&
                                       this->old_line_is_ascii_[start_line];
  }
  this->line_is_ascii_.push_back(last_line_of_replacement_is_ascii &&
                                 this->old_line_is_ascii_[end_line]);

  // Offsets after replacement: adjust with a fixed offset.
  offset_type net_bytes_added =
      replacement_text_size - (end_offset - start_offset);
  insert_back_transform(this->old_offset_of_lines_.begin() +
                            narrow_cast<std::ptrdiff_t>(end_line) + 1,
                        this->old_offset_of_lines_.end(),
                        this->offset_of_lines_,
                        [&](offset_type offset) -> offset_type {
                          return offset + net_bytes_added;
                        });
  this->line_is_ascii_.insert(this->line_is_ascii_.end(),
                              this->old_line_is_ascii_.begin() +
                                  narrow_cast<std::ptrdiff_t>(end_line) + 1,
                              this->old_line_is_ascii_.end());

  QLJS_ASSERT(std::is_sorted(this->offset_of_lines_.begin(),
                             this->offset_of_lines_.end()));
  QLJS_ASSERT(this->offset_of_lines_.size() == this->line_is_ascii_.size());
}

void lsp_locator::cache_offsets_of_lines() {
  QLJS_ASSERT(this->offset_of_lines_.empty());
  QLJS_ASSERT(this->line_is_ascii_.empty());

  this->offset_of_lines_.push_back(0);
  bool last_line_is_ascii;
  this->compute_offsets_of_lines(
      /*begin=*/this->input_.data(),
      /*end=*/this->input_.null_terminator(),
      /*out_last_line_is_ascii=*/&last_line_is_ascii);
  this->line_is_ascii_.push_back(last_line_is_ascii);
}

void lsp_locator::compute_offsets_of_lines(const char8 *begin, const char8 *end,
                                           bool *out_last_line_is_ascii) {
  auto add_beginning_of_line = [this](const char8 *beginning_of_line) -> void {
    this->offset_of_lines_.push_back(
        narrow_cast<offset_type>(beginning_of_line - this->input_.data()));
  };
  std::uint8_t flags = 0;
  auto is_line_ascii = [&flags]() -> bool { return (flags & 0x80) == 0; };
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
  *out_last_line_is_ascii = is_line_ascii();
}

int lsp_locator::find_line_at_offset(offset_type offset) const {
  QLJS_ASSERT(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::upper_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<int>((offset_of_following_line_it - 1) -
                          this->offset_of_lines_.begin());
}

lsp_locator::offset_type lsp_locator::offset(const char8 *source) const
    noexcept {
  return narrow_cast<offset_type>(source - this->input_.data());
}

lsp_position lsp_locator::position(int line_number, offset_type offset) const
    noexcept {
  offset_type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number)];
  bool line_is_ascii =
      this->line_is_ascii_[narrow_cast<std::size_t>(line_number)];

  int character;
  if (line_is_ascii) {
    character = narrow_cast<int>(offset - beginning_of_line_offset);
  } else {
    character = narrow_cast<int>(count_lsp_characters_in_utf_8(
        this->input_.substr(beginning_of_line_offset),
        offset - beginning_of_line_offset));
  }

  return lsp_position{.line = line_number, .character = character};
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
