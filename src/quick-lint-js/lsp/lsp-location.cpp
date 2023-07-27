// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/utf-8.h>

namespace quick_lint_js {
namespace {
// Like std::transform with an std::back_insert_iterator, but more efficient for
// std::vector<int>.
template <class Input_It, class Output, class Transformer>
void insert_back_transform(Input_It input_begin, Input_It input_end,
                           Output &output, Transformer &&transformer) {
  using Difference_Type = typename Output::difference_type;
  std::size_t original_size = output.size();
  std::size_t final_size =
      original_size + narrow_cast<std::size_t>(input_end - input_begin);
  output.resize(final_size);
  auto output_it = output.begin() + narrow_cast<Difference_Type>(original_size);
  output_it = std::transform(input_begin, input_end, output_it, transformer);
  QLJS_ASSERT(output_it == output.end());
}
}

bool operator==(const LSP_Position &lhs, const LSP_Position &rhs) {
  return lhs.line == rhs.line && lhs.character == rhs.character;
}

bool operator!=(const LSP_Position &lhs, const LSP_Position &rhs) {
  return !(lhs == rhs);
}

LSP_Locator::LSP_Locator(Padded_String_View input) : input_(input) {
  this->cache_offsets_of_lines();
}

LSP_Range LSP_Locator::range(Source_Code_Span span) const {
  LSP_Position start = this->position(span.begin());
  LSP_Position end = this->position(span.end());
  return LSP_Range{.start = start, .end = end};
}

LSP_Position LSP_Locator::position(const Char8 *source) const {
  Offset_Type offset = this->offset(source);
  int line_number = this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

const Char8 *LSP_Locator::from_position(LSP_Position position) const {
  int line = position.line;
  int character = position.character;
  if (line < 0 || character < 0) {
    return nullptr;
  }

  int number_of_lines = narrow_cast<int>(this->offset_of_lines_.size());
  if (line >= number_of_lines) {
    return this->input_.null_terminator();
  }

  Offset_Type line_begin_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line)];
  bool line_is_ascii = this->line_is_ascii_[narrow_cast<std::size_t>(line)];
  bool is_last_line = line == number_of_lines - 1;
  if (is_last_line) {
    // TODO(strager): Get rid of this narrow_cast.
    Offset_Type line_length =
        narrow_cast<Offset_Type>(this->input_.size() - line_begin_offset);
    if (line_is_ascii) {
      if (character > line_length) {
        return &this->input_[this->input_.size()];
      } else {
        return &this->input_[line_begin_offset + character];
      }
    } else {
      String8_View line_string(&this->input_[line_begin_offset],
                               narrow_cast<std::size_t>(line_length));
      return advance_lsp_characters_in_utf_8(line_string, character);
    }
  } else {
    Offset_Type line_end_offset =
        this->offset_of_lines_[narrow_cast<std::size_t>(line + 1)];
    Offset_Type line_length_including_terminator =
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
      Offset_Type line_terminator_length =
          line_length_including_terminator >= 2 &&
                  this->input_[line_end_offset - 2] == u8'\r' &&
                  this->input_[line_end_offset - 1] == u8'\n'
              ? 2
              : 1;
      Offset_Type line_length =
          line_length_including_terminator - line_terminator_length;
      String8_View line_string(&this->input_[line_begin_offset],
                               narrow_cast<std::size_t>(line_length));
      return advance_lsp_characters_in_utf_8(line_string, character);
    }
  }
}

void LSP_Locator::replace_text(LSP_Range range, String8_View replacement_text,
                               Padded_String_View new_input) {
  Offset_Type start_offset = narrow_cast<Offset_Type>(
      this->from_position(range.start) - this->input_.data());
  Offset_Type end_offset = narrow_cast<Offset_Type>(
      this->from_position(range.end) - this->input_.data());
  Offset_Type replacement_text_size =
      narrow_cast<Offset_Type>(replacement_text.size());

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
                                 this->old_line_is_ascii_[start_line] &&
                                 this->old_line_is_ascii_[end_line]);

  // Offsets after replacement: adjust with a fixed offset.
  Offset_Type net_bytes_added =
      replacement_text_size - (end_offset - start_offset);
  insert_back_transform(this->old_offset_of_lines_.begin() +
                            narrow_cast<std::ptrdiff_t>(end_line) + 1,
                        this->old_offset_of_lines_.end(),
                        this->offset_of_lines_,
                        [&](Offset_Type offset) -> Offset_Type {
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

void LSP_Locator::validate_caches_debug() const {
  LSP_Locator temp(this->input_);

  bool offsets_match =
      ranges_equal(this->offset_of_lines_, temp.offset_of_lines_);
  QLJS_ALWAYS_ASSERT(offsets_match);
  bool asciinesses_match =
      ranges_equal(this->line_is_ascii_, temp.line_is_ascii_,
                   [](bool actual_asciiness, bool expected_asciiness) -> bool {
                     if (!actual_asciiness) {
                       // It's okay if we think an ASCII-only line is non-ASCII.
                       return true;
                     }
                     return actual_asciiness == expected_asciiness;
                   });
  QLJS_ALWAYS_ASSERT(asciinesses_match);
}

void LSP_Locator::cache_offsets_of_lines() {
  QLJS_ASSERT(this->offset_of_lines_.empty());
  QLJS_ASSERT(this->line_is_ascii_.empty());

  constexpr int estimated_bytes_per_line = 64;
  std::size_t estimated_lines =
      narrow_cast<std::size_t>(this->input_.size() / estimated_bytes_per_line);
  this->offset_of_lines_.reserve(estimated_lines);
  this->line_is_ascii_.reserve(estimated_lines);

  this->offset_of_lines_.push_back(0);
  bool last_line_is_ascii;
  this->compute_offsets_of_lines(
      /*begin=*/this->input_.data(),
      /*end=*/this->input_.null_terminator(),
      /*out_last_line_is_ascii=*/&last_line_is_ascii);
  this->line_is_ascii_.push_back(last_line_is_ascii);
}

void LSP_Locator::compute_offsets_of_lines(const Char8 *begin, const Char8 *end,
                                           bool *out_last_line_is_ascii) {
  auto add_beginning_of_line = [this](const Char8 *beginning_of_line) -> void {
    this->offset_of_lines_.push_back(
        narrow_cast<Offset_Type>(beginning_of_line - this->input_.data()));
  };
  std::uint8_t flags = 0;
  auto is_line_ascii = [&flags]() -> bool { return (flags & 0x80) == 0; };
  auto add_end_of_line = [&]() -> void {
    this->line_is_ascii_.push_back(is_line_ascii());
    flags = 0;
  };

  for (const Char8 *c = begin; c != end;) {
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

int LSP_Locator::find_line_at_offset(Offset_Type offset) const {
  QLJS_ASSERT(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::upper_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<int>((offset_of_following_line_it - 1) -
                          this->offset_of_lines_.begin());
}

LSP_Locator::Offset_Type LSP_Locator::offset(const Char8 *source) const {
  return narrow_cast<Offset_Type>(source - this->input_.data());
}

LSP_Position LSP_Locator::position(int line_number, Offset_Type offset) const {
  Offset_Type beginning_of_line_offset =
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

  return LSP_Position{.line = line_number, .character = character};
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
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
