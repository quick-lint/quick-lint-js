// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <algorithm>
#include <cstddef>
#include <ostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/simd.h>
#include <xmmintrin.h>

namespace quick_lint_js {
std::ostream &operator<<(std::ostream &out, const source_position &p) {
  out << "source_position{" << p.line_number << ',' << p.column_number << ','
      << p.offset << '}';
  return out;
}

source_position source_range::begin() const noexcept { return this->begin_; }

source_position source_range::end() const noexcept { return this->end_; }

bool operator==(source_code_span x, string8_view y) noexcept {
  return x.string_view() == y;
}

bool operator!=(source_code_span x, string8_view y) noexcept {
  return !(x == y);
}

locator::locator(padded_string_view input) noexcept : input_(input.c_str()) {}

source_range locator::range(source_code_span span) const {
  source_position begin = this->position(span.begin());
  source_position end = this->position(span.end());
  return source_range(begin, end);
}

source_position locator::position(const char8 *source) const noexcept {
  source_position::offset_type offset = this->offset(source);
  source_position::line_number_type line_number =
      this->find_line_at_offset(offset);
  return this->position(line_number, offset);
}

void locator::cache_offsets_of_lines() const {
  this->offset_of_lines_.push_back(0);
  for (const char8 *c = this->input_; *c != '\0';) {
    // TODO(strager): Force alignment of padded_string, then use aligned loads.
    int newline_mask = _mm_movemask_epi8(
        _mm_cmpeq_epi8(_mm_loadu_si128(reinterpret_cast<const __m128i *>(c)),
                       _mm_set1_epi8('\n')));
#pragma GCC unroll 16
    for (int i = 0; i < 16; ++i) {
      if (newline_mask & (1 << i)) {
        const char8 *beginning_of_line = &c[i + 1];
        this->offset_of_lines_.push_back(
            narrow_cast<source_position::offset_type>(beginning_of_line -
                                                      this->input_));
      }
    }
    c += 16;
  }
}

source_position::line_number_type locator::find_line_at_offset(
    source_position::offset_type offset) const {
  if (this->offset_of_lines_.empty()) {
    this->cache_offsets_of_lines();
  }
  assert(!this->offset_of_lines_.empty());
  auto offset_of_following_line_it = std::lower_bound(
      this->offset_of_lines_.begin() + 1, this->offset_of_lines_.end(), offset);
  return narrow_cast<source_position::line_number_type>(
             (offset_of_following_line_it - 1) -
             this->offset_of_lines_.begin()) +
         1;
}

source_position::offset_type locator::offset(const char8 *source) const
    noexcept {
  return narrow_cast<source_position::offset_type>(source - this->input_);
}

source_position locator::position(source_position::line_number_type line_number,
                                  source_position::offset_type offset) const
    noexcept {
  source_position::offset_type beginning_of_line_offset =
      this->offset_of_lines_[narrow_cast<std::size_t>(line_number - 1)];
  int column_number = narrow_cast<int>(offset - beginning_of_line_offset) + 1;
  return source_position{line_number, column_number, offset};
}
}  // namespace quick_lint_js
