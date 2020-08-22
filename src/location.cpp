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
#include <ostream>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
std::ostream &operator<<(std::ostream &out, const source_position &p) {
  out << "source_position{" << p.line_number << ',' << p.column_number << ','
      << p.offset << '}';
  return out;
}

source_position source_range::begin() const noexcept { return this->begin_; }

source_position source_range::end() const noexcept { return this->end_; }

bool operator==(source_code_span x, std::string_view y) noexcept {
  return x.string_view() == y;
}

bool operator!=(source_code_span x, std::string_view y) noexcept {
  return !(x == y);
}

locator::locator(const char *input) noexcept
    : input_(input),
      last_position_source_(input),
      last_position_last_line_terminator_(nullptr),
      last_position_number_of_line_terminators_(0) {}

source_range locator::range(source_code_span span) const {
  return source_range(this->position(span.begin()), this->position(span.end()));
}

source_position locator::position(const char *source) const noexcept {
  const char *last_line_terminator;
  int number_of_line_terminators;
  const char *start;
  if (source >= this->last_position_source_) {
    last_line_terminator = this->last_position_last_line_terminator_;
    number_of_line_terminators =
        this->last_position_number_of_line_terminators_;
    start = this->last_position_source_;
  } else {
    last_line_terminator = nullptr;
    number_of_line_terminators = 0;
    start = this->input_;
  }

  for (const char *c = start; c != source; ++c) {
    if (*c == '\n') {
      number_of_line_terminators += 1;
      last_line_terminator = c;
    }
  }
  this->last_position_source_ = source;
  this->last_position_last_line_terminator_ = last_line_terminator;
  this->last_position_number_of_line_terminators_ = number_of_line_terminators;

  source_position::offset_type offset =
      narrow_cast<source_position::offset_type>(source - this->input_);
  int column_number;
  if (last_line_terminator) {
    column_number = narrow_cast<int>(source - last_line_terminator);
  } else {
    column_number = narrow_cast<int>(offset + 1);
  }
  return source_position{1 + number_of_line_terminators, column_number, offset};
}
}  // namespace quick_lint_js
