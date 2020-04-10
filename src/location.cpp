// quicklint-js finds bugs in JavaScript programs.
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
#include <quicklint-js/location.h>

namespace quicklint_js {
source_position source_range::begin() const noexcept { return this->begin_; }

source_position source_range::end() const noexcept { return this->end_; }

source_range locator::range(source_code_span span) const {
  return source_range(this->position(span.begin()), this->position(span.end()));
}

source_position locator::position(const char *source) const noexcept {
  decltype(source_position::offset) offset = source - this->input_;
  int number_of_line_terminators = 0;
  const char *last_line_terminator = nullptr;
  for (const char *c = this->input_; c != source; ++c) {
    if (*c == '\n') {
      number_of_line_terminators += 1;
      last_line_terminator = c;
    }
  }
  int column_number;
  if (last_line_terminator) {
    column_number = source - last_line_terminator;
  } else {
    column_number = offset + 1;
  }
  return source_position{1 + number_of_line_terminators, column_number, offset};
}
}  // namespace quicklint_js
