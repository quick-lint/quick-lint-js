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

#include <cstddef>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/generate-code.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <random>
#include <utility>
#include <vector>

namespace quick_lint_js {
std::vector<int> random_line_lengths(std::mt19937_64 &rng, int line_count) {
  // Based on jQuery 3.5.1.
  std::normal_distribution distribution(/*mean=*/22.0, /*stddev=*/28.0);

  std::vector<int> line_lengths;
  line_lengths.reserve(narrow_cast<std::size_t>(line_count));
  for (int i = 0; i < line_count; ++i) {
    int columns = static_cast<int>(distribution(rng));
    if (columns < 0) columns = 0;
    if (columns > 99) columns = 0;
    line_lengths.emplace_back(columns);
  }
  return line_lengths;
}

padded_string make_source_code(const std::vector<int> &line_lengths,
                               const string8 &newline) {
  string8 source;
  for (int line_length : line_lengths) {
    source += string8(narrow_cast<std::size_t>(line_length), u8'x');
    source += newline;
  }
  return padded_string(std::move(source));
}
}
