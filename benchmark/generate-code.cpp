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
#include <memory>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/generate-code.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <random>
#include <set>
#include <utility>
#include <vector>

namespace quick_lint_js {
namespace {
template <class T>
void partial_shuffle(std::vector<T> &, std::mt19937_64 &, int rounds);
}

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

source_code_with_spans make_realisticish_code(int line_count, int span_count) {
  std::mt19937_64 rng;

  string8 newline = u8"\n";
  std::vector<int> line_lengths = random_line_lengths(rng, line_count);
  std::unique_ptr<padded_string> source =
      std::make_unique<padded_string>(make_source_code(line_lengths, newline));

  std::uniform_int_distribution span_length_distribution(1, 10);
  auto random_span_in_line = [&](int line_length,
                                 int line_begin_offset) -> source_code_span {
    int span_length = std::min(span_length_distribution(rng), line_length);
    int span_begin_line_offset =
        std::uniform_int_distribution(0, line_length - span_length)(rng);
    const char8 *span_begin =
        &(*source)[line_begin_offset + span_begin_line_offset];
    return source_code_span(span_begin, span_begin + span_length);
  };

  std::uniform_int_distribution span_line_number_distribution(1,
                                                              line_count + 1);
  std::multiset<int> span_line_numbers;
  for (int i = 0; i < span_count; ++i) {
    span_line_numbers.insert(span_line_number_distribution(rng));
  }

  std::vector<source_code_span> spans;
  int line_begin_offset = 0;
  for (int line_index = 0; line_index < narrow_cast<int>(line_lengths.size());
       ++line_index) {
    int line_length = line_lengths[narrow_cast<std::size_t>(line_index)];
    int line_span_count =
        narrow_cast<int>(span_line_numbers.count(line_index + 1));
    for (int i = 0; i < line_span_count; ++i) {
      spans.push_back(
          random_span_in_line(/*line_length=*/line_length,
                              /*line_begin_offset=*/line_begin_offset));
    }
    line_begin_offset += line_length + narrow_cast<int>(newline.size());
  }

  std::sort(spans.begin(), spans.end(),
            [](const source_code_span &a, const source_code_span &b) {
              return a.begin() < b.begin();
            });
  partial_shuffle(spans, rng, /*rounds=*/5);

  return source_code_with_spans(std::move(source), std::move(spans));
}

namespace {
template <class T>
void partial_shuffle(std::vector<T> &items, std::mt19937_64 &rng, int rounds) {
  std::uniform_int_distribution<std::size_t> index_distribution(
      0, items.size() - 1);
  using std::swap;
  for (int round = 0; round < rounds; ++round) {
    swap(items[index_distribution(rng)], items[index_distribution(rng)]);
  }
}
}
}
