// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <memory>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/generate-code.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cast.h>
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

Padded_String make_source_code(const std::vector<int> &line_lengths,
                               const String8 &newline) {
  String8 source;
  for (int line_length : line_lengths) {
    source += String8(narrow_cast<std::size_t>(line_length), u8'x');
    source += newline;
  }
  return Padded_String(std::move(source));
}

Source_Code_With_Spans make_realisticish_code(int line_count, int span_count) {
  std::mt19937_64 rng;

  String8 newline = u8"\n";
  std::vector<int> line_lengths = random_line_lengths(rng, line_count);
  std::unique_ptr<Padded_String> source =
      std::make_unique<Padded_String>(make_source_code(line_lengths, newline));

  std::uniform_int_distribution span_length_distribution(1, 10);
  auto random_span_in_line = [&](int line_length,
                                 int line_begin_offset) -> Source_Code_Span {
    int span_length = std::min(span_length_distribution(rng), line_length);
    int span_begin_line_offset =
        std::uniform_int_distribution(0, line_length - span_length)(rng);
    const Char8 *span_begin =
        &(*source)[line_begin_offset + span_begin_line_offset];
    return Source_Code_Span(span_begin, span_begin + span_length);
  };

  std::uniform_int_distribution span_line_number_distribution(1,
                                                              line_count + 1);
  std::multiset<int> span_line_numbers;
  for (int i = 0; i < span_count; ++i) {
    span_line_numbers.insert(span_line_number_distribution(rng));
  }

  std::vector<Source_Code_Span> spans;
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

  sort(spans, [](const Source_Code_Span &a, const Source_Code_Span &b) {
    return a.begin() < b.begin();
  });
  partial_shuffle(spans, rng, /*rounds=*/5);

  return Source_Code_With_Spans(std::move(source), std::move(spans));
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
