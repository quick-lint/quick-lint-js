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

#include <benchmark/benchmark.h>
#include <memory>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/generate-code.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <random>
#include <set>
#include <vector>

// TODO(strager): Also benchmark lsp_locator.
// TODO(strager): Also benchmark vim_locator.

namespace quick_lint_js {
namespace {
struct source_code_with_spans {
  explicit source_code_with_spans(std::unique_ptr<padded_string> &&source,
                                  std::vector<source_code_span> &&spans)
      : source(std::move(source)), spans(std::move(spans)) {}

  source_code_with_spans(const source_code_with_spans &) = delete;
  source_code_with_spans &operator=(const source_code_with_spans &) = delete;

  std::unique_ptr<padded_string> source;
  std::vector<source_code_span> spans;
};

source_code_with_spans make_realisticish_code(int line_count, int span_count);
template <class T>
void partial_shuffle(std::vector<T> &, std::mt19937_64 &, int rounds);

void benchmark_location_scale_of_long_line(::benchmark::State &state) {
  int line_length = 10'000;
  padded_string line(string8(narrow_cast<std::size_t>(line_length), u8'x'));
  for (auto _ : state) {
    locator l(&line);
    for (int i = 0; i < line_length; ++i) {
      source_position p = l.position(&line[i]);
      ::benchmark::DoNotOptimize(p);
    }
  }
}
BENCHMARK(benchmark_location_scale_of_long_line);

void benchmark_location_scale_of_empty_lines(::benchmark::State &state) {
  int line_length = 10'000;
  padded_string line(string8(narrow_cast<std::size_t>(line_length), u8'\n'));
  for (auto _ : state) {
    locator l(&line);
    for (int i = 0; i < line_length; ++i) {
      source_position p = l.position(&line[i]);
      ::benchmark::DoNotOptimize(p);
    }
  }
}
BENCHMARK(benchmark_location_scale_of_empty_lines);

void benchmark_range_scale_of_empty_lines(::benchmark::State &state) {
  int line_length = 10'000;
  int span_length = 5;
  padded_string line(string8(narrow_cast<std::size_t>(line_length), u8'\n'));
  for (auto _ : state) {
    locator l(&line);
    for (int i = 0; i < line_length - span_length; i += span_length) {
      source_code_span span(&line[i], &line[i + span_length]);
      source_range r = l.range(span);
      ::benchmark::DoNotOptimize(r);
    }
  }
}
BENCHMARK(benchmark_range_scale_of_empty_lines);

void benchmark_location_realisticish(::benchmark::State &state) {
  int line_count = 10'000;
  int span_count = narrow_cast<int>(state.range(0));
  source_code_with_spans code = make_realisticish_code(
      /*line_count=*/line_count, /*span_count=*/span_count);

  for (auto _ : state) {
    locator l(code.source.get());
    for (const source_code_span &span : code.spans) {
      source_range r = l.range(span);
      ::benchmark::DoNotOptimize(r);
    }
  }
}
BENCHMARK(benchmark_location_realisticish)->Arg(1)->Arg(50);

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

template <class T>
void partial_shuffle(std::vector<T> &items, std::mt19937_64 &rng, int rounds) {
  std::uniform_int_distribution<std::size_t> index_distribution(
      0, items.size() - 1);
  using std::swap;
  for (int round = 0; round < rounds; ++round) {
    swap(items[index_distribution(rng)], items[index_distribution(rng)]);
  }
}
}  // namespace
}  // namespace quick_lint_js
