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
#include <quick-lint-js/location.h>
#include <string>

namespace quick_lint_js {
namespace {
void benchmark_location_scale_of_long_line(::benchmark::State &state) {
  std::size_t line_length = 10'000;
  std::string line(line_length, 'x');
  for (auto _ : state) {
    locator l(line.c_str());
    for (std::size_t i = 0; i < line_length; ++i) {
      source_position p = l.position(&line[i]);
      ::benchmark::DoNotOptimize(p);
    }
  }
}
BENCHMARK(benchmark_location_scale_of_long_line);

void benchmark_location_scale_of_empty_lines(::benchmark::State &state) {
  std::size_t line_length = 10'000;
  std::string line(line_length, '\n');
  for (auto _ : state) {
    locator l(line.c_str());
    for (std::size_t i = 0; i < line_length; ++i) {
      source_position p = l.position(&line[i]);
      ::benchmark::DoNotOptimize(p);
    }
  }
}
BENCHMARK(benchmark_location_scale_of_empty_lines);

void benchmark_range_scale_of_empty_lines(::benchmark::State &state) {
  std::size_t line_length = 10'000;
  std::size_t span_length = 5;
  std::string line(line_length, '\n');
  for (auto _ : state) {
    locator l(line.c_str());
    for (std::size_t i = 0; i < line_length - span_length; i += span_length) {
      source_code_span span(&line[i], &line[i + span_length]);
      source_range r = l.range(span);
      ::benchmark::DoNotOptimize(r);
    }
  }
}
BENCHMARK(benchmark_range_scale_of_empty_lines);
}  // namespace
}  // namespace quick_lint_js
