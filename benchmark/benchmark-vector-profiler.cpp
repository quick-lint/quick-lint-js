// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <cstddef>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/container/vector.h>

namespace quick_lint_js {
namespace {
// clang-format off
vector_instrumentation::entry sample_entries[] = {
  {.object_id = 0x16f018c68, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f018c90, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f018c68, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300da30,  .size = 1, .capacity = 4},
  {.object_id = 0x16f018c90, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f018c68, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300da30, .size = 1, .capacity = 4},
  {.object_id = 0x16f018c68, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f018c90, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f018c68, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300da70,  .size = 1, .capacity = 4},
  {.object_id = 0x16f018c90, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f018c68, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300da70, .size = 1, .capacity = 4},
  {.object_id = 0x16f018518, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f018540, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f018518, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300daa8,  .size = 1, .capacity = 4},
  {.object_id = 0x16f018540, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f018518, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300daa8, .size = 1, .capacity = 4},
  {.object_id = 0x16f017bc8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f017bf0, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f017bc8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dab0,  .size = 1, .capacity = 4},
  {.object_id = 0x16f017bf0, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f017bc8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300dab0, .size = 1, .capacity = 4},
  {.object_id = 0x16f018448, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f018470, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f018448, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dae8,  .size = 1, .capacity = 4},
  {.object_id = 0x16f018470, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300db20,  .size = 1, .capacity = 4},
  {.object_id = 0x16f018448, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dae8,  .size = 2, .capacity = 4},
  {.object_id = 0x16f017158, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f017180, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f017158, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300db80,  .size = 1, .capacity = 4},
  {.object_id = 0x16f017180, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f017158, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300db80, .size = 1, .capacity = 4},
  {.object_id = 0x16f018470, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300db20,  .size = 2, .capacity = 4},
  {.object_id = 0x16f018448, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dae8,  .size = 3, .capacity = 4},
  {.object_id = 0x16f018470, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300db20,  .size = 3, .capacity = 4},
  {.object_id = 0x16f018448, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dae8,  .size = 4, .capacity = 4},
  {.object_id = 0x16f018470, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f018448, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f0173c8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f0173f0, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f0173c8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dc38,  .size = 1, .capacity = 4},
  {.object_id = 0x16f016958, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f016980, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f016958, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dca0,  .size = 1, .capacity = 4},
  {.object_id = 0x16f015ee8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f015f10, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f015ee8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dd08,  .size = 1, .capacity = 4},
  {.object_id = 0x16f015480, .owner = "parse_expression_remainder call children", .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f015480, .owner = "parse_expression_remainder call children", .event = vector_instrumentation::event::append, .data_pointer = 0x12300dd28,  .size = 1, .capacity = 4},
  {.object_id = 0x16f015278, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f0152a0, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::create, .data_pointer = 0x0,          .size = 0, .capacity = 0},
  {.object_id = 0x16f015278, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::append, .data_pointer = 0x12300dd68,  .size = 1, .capacity = 4},
  {.object_id = 0x16f0173f0, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f0173c8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300dc38, .size = 1, .capacity = 4},
  {.object_id = 0x16f016980, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f016958, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300dca0, .size = 1, .capacity = 4},
  {.object_id = 0x16f015f10, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f015ee8, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300dd08, .size = 1, .capacity = 4},
  {.object_id = 0x16f015480, .owner = "parse_expression_remainder call children", .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300dd28, .size = 1, .capacity = 4},
  {.object_id = 0x16f0152a0, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x0,         .size = 0, .capacity = 0},
  {.object_id = 0x16f015278, .owner = "binary_expression_builder children",       .event = vector_instrumentation::event::destroy, .data_pointer = 0x12300dd68, .size = 1, .capacity = 4},
};
// clang-format on

void benchmark_add_entries_from_array(::benchmark::State& state,
                                      std::size_t entry_count) {
  for (auto _ : state) {
    vector_instrumentation profiler;
    for (std::size_t i = 0; i < entry_count; ++i) {
      const vector_instrumentation::entry& entry =
          sample_entries[i % std::size(sample_entries)];
      profiler.add_entry(entry.object_id, entry.owner, entry.event,
                         entry.data_pointer, entry.size, entry.capacity);
    }
    ::benchmark::DoNotOptimize(profiler);
  }

  state.counters["entries"] = ::benchmark::Counter(
      static_cast<double>(entry_count * state.iterations()),
      ::benchmark::Counter::kIsRate);
}
BENCHMARK_CAPTURE(benchmark_add_entries_from_array, none, 0);
BENCHMARK_CAPTURE(benchmark_add_entries_from_array, few, 20);
BENCHMARK_CAPTURE(benchmark_add_entries_from_array, tons, 50'000);

void benchmark_max_size_histogram_by_owner(::benchmark::State& state,
                                           std::size_t entry_count) {
  vector_instrumentation profiler;
  for (std::size_t i = 0; i < entry_count; ++i) {
    const vector_instrumentation::entry& entry =
        sample_entries[i % std::size(sample_entries)];
    profiler.add_entry(entry.object_id, entry.owner, entry.event,
                       entry.data_pointer, entry.size, entry.capacity);
  }
  std::vector<vector_instrumentation::entry> entries = profiler.entries();

  for (auto _ : state) {
    vector_max_size_histogram_by_owner histogram;
    histogram.add_entries(entries);
    auto hist = histogram.histogram();
    ::benchmark::DoNotOptimize(hist);
  }

  state.counters["entries"] = ::benchmark::Counter(
      static_cast<double>(entry_count * state.iterations()),
      ::benchmark::Counter::kIsRate);
}
BENCHMARK_CAPTURE(benchmark_max_size_histogram_by_owner, none, 0);
BENCHMARK_CAPTURE(benchmark_max_size_histogram_by_owner, few, 20);
BENCHMARK_CAPTURE(benchmark_max_size_histogram_by_owner, tons, 50'000);
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
