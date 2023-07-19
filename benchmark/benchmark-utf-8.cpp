// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/utf-8.h>

namespace quick_lint_js {
namespace {
String8 repeat(String8_View s, int count) {
  String8 result;
  for (int i = 0; i < count; ++i) {
    result += s;
  }
  return result;
}

void benchmark_advance_lsp_characters_in_utf_8(::benchmark::State& state,
                                               String8_View input) {
  Padded_String padded_input(input);
  int total_character_count = narrow_cast<int>(count_lsp_characters_in_utf_8(
      &padded_input, narrow_cast<int>(padded_input.size())));
  // Avoid count==size optimizations:
  int characters_to_count = total_character_count - 1;

  for (auto _ : state) {
    const Char8* end =
        advance_lsp_characters_in_utf_8(input, characters_to_count);
    ::benchmark::DoNotOptimize(end);
  }

  double bytes_per_iteration = static_cast<double>(input.size());
  double characters_per_iteration = static_cast<double>(characters_to_count);
  double iteration_count = static_cast<double>(state.iterations());
  state.counters["bytes"] = ::benchmark::Counter(
      bytes_per_iteration * iteration_count, ::benchmark::Counter::kIsRate);
  state.counters["characters"] =
      ::benchmark::Counter(characters_per_iteration * iteration_count,
                           ::benchmark::Counter::kIsRate);
}
BENCHMARK_CAPTURE(benchmark_advance_lsp_characters_in_utf_8, tiny_ascii,
                  u8"a"_sv);
BENCHMARK_CAPTURE(benchmark_advance_lsp_characters_in_utf_8, small_ascii,
                  u8"hello"_sv);
BENCHMARK_CAPTURE(benchmark_advance_lsp_characters_in_utf_8, large_ascii,
                  String8(4096, u8'x'));
BENCHMARK_CAPTURE(benchmark_advance_lsp_characters_in_utf_8, small_japanese,
                  u8"こんにちは"_sv);
BENCHMARK_CAPTURE(benchmark_advance_lsp_characters_in_utf_8, large_japanese,
                  repeat(u8"こんにちは"_sv, 1024));
// TODO(strager): Mixed Japanese and ASCII.
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
