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
#include <cstddef>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/generate-code.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lsp-document.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <random>
#include <vector>

namespace quick_lint_js {
namespace {
string8_view sentence = u8"The quick brown fox jumps over the lazy dog.";

// Simulate opening a document and typing a sentence.
void benchmark_create_and_insert_single_characters(::benchmark::State &state) {
  int line_count = 400;

  std::mt19937_64 rng;
  std::vector<int> line_lengths =
      random_line_lengths(rng, /*line_count=*/line_count);
  padded_string base_code =
      make_source_code(/*line_lengths=*/line_lengths, /*newline=*/u8"\n");
  string8_view base_code_view = base_code.string_view();

  int insertion_line = narrow_cast<int>(line_lengths.size() / 2);
  int initial_insertion_character =
      line_lengths[narrow_cast<std::size_t>(insertion_line)] / 2;

  for (auto _ : state) {
    lsp_document document;
    document.set_text(base_code_view);

    for (int i = 0; i < narrow_cast<int>(sentence.size()); ++i) {
      int insertion_character = initial_insertion_character + i;
      document.replace_text(
          lsp_range{
              .start = {.line = insertion_line,
                        .character = insertion_character},
              .end = {.line = insertion_line, .character = insertion_character},
          },
          sentence.substr(narrow_cast<std::size_t>(i), 1));
      ::benchmark::DoNotOptimize(document.string());
    }
  }
}
BENCHMARK(benchmark_create_and_insert_single_characters);
}  // namespace
}  // namespace quick_lint_js
