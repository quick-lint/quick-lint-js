// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <cstddef>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/generate-code.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/cast.h>
#include <random>
#include <vector>

namespace quick_lint_js {
namespace {
String8_View sentence = u8"The quick brown fox jumps over the lazy dog.";

// Simulate opening a document and typing a sentence.
void benchmark_lsp_create_and_insert_single_characters(
    ::benchmark::State &state) {
  int line_count = 400;

  std::mt19937_64 rng;
  std::vector<int> line_lengths =
      random_line_lengths(rng, /*line_count=*/line_count);
  Padded_String base_code =
      make_source_code(/*line_lengths=*/line_lengths, /*newline=*/u8"\n");
  String8_View base_code_view = base_code.string_view();

  int insertion_line = narrow_cast<int>(line_lengths.size() / 2);
  int initial_insertion_character =
      line_lengths[narrow_cast<std::size_t>(insertion_line)] / 2;

  for (auto _ : state) {
    LSP_Document_Text doc;
    doc.set_text(base_code_view);

    for (int i = 0; i < narrow_cast<int>(sentence.size()); ++i) {
      int insertion_character = initial_insertion_character + i;
      doc.replace_text(
          LSP_Range{
              .start = {.line = insertion_line,
                        .character = insertion_character},
              .end = {.line = insertion_line, .character = insertion_character},
          },
          sentence.substr(narrow_cast<std::size_t>(i), 1));
      ::benchmark::DoNotOptimize(doc.string());
    }
  }
}
BENCHMARK(benchmark_lsp_create_and_insert_single_characters);
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
