// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_GENERATE_CODE_H
#define QUICK_LINT_JS_GENERATE_CODE_H

#include <memory>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <random>
#include <vector>

namespace quick_lint_js {
std::vector<int> random_line_lengths(std::mt19937_64 &, int line_count);
padded_string make_source_code(const std::vector<int> &line_lengths,
                               const string8 &newline);

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
}

#endif

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
