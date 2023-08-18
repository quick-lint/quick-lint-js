// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_GENERATE_CODE_H
#define QUICK_LINT_JS_GENERATE_CODE_H

#include <memory>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <random>
#include <vector>

namespace quick_lint_js {
std::vector<int> random_line_lengths(std::mt19937_64 &, int line_count);
Padded_String make_source_code(const std::vector<int> &line_lengths,
                               const String8 &newline);

struct Source_Code_With_Spans {
  explicit Source_Code_With_Spans(std::unique_ptr<Padded_String> &&source,
                                  std::vector<Source_Code_Span> &&spans)
      : source(std::move(source)), spans(std::move(spans)) {}

  Source_Code_With_Spans(const Source_Code_With_Spans &) = delete;
  Source_Code_With_Spans &operator=(const Source_Code_With_Spans &) = delete;

  std::unique_ptr<Padded_String> source;
  std::vector<Source_Code_Span> spans;
};

Source_Code_With_Spans make_realisticish_code(int line_count, int span_count);
}

#endif

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
