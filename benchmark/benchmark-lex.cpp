// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
namespace {
void benchmark_lex(::benchmark::State &state, string8_view raw_source) {
  padded_string source(raw_source);
  for (auto _ : state) {
    lexer l(&source, &null_error_reporter::instance);
    while (l.peek().type != token_type::end_of_file) {
      l.skip();
    }
    ::benchmark::DoNotOptimize(l.peek().type);
  }
  double bytes_per_iteration = static_cast<double>(source.size() + 1);
  double iteration_count = static_cast<double>(state.iterations());
  state.counters["bytes"] = ::benchmark::Counter(
      bytes_per_iteration * iteration_count, ::benchmark::Counter::kIsRate);
  state.counters["byte"] = ::benchmark::Counter(
      bytes_per_iteration * iteration_count,
      ::benchmark::Counter::kIsRate | ::benchmark::Counter::kInvert);
}
BENCHMARK_CAPTURE(benchmark_lex, empty, u8""_sv);
BENCHMARK_CAPTURE(benchmark_lex, tiny_number, u8"0"_sv);
BENCHMARK_CAPTURE(benchmark_lex, small_number, u8"123"_sv);
BENCHMARK_CAPTURE(benchmark_lex, tiny_identifier, u8"x"_sv);
BENCHMARK_CAPTURE(benchmark_lex, tiny_identifiers_with_spaces,
                  u8"x x x x x x x x"_sv);
BENCHMARK_CAPTURE(benchmark_lex, tiny_identifiers_with_dots,
                  u8"x.x.x.x.x.x.x.x"_sv);
BENCHMARK_CAPTURE(benchmark_lex, 20_spaces, u8"                    "_sv);
BENCHMARK_CAPTURE(benchmark_lex, small_identifier, u8"pos"_sv);
BENCHMARK_CAPTURE(benchmark_lex, normal_identifier, u8"position"_sv);
BENCHMARK_CAPTURE(benchmark_lex, mixed_case_identifier, u8"XMLHttpRequest"_sv);
BENCHMARK_CAPTURE(benchmark_lex, long_identifier_1,
                  u8"reenterHydrationStateFromDehydratedSuspenseInstance"_sv);
BENCHMARK_CAPTURE(benchmark_lex, long_identifier_2,
                  u8"didWarnAboutGetSnapshotBeforeUpdateWithoutDidUpdate"_sv);
BENCHMARK_CAPTURE(benchmark_lex, jquery_snippet,
                  u8R"(/*!
 * Copyright JS Foundation and other contributors
 * Released under the MIT license
 * https://jquery.org/license
 *
 * Date: 2020-05-04T22:49Z
 */
function buildFragment( elems, context, scripts, selection, ignored ) {
	var elem, tmp, tag, wrap, attached, j,
		fragment = context.createDocumentFragment(),
		nodes = [],
		i = 0,
		l = elems.length;

	for ( ; i < l; i++ ) {
		elem = elems[ i ];

		if ( elem || elem === 0 ) {

			// Add nodes directly
			if ( toType( elem ) === "object" ) {
)"_sv);
}  // namespace
}  // namespace quick_lint_js

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
