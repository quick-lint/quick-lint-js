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
#include <cstring>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>

namespace quick_lint_js {
namespace {
void benchmark_lex(::benchmark::State &state, const char *source) {
  for (auto _ : state) {
    lexer l(source, &null_error_reporter::instance);
    while (l.peek().type != token_type::end_of_file) {
      l.skip();
    }
    ::benchmark::DoNotOptimize(l.peek().type);
  }
  double bytes_per_iteration = static_cast<double>(std::strlen(source) + 1);
  double iteration_count = static_cast<double>(state.iterations());
  state.counters["bytes"] = ::benchmark::Counter(
      bytes_per_iteration * iteration_count, ::benchmark::Counter::kIsRate);
  state.counters["byte"] = ::benchmark::Counter(
      bytes_per_iteration * iteration_count,
      ::benchmark::Counter::kIsRate | ::benchmark::Counter::kInvert);
}
BENCHMARK_CAPTURE(benchmark_lex, empty, "");
BENCHMARK_CAPTURE(benchmark_lex, tiny_number, "0");
BENCHMARK_CAPTURE(benchmark_lex, small_number, "123");
BENCHMARK_CAPTURE(benchmark_lex, tiny_identifier, "x");
BENCHMARK_CAPTURE(benchmark_lex, tiny_identifiers_with_spaces,
                  "x x x x x x x x");
BENCHMARK_CAPTURE(benchmark_lex, tiny_identifiers_with_dots, "x.x.x.x.x.x.x.x");
BENCHMARK_CAPTURE(benchmark_lex, 20_spaces, "                    ");
BENCHMARK_CAPTURE(benchmark_lex, small_identifier, "pos");
BENCHMARK_CAPTURE(benchmark_lex, normal_identifier, "position");
BENCHMARK_CAPTURE(benchmark_lex, mixed_case_identifier, "XMLHttpRequest");
BENCHMARK_CAPTURE(benchmark_lex, long_identifier_1,
                  "reenterHydrationStateFromDehydratedSuspenseInstance");
BENCHMARK_CAPTURE(benchmark_lex, long_identifier_2,
                  "didWarnAboutGetSnapshotBeforeUpdateWithoutDidUpdate");
BENCHMARK_CAPTURE(benchmark_lex, jquery_snippet, R"(/*!
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
)");
}  // namespace
}  // namespace quick_lint_js
