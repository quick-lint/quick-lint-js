// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
namespace {
void benchmark_lex(::benchmark::State &state, String8_View raw_source) {
  Padded_String source(raw_source);
  for (auto _ : state) {
    Lexer l(&source);
    while (l.peek().type != Token_Type::end_of_file) {
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

BENCHMARK_CAPTURE(benchmark_lex, short_block_comment, u8"/* hello */"_sv);
BENCHMARK_CAPTURE(benchmark_lex, long_block_comment,
                  u8"/*\n"
                  u8" * this is some text. this is some text.\n"
                  u8" * this is some text. this is some text.\n"
                  u8" * this is some text. this is some text.\n"
                  u8" */"_sv);
BENCHMARK_CAPTURE(
    benchmark_lex, star_banner_block_comment,
    u8"/**********************************************************\n"
    u8" * super cool section or module or something I don't know *\n"
    u8" **********************************************************/"_sv);

// Source: https://www.lipsum.com/
BENCHMARK_CAPTURE(
    benchmark_lex, many_line_comments,
    u8R"(// Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultrices
// sagittis ultrices. Donec quis risus ac est condimentum eleifend. Phasellus
// accumsan velit sit amet sapien aliquet, nec congue quam feugiat. Nam massa
// diam, viverra ac velit et, venenatis porttitor dui. Etiam lacinia neque quis
// mi vulputate, at facilisis tortor imperdiet. Pellentesque vulputate bibendum
// metus, ac viverra diam bibendum a. Praesent nec sodales tellus. In vel
// dignissim magna, id tincidunt nisi. Maecenas non congue arcu. Orci varius
// natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.
// Aliquam ut nisl id velit tempus suscipit non at ex. Phasellus ultrices
// pellentesque lacus, sed blandit ante semper sit amet. Fusce eget elit
// commodo, efficitur metus a, volutpat odio. Cras mattis sollicitudin est, nec
// vestibulum odio commodo nec.

// Nulla non placerat mauris. Cras efficitur scelerisque nisl, ac fringilla
// velit mattis at. In eu malesuada leo, eget commodo dui. Duis eu felis velit.
// In tristique, dolor quis dignissim elementum, orci enim vulputate mi, auctor
// auctor augue sapien nec tortor. Aenean augue risus, gravida et viverra non,
// semper non nisl. Nam porttitor arcu in nunc ornare dictum.

// Quisque id commodo justo. Nulla a congue urna. Morbi ac lorem ac nisi
// tincidunt auctor id a turpis. Suspendisse efficitur condimentum augue, vel
// porta metus elementum ac. Pellentesque quis elit et ex volutpat congue mattis
// sit amet lectus. Etiam vel diam nisi. Integer vitae lorem sollicitudin,
// viverra felis et, viverra lorem. Aliquam sollicitudin, lectus nec commodo
// elementum, ex velit cursus libero, id fermentum nisl est quis augue. Praesent
// consequat orci non dignissim rhoncus. Sed vel ultricies felis. Pellentesque
// facilisis semper condimentum. Nunc fermentum nulla a enim fringilla, sed
// hendrerit mauris molestie. In non venenatis diam, non vestibulum velit.
// Mauris vitae ipsum et purus aliquet porta. Pellentesque ante felis, ultrices
// et aliquet quis, fermentum eu lorem.

// Praesent ac quam imperdiet, vulputate augue non, pretium elit. Praesent a est
// dolor. Nullam vulputate consequat ornare. Etiam quis tortor egestas, posuere
// erat venenatis, blandit diam. Quisque porttitor placerat faucibus.
// Suspendisse id porta nulla. Aenean in gravida sapien, nec pulvinar ipsum.
// Aliquam in eros elementum nisl tempor hendrerit. Duis vitae pharetra libero.
// Nullam blandit justo quam, ac pretium leo varius in. Mauris eget ligula
// sagittis, lobortis neque ut, molestie quam. Nulla tempus et sapien eu
// porttitor. Nulla suscipit vel ligula sed facilisis. Integer dictum enim id
// risus sagittis imperdiet. Suspendisse potenti. Nullam felis libero, fermentum
// vitae erat a, tempus varius eros.

// Donec scelerisque posuere fermentum. Maecenas non congue tortor. Mauris
// ornare tempus metus, accumsan tempor est finibus eu. Nullam pretium cursus
// orci quis aliquet. In non purus congue, volutpat leo ut, hendrerit justo.
// Nunc at lacus id sapien egestas eleifend eu sed turpis. Ut diam felis, mollis
// quis dolor et, placerat rhoncus tortor. Vestibulum laoreet eget felis id
// lacinia. Ut at augue eu nisi consectetur volutpat sit amet ut ex. Nullam a
// eleifend nibh. Duis sed dictum nunc.

// Proin sed elit eget diam varius euismod. Sed eget purus eget sapien feugiat
// viverra. Mauris vulputate augue in consectetur mattis. Phasellus sed dolor ut
// neque tincidunt bibendum nec nec est. Nulla viverra egestas augue sed
// dignissim. Donec viverra justo vel nibh pellentesque interdum. Vestibulum at
// faucibus libero.

// Praesent lobortis tempus erat, at varius sem aliquam id. Aliquam urna diam,
// viverra non mauris sed, porttitor bibendum neque. Morbi quis euismod dui, ut
// vulputate libero. Orci varius natoque penatibus et magnis dis parturient
// montes, nascetur ridiculus mus. Donec pellentesque ligula eu eros hendrerit
// molestie. Aliquam fringilla elit at magna pretium, at porttitor sem
// efficitur. Pellentesque a varius neque. Phasellus enim libero, viverra eget
// euismod viverra, pharetra eget leo. Integer risus mauris, varius eu semper
// sed, euismod sit amet ante. Praesent ac purus quis neque pharetra pharetra a
// vitae urna. Ut ac turpis erat. Nullam ut efficitur ante, vitae pharetra nisl.

// Sed ornare velit turpis, nec cursus eros lobortis a. Pellentesque ac
// ullamcorper nibh. Fusce accumsan placerat ullamcorper. Nullam ut libero id
// leo bibendum sagittis vel eget est. Phasellus aliquam ante ut sem tempus
// pretium. Praesent tempor, risus non sodales blandit, nunc arcu molestie
// augue, at eleifend tellus enim ac erat. Vestibulum eget tellus elementum,
// dignissim urna et, tincidunt eros. In massa tellus, fermentum ac libero eu,
// semper dapibus massa. Maecenas felis magna, vestibulum id justo ut, porta
// lacinia libero. Sed ac tristique lacus, at hendrerit urna. Phasellus sit amet
// consectetur justo. Fusce sit amet porta purus.

// Ut scelerisque condimentum ipsum nec interdum. Fusce consectetur, tortor
// rhoncus volutpat laoreet, ipsum ipsum vestibulum augue, eu congue ante urna
// in velit. Nulla aliquet dui ac erat efficitur congue. Aliquam ullamcorper
// urna ante. Quisque ipsum nunc, egestas in suscipit id, accumsan ac sem. Donec
// sed suscipit ex, in suscipit velit. Mauris scelerisque libero a metus
// eleifend ornare.

// Nunc pulvinar iaculis vulputate. Sed eget neque ante. Quisque nec justo nec
// augue ultricies condimentum. Nulla gravida augue sapien, in auctor purus
// sollicitudin eget. Sed fermentum vehicula sollicitudin. Duis ac turpis id
// ipsum consequat volutpat. Sed ac tristique diam. Ut mattis sem nunc.
// Suspendisse et venenatis massa. Mauris volutpat vulputate dictum. Vestibulum
// eget purus at leo auctor iaculis. Nullam ultricies tellus in magna tincidunt
// lobortis.
)"_sv);

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
