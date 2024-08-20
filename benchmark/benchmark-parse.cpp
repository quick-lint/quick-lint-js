// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/null-visitor.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/warning.h>
#include <string>

QLJS_WARNING_IGNORE_MSVC(4996)  // Function or variable may be unsafe.

namespace quick_lint_js {
namespace {
#if !defined(__EMSCRIPTEN__)  // TODO(#800): Support Emscripten.
void benchmark_parse_file(benchmark::State &state) {
  const char *source_path_env_var = "QLJS_PARSE_BENCHMARK_SOURCE_FILE";
  const char *source_path = std::getenv(source_path_env_var);
  if (!source_path || *source_path == '\0') {
    std::fprintf(stderr,
                 "fatal: The %s environment variable was not set.\n"
                 "       Set it to the path of a JavaScript source file.\n",
                 source_path_env_var);
    std::exit(1);
  }
  Padded_String source = quick_lint_js::read_file_or_exit(source_path);

  Parser_Options p_options;
  for (auto _ : state) {
    Parser p(&source, p_options);
    Null_Visitor visitor;
    p.parse_and_visit_module(visitor);
  }
}
BENCHMARK(benchmark_parse_file);
#endif

void benchmark_parse(benchmark::State &state, String8_View raw_source) {
  Padded_String source(raw_source);
  Parser_Options p_options;
  for (auto _ : state) {
    Parser p(&source, p_options);
    Null_Visitor visitor;
    p.parse_and_visit_module(visitor);
  }
}
BENCHMARK_CAPTURE(benchmark_parse, pathological_await_slash_in_function,
                  u8R"(
function f() {
  await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
  ()=>{{{{{{{await/
}
)"_sv);
BENCHMARK_CAPTURE(benchmark_parse, pathological_await_slash_top_level,
                  u8R"(
await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
()=>{{{{{{{await/
)"_sv);
BENCHMARK_CAPTURE(benchmark_parse, pathological_await_arrow,
                  u8R"(
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
await a =>
a
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
