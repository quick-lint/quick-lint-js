// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <boost/container/pmr/global_resource.hpp>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/warning.h>
#include <string>

QLJS_WARNING_IGNORE_MSVC(4996)  // Function or variable may be unsafe.

using boost::container::pmr::new_delete_resource;

namespace quick_lint_js {
namespace {
void benchmark_lint(benchmark::State &state) {
  const char *source_path_env_var = "QLJS_LINT_BENCHMARK_SOURCE_FILE";
  const char *source_path = std::getenv(source_path_env_var);
  if (!source_path || *source_path == '\0') {
    std::fprintf(stderr,
                 "fatal: The %s environment variable was not set.\n"
                 "       Set it to the path of a JavaScript source file.\n",
                 source_path_env_var);
    std::exit(1);
  }
  padded_string source = quick_lint_js::read_file_or_exit(source_path);

  configuration config;
  parser p(&source, &null_error_reporter::instance);
  buffering_visitor visitor(new_delete_resource());
  p.parse_and_visit_module(visitor);

  for (auto _ : state) {
    linter l(&null_error_reporter::instance, &config.globals());
    visitor.copy_into(l);
  }
}
BENCHMARK(benchmark_lint);

void benchmark_parse_and_lint(benchmark::State &state) {
  const char *source_path_env_var = "QLJS_LINT_BENCHMARK_SOURCE_FILE";
  const char *source_path = std::getenv(source_path_env_var);
  if (!source_path || *source_path == '\0') {
    std::fprintf(stderr,
                 "fatal: The %s environment variable was not set.\n"
                 "       Set it to the path of a JavaScript source file.\n",
                 source_path_env_var);
    std::exit(1);
  }
  padded_string source = quick_lint_js::read_file_or_exit(source_path);

  configuration config;
  for (auto _ : state) {
    parser p(&source, &null_error_reporter::instance);
    linter l(&null_error_reporter::instance, &config.globals());
    p.parse_and_visit_module(l);
  }
}
BENCHMARK(benchmark_parse_and_lint);

void benchmark_undeclared_variable_references(benchmark::State &state) {
  int global_variable_count = 1000;
  int variable_use_count = 1000;

  global_declared_variable_set globals;
  for (int i = 0; i < global_variable_count; ++i) {
    globals.add_predefined_global_variable(
        to_string8("global" + std::to_string(i)).c_str(), /*is_writable=*/true);
  }

  std::vector<std::pair<std::size_t, std::size_t>> variable_use_ranges;
  string8 variable_uses;
  for (int i = 0; i < variable_use_count; ++i) {
    // NOTE(strager): The implementation might short circuit based on identifier
    // length. Use the same prefix so more work needs to be done.
    string8 variable_name = to_string8("usage_" + std::to_string(i));
    std::size_t use_begin = variable_uses.size();
    variable_uses += variable_name;
    std::size_t use_end = variable_uses.size();
    variable_use_ranges.emplace_back(use_begin, use_end);
  }

  std::vector<identifier> variable_use_identifiers;
  for (auto [begin_index, end_index] : variable_use_ranges) {
    const char8 *begin = &variable_uses[begin_index];
    const char8 *end = &variable_uses[end_index];
    variable_use_identifiers.emplace_back(
        source_code_span(begin, end),
        string8_view(begin, end_index - begin_index));
  }

  for (auto _ : state) {
    linter l(&null_error_reporter::instance, &globals);
    for (identifier &variable_use : variable_use_identifiers) {
      l.visit_variable_use(variable_use);
    }
    l.visit_end_of_module();
  }
}
BENCHMARK(benchmark_undeclared_variable_references);
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
