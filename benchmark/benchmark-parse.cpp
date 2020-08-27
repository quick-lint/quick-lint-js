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
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/error.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <string>

namespace quick_lint_js {
namespace {
class null_visitor {
 public:
  void visit_end_of_module() {}

  void visit_enter_block_scope() {}

  void visit_enter_class_scope() {}

  void visit_enter_for_scope() {}

  void visit_enter_function_scope() {}

  void visit_enter_named_function_scope(identifier) {}

  void visit_exit_block_scope() {}

  void visit_exit_class_scope() {}

  void visit_exit_for_scope() {}

  void visit_exit_function_scope() {}

  void visit_property_declaration(identifier) {}

  void visit_variable_assignment(identifier) {}

  void visit_variable_declaration(identifier, variable_kind) {}

  void visit_variable_use(identifier) {}
};

void benchmark_parse(benchmark::State &state) {
  const char *source_path_env_var = "QLJS_PARSE_BENCHMARK_SOURCE_FILE";
  const char *source_path = std::getenv(source_path_env_var);
  if (!source_path || *source_path == '\0') {
    std::fprintf(stderr,
                 "fatal: The %s environment variable was not set.\n"
                 "       Set it to the path of a JavaScript source file.\n",
                 source_path_env_var);
    std::exit(1);
  }
  padded_string source(quick_lint_js::read_file("jquery-3.5.1.js"));

  for (auto _ : state) {
    parser p(&source, &null_error_reporter::instance);
    null_visitor visitor;
    p.parse_and_visit_module(visitor);
  }
}
BENCHMARK(benchmark_parse);
}  // namespace
}  // namespace quick_lint_js
