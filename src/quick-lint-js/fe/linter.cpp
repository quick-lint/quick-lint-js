// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/debug/debug-probe.h>
#include <quick-lint-js/fe/debug-parse-visitor.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/fe/multi-parse-visitor.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/io/output-stream.h>

namespace quick_lint_js {
void parse_and_lint(padded_string_view code, diag_reporter& reporter,
                    const global_declared_variable_set& globals,
                    linter_options options) {
  parser p(code, &reporter,
           parser_options{
               .jsx = options.jsx,
               .typescript = options.typescript,
           });
  variable_analyzer var_analyzer(
      &reporter, &globals,
      variable_analyzer_options{
          .allow_deleting_typescript_variable = !options.typescript,
          .eval_can_declare_variables = !options.typescript,
      });

#if defined(__EMSCRIPTEN__)
  // No file I/O on the web.
  QLJS_ALWAYS_ASSERT(!options.print_parser_visits);
  parse_visitor_base& v = var_analyzer;
#else
  debug_parse_visitor logger(file_output_stream::get_stderr());
  multi_parse_visitor logging_visitor(&logger, &var_analyzer);

  parse_visitor_base& v =
      options.print_parser_visits
          ? static_cast<parse_visitor_base&>(logging_visitor)
          : var_analyzer;
#endif

  bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
  if (!ok) {
    // TODO(strager): Should we do anything on failure? Should we show a
    // pop-up message for example? Or is the existing diagnostic enough?
  }

  debug_probe_publish_vector_profile();
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
