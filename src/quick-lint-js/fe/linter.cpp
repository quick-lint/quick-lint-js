// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/configuration/configuration.h>
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
#include <quick-lint-js/port/unreachable.h>

namespace quick_lint_js {
void parse_and_lint(Padded_String_View code, Diag_Reporter& reporter,
                    Linter_Options options) {
  Parser_Options parser_options;
  parser_options.jsx_mode = options.configuration->jsx_mode;
  switch (options.language) {
  case File_Language::javascript:
    parser_options.jsx = false;
    parser_options.typescript = false;
    break;
  case File_Language::javascript_jsx:
    parser_options.jsx = true;
    parser_options.typescript = false;
    break;
  case File_Language::typescript:
    parser_options.jsx = false;
    parser_options.typescript = true;
    break;
  case File_Language::typescript_definition:
    parser_options.jsx = false;
    parser_options.typescript = true;
    parser_options.typescript_definition_file = true;
    break;
  case File_Language::typescript_jsx:
    parser_options.jsx = true;
    parser_options.typescript = true;
    break;
  }

  Parser p(code, &reporter, parser_options);
  Variable_Analyzer var_analyzer(
      &reporter, &options.configuration->globals(),
      Variable_Analyzer_Options{
          .allow_deleting_typescript_variable = !parser_options.typescript,
          .eval_can_declare_variables = !parser_options.typescript,
          // TODO(strager): Deduplicate with typescript_var_options in tests.
          .can_assign_to_class = !parser_options.typescript,
          .import_variable_can_be_runtime_or_type = parser_options.typescript,
      });

#if defined(__EMSCRIPTEN__)
  // No file I/O on the web.
  QLJS_ALWAYS_ASSERT(!options.print_parser_visits);
  Parse_Visitor_Base& v = var_analyzer;
#else
  Debug_Parse_Visitor logger(File_Output_Stream::get_stderr());
  Multi_Parse_Visitor logging_visitor(&logger, &var_analyzer);

  Parse_Visitor_Base& v =
      options.print_parser_visits
          ? static_cast<Parse_Visitor_Base&>(logging_visitor)
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
