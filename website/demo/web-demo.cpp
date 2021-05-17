// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <memory>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/web-demo-error-reporter.h>
#include <quick-lint-js/web-demo.h>

namespace quick_lint_js {
const web_demo_error_reporter::error *quick_lint_js_parse_and_lint_for_web_demo(
    const char8 *raw_input) {
  // TODO(strager): Allow null characters.
  padded_string input(string8_view{raw_input});
  std::unique_ptr<web_demo_error_reporter> error_reporter =
      std::make_unique<web_demo_error_reporter>(&input);
  parser p(&input, error_reporter.get());
  linter l(error_reporter.get());

  // TODO(strager): Use parse_and_visit_module_catching_unimplemented instead of
  // parse_and_visit_module to avoid crashing on QLJS_PARSER_UNIMPLEMENTED.
  p.parse_and_visit_module(l);

  const web_demo_error_reporter::error *errors = error_reporter->get_errors();
  // TODO(strager): Avoid memory leak somehow.
  error_reporter.release();
  return errors;
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
