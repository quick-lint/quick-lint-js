// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <memory>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/wasm-demo-error-reporter.h>
#include <quick-lint-js/wasm-demo.h>

namespace quick_lint_js {
const wasm_demo_error_reporter::error *
quick_lint_js_parse_and_lint_for_wasm_demo(const char8 *raw_input) {
  // TODO(strager): Allow null characters.
  padded_string input(string8_view{raw_input});
  std::unique_ptr<wasm_demo_error_reporter> error_reporter =
      std::make_unique<wasm_demo_error_reporter>(&input);
  parser p(&input, error_reporter.get());
  linter l(error_reporter.get());

  p.parse_and_visit_module(l);

  const wasm_demo_error_reporter::error *errors = error_reporter->get_errors();
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
