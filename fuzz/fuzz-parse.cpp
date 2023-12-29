// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/fe/null-visitor.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/port/char8.h>

extern "C" {
int LLVMFuzzerTestOneInput(const std::uint8_t *data, std::size_t size) {
  quick_lint_js::Padded_String source(quick_lint_js::String8(
      reinterpret_cast<const quick_lint_js::Char8 *>(data), size));
  quick_lint_js::Parser p(&source, &quick_lint_js::Null_Diag_Reporter::instance,
                          quick_lint_js::Parser_Options());
  quick_lint_js::Null_Visitor visitor;
  [[maybe_unused]] bool ok =
      p.parse_and_visit_module_catching_fatal_parse_errors(visitor);

  return 0;
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
