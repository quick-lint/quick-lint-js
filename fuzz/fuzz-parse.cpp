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

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/null-visitor.h>
#include <quick-lint-js/parse.h>

namespace quick_lint_js {
namespace {
bool is_ascii(const std::uint8_t *data, std::size_t size) {
  for (std::size_t i = 0; i < size; ++i) {
    if ((data[i] & 0x80) != 0) {
      return false;
    }
  }
  return true;
}
}
}

extern "C" {
int LLVMFuzzerTestOneInput(const std::uint8_t *data, std::size_t size) {
  if (!quick_lint_js::is_ascii(data, size)) {
    // TODO(strager): Allow non-ASCII when the lexer handles UTF-8 correctly.
    return 0;
  }

  quick_lint_js::padded_string source(quick_lint_js::string8(
      reinterpret_cast<const quick_lint_js::char8 *>(data), size));
  quick_lint_js::parser p(&source,
                          &quick_lint_js::null_error_reporter::instance);
  quick_lint_js::null_visitor visitor;
  p.parse_and_visit_module(visitor);

  return 0;
}
}
