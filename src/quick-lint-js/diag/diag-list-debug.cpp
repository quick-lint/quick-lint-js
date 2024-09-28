// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/diag/diag-list.h>

namespace quick_lint_js {
std::ostream& operator<<(std::ostream& out, const Diag_List& diags) {
  int diag_count = 0;
  diags.for_each([&](Diag_Type, const void*) -> void { diag_count += 1; });
  out << diag_count << " " << (diag_count == 1 ? "diagnostic" : "diagnostics");
  if (diag_count > 0) {
    out << ": {\n";
    diags.for_each([&](Diag_Type type, const void*) -> void {
      out << "  " << type << ",\n";
    });
    out << "}";
  }
  return out;
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
