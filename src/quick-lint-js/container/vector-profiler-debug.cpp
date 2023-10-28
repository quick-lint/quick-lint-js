// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <iomanip>
#include <ostream>
#include <quick-lint-js/container/vector-profiler.h>

namespace quick_lint_js {
std::ostream &operator<<(std::ostream &out,
                         const Vector_Instrumentation::Entry &e) {
  out << "entry{.object_id = 0x" << std::hex << e.object_id << std::dec
      << ", .owner = \"" << e.owner << "\", .event = ";
  switch (e.event) {
  case Vector_Instrumentation::Event::append:
    out << "append";
    break;
  case Vector_Instrumentation::Event::assign:
    out << "assign";
    break;
  case Vector_Instrumentation::Event::clear:
    out << "clear";
    break;
  case Vector_Instrumentation::Event::create:
    out << "create";
    break;
  case Vector_Instrumentation::Event::destroy:
    out << "destroy";
    break;
  case Vector_Instrumentation::Event::resize:
    out << "resize";
    break;
  }
  out << ", .data_pointer = 0x" << std::hex << e.data_pointer << std::dec
      << ", .size = " << e.size << ", .capacity = " << e.capacity << "}";
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
