// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/diag/diag-list-diag-reporter.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>

namespace quick_lint_js {
Diag_List_Diag_Reporter::Diag_List_Diag_Reporter(Memory_Resource* memory)
    : diags_(memory) {}

Diag_List_Diag_Reporter::~Diag_List_Diag_Reporter() = default;

Diag_List& Diag_List_Diag_Reporter::diags() { return this->diags_; }

const Diag_List& Diag_List_Diag_Reporter::diags() const { return this->diags_; }

void Diag_List_Diag_Reporter::report_impl(Diag_Type type, void* diag) {
  static constexpr unsigned char diag_sizes[] = {
#define QLJS_DIAG_TYPE_NAME(name) sizeof(::quick_lint_js::name),
      QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME
  };
  this->diags_.add_impl(type, diag,
                        diag_sizes[static_cast<std::ptrdiff_t>(type)]);
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
