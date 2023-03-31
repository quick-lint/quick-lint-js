// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_NARROW_CAST_H
#define QUICK_LINT_JS_UTIL_NARROW_CAST_H

#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/in-range.h>
#include <quick-lint-js/port/source-location.h>

namespace quick_lint_js {
template <class Out, class In>
Out narrow_cast(In x
#if !(defined(NDEBUG) && NDEBUG)
                ,
                source_location caller = source_location::current()
#endif
                    ) noexcept {
#if !(defined(NDEBUG) && NDEBUG)
  if (!in_range<Out>(x)) {
    if constexpr (source_location::valid()) {
      report_assertion_failure(caller.file_name(),
                               static_cast<int>(caller.line()),
                               caller.function_name(), "number not in range");
    } else {
      report_assertion_failure(__FILE__, __LINE__, __func__,
                               "number not in range");
    }
    QLJS_ASSERT_TRAP();
  }
#endif
  return static_cast<Out>(x);
}
}

#endif

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
