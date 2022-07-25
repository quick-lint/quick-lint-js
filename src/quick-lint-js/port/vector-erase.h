// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_VECTOR_ERASE_H
#define QUICK_LINT_JS_PORT_VECTOR_ERASE_H

#include <algorithm>

namespace quick_lint_js {
// Equivalent to std::erase (except with no return value).
template <class Vector>
void erase(Vector& v, const typename Vector::value_type& needle) {
  auto end = v.end();
  v.erase(std::remove(v.begin(), end, needle), end);
}

// Equivalent to std::erase_if (except with no return value).
template <class Vector, class Pred>
void erase_if(Vector& v, Pred&& pred) {
  auto end = v.end();
  v.erase(std::remove_if(v.begin(), end, pred), end);
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
