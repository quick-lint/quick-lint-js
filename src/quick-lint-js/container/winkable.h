// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_WINKABLE_H
#define QUICK_LINT_JS_CONTAINER_WINKABLE_H

#include <type_traits>

namespace quick_lint_js {
// A type is winkable if the underlying memory for instances of the type can
// safely be deallocated without calling the object's destructor. (Memory leaks
// are considered "safe".)
//
// Example winkable types:
// * anything trivially destructible
// * container types like std::vector<U>, if U is winkable
template <class T>
struct is_winkable : std::is_trivially_destructible<T> {};

template <class T>
constexpr bool is_winkable_v = is_winkable<T>::value;
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
