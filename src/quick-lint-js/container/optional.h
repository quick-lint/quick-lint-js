// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_OPTIONAL_H
#define QUICK_LINT_JS_CONTAINER_OPTIONAL_H

#include <optional>

namespace quick_lint_js {
template <class T>
T *get(std::optional<T> &o) noexcept {
  return o.has_value() ? &*o : nullptr;
}

template <class T>
const T *get(const std::optional<T> &o) noexcept {
  return o.has_value() ? &*o : nullptr;
}

// Prevent dangling pointers. For example:
//
//   std::optional<int> get_thing();
//   int *x = get(get_thing());  // ERROR
template <class T>
T *get(std::optional<T> &&) noexcept = delete;
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
