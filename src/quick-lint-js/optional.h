// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <optional>

namespace quick_lint_js {
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
