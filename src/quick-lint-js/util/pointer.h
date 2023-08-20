// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>

namespace quick_lint_js {
inline bool is_aligned(void* p, std::size_t alignment) {
  std::size_t alignment_mask = alignment - 1;
  return (reinterpret_cast<std::uintptr_t>(p) & alignment_mask) == 0;
}

template <class Integer_Pointer>
Integer_Pointer align_up(Integer_Pointer p, std::size_t alignment) {
  Integer_Pointer alignment_mask = static_cast<Integer_Pointer>(alignment - 1);
  // TODO(strager): What about integer overflow?
  return ((p - 1) | alignment_mask) + 1;
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
