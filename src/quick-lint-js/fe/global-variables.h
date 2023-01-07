// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_GLOBAL_VARIABLES_H
#define QUICK_LINT_JS_FE_GLOBAL_VARIABLES_H

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
inline constexpr std::size_t global_group_count = 10;

struct global_group {
  const char8 *name;
  const char8 *globals;
  const char8 *non_writable_globals;
  const char8 *non_shadowable_globals;
  std::int16_t globals_count;
  std::int16_t non_writable_globals_count;
  std::int16_t non_shadowable_globals_count;
};
extern const global_group global_groups[];

extern const char8 global_variables_browser[];
extern const char8 global_variables_web_worker[];
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
