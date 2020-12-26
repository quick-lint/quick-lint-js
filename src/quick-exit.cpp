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

#include <cstdlib>
#include <quick-lint-js/have.h>
#include <quick-lint-js/quick-exit.h>

namespace quick_lint_js {
[[noreturn]] void quick_exit(int exit_code) noexcept {
#if QLJS_HAVE_STD_QUICK_EXIT
  std::quick_exit(exit_code);
#elif QLJS_HAVE_QUICK_EXIT
  ::quick_exit(exit_code);
#else
  std::_Exit(exit_code);
#endif
}
}
