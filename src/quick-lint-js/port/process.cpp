// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/process.h>
#include <quick-lint-js/util/narrow-cast.h>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

namespace quick_lint_js {
#if !defined(__EMSCRIPTEN__)
std::uint64_t get_current_process_id() noexcept {
#if QLJS_HAVE_WINDOWS_H
  return ::GetCurrentProcessId();
#elif QLJS_HAVE_UNISTD_H
  return narrow_cast<std::uint64_t>(::getpid());
#else
#error "Unknown platform"
#endif
}
#endif
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
