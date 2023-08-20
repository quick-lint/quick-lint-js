// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <quick-lint-js/io/event-loop-kqueue.h>
#include <quick-lint-js/io/event-loop-poll.h>
#include <quick-lint-js/io/event-loop-win32.h>
#include <quick-lint-js/port/have.h>

namespace quick_lint_js {
template <class Derived>
using Event_Loop =
#if QLJS_HAVE_KQUEUE
    Kqueue_Event_Loop
#elif QLJS_HAVE_POLL
    Poll_Event_Loop
#elif defined(_WIN32)
    Windows_Event_Loop
#else
#error "Unknown platform"
#endif
    <Derived>;
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
