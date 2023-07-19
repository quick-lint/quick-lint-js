// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DEBUG_FIND_DEBUG_SERVER_H
#define QUICK_LINT_JS_DEBUG_FIND_DEBUG_SERVER_H

#include <cstdint>
#include <quick-lint-js/feature.h>
#include <vector>

namespace quick_lint_js {
#if QLJS_FEATURE_DEBUG_SERVER
// Announce the current thread as a debug server thread.
//
// After calling this function, this process will be findable later by the
// find_debug_servers function.
//
// A thread is automatically unregistered when it exits.
//
// See NOTE[find-debug-server] for implementation details.
void register_current_thread_as_debug_server_thread(std::uint16_t port_number);
#endif

struct Found_Debug_Server {
  std::uint64_t process_id;
  std::uint16_t port_number;
};

// Find running debug servers in all processes on the current machine.
//
// This function is best effort. It might fail to find some debug servers.
//
// This function is inherently racy. By the time this function returns, any
// number of returned debug servers might be dead.
//
// NOTE[find-debug-server] for implementation details.
std::vector<Found_Debug_Server> find_debug_servers();
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
