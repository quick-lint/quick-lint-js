// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LOG_H
#define QUICK_LINT_JS_LOG_H

// This file is separate from logger.h to reduce #include bloat.

// To enable logging, change the QLJS_DEBUG_LOGGING_FILE macro in logger.cpp.

#define QLJS_DEBUG_LOG(...)                  \
  do {                                       \
    ::quick_lint_js::debug_log(__VA_ARGS__); \
  } while (false)

namespace quick_lint_js {
void debug_log(const char* format, ...);
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
