// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <quick-lint-js/have.h>
#include <quick-lint-js/log.h>
#include <quick-lint-js/warning.h>

#if QLJS_HAVE_GETTID
#include <sys/types.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
#if defined(QLJS_DEBUG_LOGGING_FILE)
template <class... Args>
void debug_log_to_file(const char* format, Args&&... args) {
  static FILE* file = std::fopen(QLJS_DEBUG_LOGGING_FILE, "a");
  if (file) {
#if QLJS_HAVE_GETPID
#if QLJS_HAVE_GETTID
    std::fprintf(file, "[%d.%d] ", ::getpid(), ::gettid());
#else
    std::fprintf(file, "[%d] ", ::getpid());
#endif
#endif
    QLJS_WARNING_PUSH
    QLJS_WARNING_IGNORE_CLANG("-Wformat-security")
    QLJS_WARNING_IGNORE_GCC("-Wformat-security")
    std::fprintf(file, format, args...);
    QLJS_WARNING_POP
    std::fflush(file);
  }
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
