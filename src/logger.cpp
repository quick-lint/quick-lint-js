// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <atomic>
#include <cstdarg>
#include <cstdio>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/logger.h>
#include <quick-lint-js/warning.h>

#if QLJS_HAVE_GETTID
#include <sys/types.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

QLJS_WARNING_IGNORE_CLANG("-Wformat-nonliteral")
QLJS_WARNING_IGNORE_GCC("-Wformat-security")

// Define this macro to a non-empty string to log to the specified file:
// #define QLJS_DEBUG_LOGGING_FILE "/tmp/qljs.log"

namespace quick_lint_js {
namespace {
std::atomic<logger*> global_logger;

logger* get_default_global_logger() {
#if defined(QLJS_DEBUG_LOGGING_FILE)
  static file_logger default_logger(QLJS_DEBUG_LOGGING_FILE);
  return &default_logger;
#else
  return null_logger::instance();
#endif
}
}

logger::~logger() = default;

null_logger* null_logger::instance() noexcept {
  static null_logger logger;
  return &logger;
}

void null_logger::log_v(const char*, std::va_list) {
  // Do nothing.
}

file_logger::file_logger(const char* path) : file_(std::fopen(path, "a")) {
  // TODO(strager): Report fopen failures.
}

void file_logger::log_v(const char* format, std::va_list va) {
  FILE* file = this->file_.get();
  if (!file) {
    // File didn't open. Don't try to log anything.
    return;
  }

#if QLJS_HAVE_GETPID
#if QLJS_HAVE_GETTID
  std::fprintf(file, "[%d.%d] ", ::getpid(), ::gettid());
#else
  std::fprintf(file, "[%d] ", ::getpid());
#endif
#endif
  std::vfprintf(file, format, va);
  std::fflush(file);
}

void file_logger::file_deleter::operator()(FILE* file) {
  if (file) {
    std::fclose(file);
    // TODO(strager): Report fclose failures.
  }
}

void set_global_logger(logger* new_global_logger) {
  global_logger.store(new_global_logger);
}

logger* get_global_logger() {
  logger* l = global_logger.load();
  if (!l) {
    logger* default_logger = get_default_global_logger();
    if (global_logger.compare_exchange_strong(l, default_logger)) {
      l = default_logger;
    } else {
      // Another thread set the global logger. Use its version.
      QLJS_ASSERT(l);
    }
  }
  return l;
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
