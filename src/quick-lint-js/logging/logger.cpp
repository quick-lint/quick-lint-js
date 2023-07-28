// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cinttypes>
#include <cstdarg>
#include <cstddef>
#include <cstdio>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/logger.h>
#include <quick-lint-js/port/process.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/synchronized.h>
#include <string.h>
#include <vector>

QLJS_WARNING_IGNORE_CLANG("-Wformat-nonliteral")
QLJS_WARNING_IGNORE_GCC("-Wformat-security")

// TODO(strager): Add printf-like annotations.
QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=format")

// Define this macro to a non-empty string to log to the specified file:
// #define QLJS_DEBUG_LOGGING_FILE "/tmp/qljs.log"

namespace quick_lint_js {
namespace {
struct Global_Loggers {
  std::vector<Logger*> loggers;
  bool initialized = false;

  void initialize_if_needed() {
    if (this->initialized) {
      return;
    }
#if defined(QLJS_DEBUG_LOGGING_FILE)
    static file_logger default_logger(QLJS_DEBUG_LOGGING_FILE);
    this->loggers.push_back(&default_logger);
#endif
    this->initialized = true;
  }
};

Synchronized<Global_Loggers> loggers;
}

Logger::~Logger() = default;

File_Logger::File_Logger(const char* path) : file_(std::fopen(path, "a")) {
  // TODO(strager): Report fopen failures.
}

void File_Logger::log(std::string_view message) {
  FILE* file = this->file_.get();
  if (!file) {
    // File didn't open. Don't try to log anything.
    return;
  }

#if !defined(__EMSCRIPTEN__)
  std::fprintf(file, "[%" PRIu64 ".%" PRIu64 "] ", get_current_process_id(),
               get_current_thread_id());
#endif
  std::fprintf(file, "%.*s", narrow_cast<int>(message.size()), message.data());
  std::fflush(file);
}

void File_Logger::File_Deleter::operator()(FILE* file) {
  if (file) {
    std::fclose(file);
    // TODO(strager): Report fclose failures.
  }
}

void enable_logger(Logger* l) {
  Lock_Ptr<Global_Loggers> locked_loggers = loggers.lock();
  locked_loggers->initialize_if_needed();

  QLJS_ASSERT(!contains(locked_loggers->loggers, l));
  locked_loggers->loggers.push_back(l);
}

void disable_logger(Logger* l) {
  Lock_Ptr<Global_Loggers> locked_loggers = loggers.lock();
  locked_loggers->initialize_if_needed();

  locked_loggers->loggers.erase(
      find_unique_existing(locked_loggers->loggers, l));
}

bool is_logging_enabled() {
  Lock_Ptr<Global_Loggers> locked_loggers = loggers.lock();
  locked_loggers->initialize_if_needed();
  return !locked_loggers->loggers.empty();
}

namespace {
void debug_log_v(const char* format, std::va_list args) {
  Lock_Ptr<Global_Loggers> locked_loggers = loggers.lock();
  locked_loggers->initialize_if_needed();
  if (locked_loggers->loggers.empty()) {
    return;
  }

  std::array<char, 1024> message;
  int full_message_length =
      std::vsnprintf(message.data(), message.size(), format, args);
  QLJS_ALWAYS_ASSERT(full_message_length >= 0);
  std::size_t message_length = std::min(
      narrow_cast<std::size_t>(full_message_length), message.size() - 1);
  std::string_view message_view(message.data(), message_length);

  for (Logger* l : locked_loggers->loggers) {
    l->log(message_view);
  }
}
}

void debug_log(const char* format, ...) {
  std::va_list args;
  va_start(args, format);
  debug_log_v(format, args);
  va_end(args);
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
