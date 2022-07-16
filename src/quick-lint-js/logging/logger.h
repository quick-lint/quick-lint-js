// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LOGGER_H
#define QUICK_LINT_JS_LOGGER_H

#include <cstdarg>
#include <cstdio>
#include <memory>
#include <string_view>

namespace quick_lint_js {
class logger {
 public:
  virtual ~logger();

  // Need not be thread-safe.
  virtual void log(std::string_view) = 0;
};

class file_logger : public logger {
 public:
  explicit file_logger(const char* path);

  void log(std::string_view message) override;

 private:
  struct file_deleter {
    void operator()(FILE*);
  };

  std::unique_ptr<FILE, file_deleter> file_;
};

void enable_logger(logger*);
void disable_logger(logger*);
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
