// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdarg>
#include <cstdio>
#include <memory>
#include <string_view>

namespace quick_lint_js {
class Logger {
 public:
  virtual ~Logger();

  // Need not be thread-safe.
  virtual void log(std::string_view) = 0;
};

class File_Logger : public Logger {
 public:
  explicit File_Logger(const char* path);

  void log(std::string_view message) override;

 private:
  struct File_Deleter {
    void operator()(FILE*);
  };

  std::unique_ptr<FILE, File_Deleter> file_;
};

void enable_logger(Logger*);
void disable_logger(Logger*);
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
