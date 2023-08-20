// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdint>
#include <quick-lint-js/port/have.h>

namespace quick_lint_js {
// TODO(strager): Use std::source_location if available.
#if QLJS_HAVE_BUILTIN_FILE_FUNCTION_LINE
class Source_Location {
 public:
  static constexpr bool valid() { return true; }

  /*implicit*/ constexpr Source_Location() = default;

  static Source_Location current(
      const char* file_name = __builtin_FILE(),
      const char* function_name = __builtin_FUNCTION(),
      std::uint_least32_t line = __builtin_LINE()) {
    return Source_Location(file_name, function_name, line);
  }

  constexpr const char* file_name() const { return this->file_name_; }
  constexpr const char* function_name() const { return this->function_name_; }
  constexpr std::uint_least32_t line() const { return this->line_; }

 private:
  explicit Source_Location(const char* file_name, const char* function_name,
                           std::uint_least32_t line)
      : file_name_(file_name), function_name_(function_name), line_(line) {}

  const char* file_name_ = nullptr;
  const char* function_name_ = nullptr;
  std::uint_least32_t line_ = 0;
};
#else
class Source_Location {
 public:
  static constexpr bool valid() { return false; }

  /*implicit*/ constexpr Source_Location() = default;

  static Source_Location current() { return Source_Location(); }

  constexpr const char* file_name() const { return nullptr; }
  constexpr const char* function_name() const { return nullptr; }
  constexpr std::uint_least32_t line() const { return 0; }
};
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
