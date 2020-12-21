// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_SOURCE_LOCATION_H
#define QUICK_LINT_JS_SOURCE_LOCATION_H

#include <cstdint>
#include <quick-lint-js/have.h>

namespace quick_lint_js {
// TODO(strager): Use std::source_location if available.
#if QLJS_HAVE_BUILTIN_FILE_FUNCTION_LINE
class source_location {
 public:
  static constexpr bool valid() noexcept { return true; }

  /*implicit*/ constexpr source_location() = default;

  static source_location current(
      const char* file_name = __builtin_FILE(),
      std::uint_least32_t line = __builtin_LINE()) noexcept {
    return source_location(file_name, line);
  }

  constexpr const char* file_name() const noexcept { return this->file_name_; }
  constexpr std::uint_least32_t line() const noexcept { return this->line_; }

 private:
  explicit source_location(const char* file_name,
                           std::uint_least32_t line) noexcept
      : file_name_(file_name), line_(line) {}

  const char* file_name_ = nullptr;
  std::uint_least32_t line_ = 0;
};
#else
class source_location {
 public:
  static constexpr bool valid() noexcept { return false; }

  /*implicit*/ constexpr source_location() = default;

  static source_location current() noexcept { return source_location(); }

  constexpr const char* file_name() const noexcept { return nullptr; }
  constexpr std::uint_least32_t line() const noexcept { return 0; }
};
#endif
}

#endif
