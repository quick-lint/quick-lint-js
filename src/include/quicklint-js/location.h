// quicklint-js finds bugs in JavaScript programs.
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

#ifndef QUICKLINT_JS_LOCATION_H
#define QUICKLINT_JS_LOCATION_H

#include <cstddef>
#include <string_view>

namespace quicklint_js {
struct source_position {
  int line_number;
  int column_number;
  std::size_t offset;
};

class source_range {
 public:
  using offset = decltype(source_position::offset);

  explicit source_range(source_position begin, source_position end) noexcept
      : begin_(begin), end_(end) {}

  offset begin_offset() const noexcept { return this->begin_.offset; }
  source_position begin() const noexcept;

  offset end_offset() const noexcept { return this->end_.offset; }
  source_position end() const noexcept;

 private:
  source_position begin_;
  source_position end_;
};

class source_code_span {
 public:
  explicit source_code_span(const char* begin, const char* end) noexcept
      : begin_(begin), end_(end) {}

  const char* begin() const noexcept { return this->begin_; }

  const char* end() const noexcept { return this->end_; }

  std::string_view string_view() const noexcept {
    return std::string_view(this->begin(), this->end() - this->begin());
  }

 private:
  const char* begin_;
  const char* end_;
};

class locator {
 public:
  explicit locator(const char* input) noexcept : input_(input) {}

  source_range range(source_code_span) const;
  source_position position(const char*) const noexcept;

 private:
  const char* input_;
};
}  // namespace quicklint_js

#endif
