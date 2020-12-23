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

#ifndef QUICK_LINT_JS_LOCATION_H
#define QUICK_LINT_JS_LOCATION_H

#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
class source_code_span {
 public:
  explicit source_code_span(const char8* begin, const char8* end) noexcept
      : begin_(begin), end_(end) {}

  const char8* begin() const noexcept { return this->begin_; }

  const char8* end() const noexcept { return this->end_; }

  string8_view string_view() const noexcept {
    return string8_view(this->begin(),
                        narrow_cast<std::size_t>(this->end() - this->begin()));
  }

 private:
  const char8* begin_;
  const char8* end_;
};

bool operator==(source_code_span, string8_view) noexcept;
bool operator!=(source_code_span, string8_view) noexcept;
}

#endif
