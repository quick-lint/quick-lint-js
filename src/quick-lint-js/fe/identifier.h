// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_IDENTIFIER_H
#define QUICK_LINT_JS_IDENTIFIER_H

#include <cstddef>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
class identifier {
 public:
  // For tests only.
  explicit identifier(source_code_span span) noexcept
      : span_begin_(span.begin()),
        normalized_begin_(this->span_begin_),
        span_size_(narrow_cast<int>(span.end() - span.begin())),
        normalized_size_(this->span_size_) {}

  explicit identifier(source_code_span span, string8_view normalized) noexcept
      : span_begin_(span.begin()),
        normalized_begin_(normalized.data()),
        span_size_(narrow_cast<int>(span.end() - span.begin())),
        normalized_size_(narrow_cast<int>(normalized.size())) {}

  explicit identifier(source_code_span span,
                      const char8* normalized) noexcept = delete;

  source_code_span span() const noexcept {
    return source_code_span(this->span_begin_,
                            this->span_begin_ + this->span_size_);
  }

  // normalized_name returns the variable's name with escape sequences resolved.
  //
  // For example, a variable named \u{61} in the source code will have
  // normalized_name refer to u8"a".
  //
  // The returned pointers might not reside within the source code string. In
  // other words, the normalized name might be heap-allocated. Call span()
  // instead if you want pointers within the source code input.
  string8_view normalized_name() const noexcept {
    return string8_view(this->normalized_begin_,
                        narrow_cast<std::size_t>(this->normalized_size_));
  }

 private:
  const char8* span_begin_;
  const char8* normalized_begin_;
  int span_size_;
  int normalized_size_;
};
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
