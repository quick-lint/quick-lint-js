// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_IDENTIFIER_H
#define QUICK_LINT_JS_FE_IDENTIFIER_H

#include <cstddef>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/narrow-cast.h>

namespace quick_lint_js {
class Identifier {
 public:
  // For tests only.
  explicit Identifier(Source_Code_Span span)
      : span_begin_(span.begin()),
        normalized_begin_(this->span_begin_),
        span_size_(narrow_cast<unsigned>(span.end() - span.begin())),
        normalized_size_(this->span_size_) {}

  explicit Identifier(Source_Code_Span span, String8_View normalized)
      : span_begin_(span.begin()),
        normalized_begin_(normalized.data()),
        span_size_(narrow_cast<unsigned>(span.end() - span.begin())),
        normalized_size_(narrow_cast<unsigned>(normalized.size())) {}

  explicit Identifier(Source_Code_Span span, const Char8* normalized) = delete;

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")
  Source_Code_Span span() const {
    return Source_Code_Span(this->span_begin_,
                            this->span_begin_ + this->span_size_);
  }
  QLJS_WARNING_POP

  // normalized_name returns the variable's name with escape sequences resolved.
  //
  // For example, a variable named \u{61} in the source code will have
  // normalized_name refer to u8"a".
  //
  // The returned pointers might not reside within the source code string. In
  // other words, the normalized name might be heap-allocated. Call span()
  // instead if you want pointers within the source code input.
  String8_View normalized_name() const {
    return String8_View(this->normalized_begin_, this->normalized_size_);
  }

 private:
  const Char8* span_begin_;
  const Char8* normalized_begin_;
  unsigned span_size_;
  unsigned normalized_size_;
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
