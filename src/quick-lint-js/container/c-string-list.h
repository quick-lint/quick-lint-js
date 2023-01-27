// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_C_STRING_LIST_H
#define QUICK_LINT_JS_CONTAINER_C_STRING_LIST_H

#include <cstddef>
#include <cstring>
#include <string_view>

namespace quick_lint_js {
// See c_string_list_view.
class c_string_list_sentinel {};

// See c_string_list_view.
class c_string_list_iterator {
 public:
  explicit c_string_list_iterator(const char* cursor) noexcept
      : cursor_(cursor), current_string_size_(std::strlen(cursor)) {}

  // The returned view's .data() is guaranteed to be null-terminated.
  std::string_view operator*() const noexcept {
    return std::string_view(this->cursor_, this->current_string_size_);
  }

  const char* c_str() const noexcept { return this->cursor_; }

  c_string_list_iterator& operator++() noexcept {
    *this =
        c_string_list_iterator(this->cursor_ + this->current_string_size_ + 1);
    return *this;
  }

  friend bool operator==(c_string_list_iterator it,
                         c_string_list_sentinel) noexcept {
    return *it.cursor_ == '\0';
  }
  friend bool operator!=(c_string_list_iterator it,
                         c_string_list_sentinel sentinel) noexcept {
    return !(it == sentinel);
  }

  friend bool operator==(c_string_list_sentinel sentinel,
                         c_string_list_iterator it) noexcept {
    return it == sentinel;
  }
  friend bool operator!=(c_string_list_sentinel sentinel,
                         c_string_list_iterator it) noexcept {
    return !(sentinel == it);
  }

 private:
  const char* cursor_;
  std::size_t current_string_size_;
};

// A list of non-empty null-terminated strings.
class c_string_list_view {
 public:
  using value_type = std::string_view;
  using iterator = c_string_list_iterator;
  using const_iterator = c_string_list_iterator;

  // begin must be in the form "string1\0string2\0string3\0".
  //
  // Note that the list is terminated by two null bytes (one written
  // explicitly and one added by C++'s "" syntax.)
  explicit c_string_list_view(const char* begin) noexcept : begin_(begin) {}

  c_string_list_iterator begin() const noexcept {
    return c_string_list_iterator(this->begin_);
  }

  c_string_list_sentinel end() const noexcept {
    return c_string_list_sentinel();
  }

 private:
  const char* begin_;
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
