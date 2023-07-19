// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_C_STRING_LIST_H
#define QUICK_LINT_JS_CONTAINER_C_STRING_LIST_H

#include <cstddef>
#include <cstring>
#include <string_view>

namespace quick_lint_js {
// See C_String_List_View.
class C_String_List_Sentinel {};

// See C_String_List_View.
class C_String_List_Iterator {
 public:
  explicit C_String_List_Iterator(const char* cursor) noexcept
      : cursor_(cursor), current_string_size_(std::strlen(cursor)) {}

  // The returned view's .data() is guaranteed to be null-terminated.
  std::string_view operator*() const noexcept {
    return std::string_view(this->cursor_, this->current_string_size_);
  }

  const char* c_str() const noexcept { return this->cursor_; }

  C_String_List_Iterator& operator++() noexcept {
    *this =
        C_String_List_Iterator(this->cursor_ + this->current_string_size_ + 1);
    return *this;
  }

  friend bool operator==(C_String_List_Iterator it,
                         C_String_List_Sentinel) noexcept {
    return *it.cursor_ == '\0';
  }
  friend bool operator!=(C_String_List_Iterator it,
                         C_String_List_Sentinel sentinel) noexcept {
    return !(it == sentinel);
  }

  friend bool operator==(C_String_List_Sentinel sentinel,
                         C_String_List_Iterator it) noexcept {
    return it == sentinel;
  }
  friend bool operator!=(C_String_List_Sentinel sentinel,
                         C_String_List_Iterator it) noexcept {
    return !(sentinel == it);
  }

 private:
  const char* cursor_;
  std::size_t current_string_size_;
};

// A list of non-empty null-terminated strings.
class C_String_List_View {
 public:
  using value_type = std::string_view;
  using iterator = C_String_List_Iterator;
  using const_iterator = C_String_List_Iterator;

  // begin must be in the form "string1\0string2\0string3\0".
  //
  // Note that the list is terminated by two null bytes (one written
  // explicitly and one added by C++'s "" syntax.)
  explicit C_String_List_View(const char* begin) noexcept : begin_(begin) {}

  C_String_List_Iterator begin() const noexcept {
    return C_String_List_Iterator(this->begin_);
  }

  C_String_List_Sentinel end() const noexcept {
    return C_String_List_Sentinel();
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
