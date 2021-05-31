// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PADDED_STRING_H
#define QUICK_LINT_JS_PADDED_STRING_H

#include <iosfwd>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <string>

namespace quick_lint_js {
// Like std::string, but guaranteed to have several null bytes at the end.
//
// padded_string enables using SIMD instructions without extra bounds checking.
class padded_string {
 public:
  using size_type = int;

  static constexpr size_type padding_size = 32;

  explicit padded_string();
  explicit padded_string(string8 &&);
  explicit padded_string(string8_view);
  explicit padded_string(const char8 *) = delete;

  padded_string(const padded_string &) = delete;
  padded_string &operator=(const padded_string &) = delete;

  padded_string(padded_string &&);
  padded_string &operator=(padded_string &&);

  ~padded_string();

  const char8 *c_str() const noexcept { return this->data(); }

  char8 *data() noexcept { return this->data_; }
  const char8 *data() const noexcept { return this->data_; }

  size_type size() const noexcept {
    return this->size_excluding_padding_bytes_;
  }

  const char8 &operator[](size_type index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index <= this->size());
    return this->data_[narrow_cast<unsigned>(index)];
  }

  void resize(size_type new_size);
  void resize_grow_uninitialized(size_type new_size);

  char8 *begin() noexcept { return this->data(); }
  char8 *end() noexcept { return this->data() + this->size(); }

  const char8 *cbegin() const noexcept { return this->data(); }
  const char8 *cend() const noexcept { return this->data() + this->size(); }

  const char8 *null_terminator() const noexcept {
    return this->data() + this->size();
  }

  string8_view string_view() const noexcept;

  friend std::ostream &operator<<(std::ostream &, const padded_string &);

  friend bool operator==(const padded_string &, const padded_string &) noexcept;
  friend bool operator!=(const padded_string &, const padded_string &) noexcept;
  friend bool operator==(string8_view, const padded_string &) noexcept;
  friend bool operator!=(string8_view, const padded_string &) noexcept;
  friend bool operator==(const padded_string &, string8_view) noexcept;
  friend bool operator!=(const padded_string &, string8_view) noexcept;

 private:
  void free_and_set_storage(char8 *new_data,
                            size_type new_size_excluding_padding_bytes);

  char8 *data_;
  size_type size_excluding_padding_bytes_;
};

class padded_string_view {
 public:
  using size_type = padded_string::size_type;

  /*implicit*/ padded_string_view(const padded_string *string)
      : data_(string->data()), length_(string->size()) {
    QLJS_ASSERT(*this->null_terminator() == u8'\0');
  }

  explicit padded_string_view(const char8 *begin, const char8 *null_terminator)
      : data_(begin),
        length_(narrow_cast<size_type>(null_terminator - begin)) {}

  padded_string_view(const padded_string_view &) noexcept = default;
  padded_string_view &operator=(const padded_string_view &) noexcept = default;

  padded_string_view(padded_string_view &&) noexcept = default;
  padded_string_view &operator=(padded_string_view &&) noexcept = default;

  const char8 *data() const noexcept { return this->data_; }

  size_type size() const noexcept { return this->length_; }
  size_type padded_size() const noexcept {
    return this->size() + padded_string::padding_size;
  }

  const char8 *null_terminator() const noexcept {
    return this->data_ + this->length_;
  }

  const char8 &operator[](size_type index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index <= this->size());
    return this->data_[index];
  }

  padded_string_view substr(size_type offset) const noexcept {
    return padded_string_view(this->data() + offset, this->null_terminator());
  }

  string8_view string_view() const noexcept;

  friend std::ostream &operator<<(std::ostream &, const padded_string_view &);

  friend bool operator==(string8_view, const padded_string_view &) noexcept;
  friend bool operator!=(string8_view, const padded_string_view &) noexcept;
  friend bool operator==(const padded_string_view &, string8_view) noexcept;
  friend bool operator!=(const padded_string_view &, string8_view) noexcept;

 private:
  const char8 *data_;
  size_type length_;
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
