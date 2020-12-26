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
  static constexpr int padding_size = 16;

  explicit padded_string();
  explicit padded_string(string8 &&);
  explicit padded_string(string8_view);
  explicit padded_string(const char8 *) = delete;

  const char8 *c_str() const noexcept { return this->data(); }

  char8 *data() noexcept { return this->data_.data(); }
  const char8 *data() const noexcept { return this->data_.data(); }

  int size() const noexcept {
    return narrow_cast<int>(this->data_.size() -
                            narrow_cast<unsigned>(this->null_bytes_to_add));
  }

  const char8 &operator[](int index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index <= this->size());
    return this->data_[narrow_cast<unsigned>(index)];
  }

  void resize(int new_size);

  char8 *begin() noexcept { return this->data(); }
  char8 *end() noexcept { return this->data() + this->size(); }

  const char8 *cbegin() const noexcept { return this->data(); }
  const char8 *cend() const noexcept { return this->data() + this->size(); }

  const char8 *null_terminator() const noexcept {
    return this->data() + this->size();
  }

  string8_view string_view() const noexcept;

  friend std::ostream &operator<<(std::ostream &, const padded_string &);

  friend bool operator==(string8_view, const padded_string &) noexcept;
  friend bool operator!=(string8_view, const padded_string &) noexcept;
  friend bool operator==(const padded_string &, string8_view) noexcept;
  friend bool operator!=(const padded_string &, string8_view) noexcept;

 private:
  static constexpr int null_bytes_to_add = padding_size - 1;

  string8 data_;
};

class padded_string_view {
 public:
  /*implicit*/ padded_string_view(const padded_string *string)
      : data_(string->data()), length_(string->size()) {
    QLJS_ASSERT(*this->null_terminator() == u8'\0');
  }

  explicit padded_string_view(const char8 *begin, const char8 *null_terminator)
      : data_(begin), length_(narrow_cast<int>(null_terminator - begin)) {}

  padded_string_view(const padded_string_view &) noexcept = default;
  padded_string_view &operator=(const padded_string_view &) noexcept = default;

  padded_string_view(padded_string_view &&) noexcept = default;
  padded_string_view &operator=(padded_string_view &&) noexcept = default;

  const char8 *data() const noexcept { return this->data_; }

  int size() const noexcept { return this->length_; }

  const char8 *null_terminator() const noexcept {
    return this->data_ + this->length_;
  }

  const char8 &operator[](int index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index <= this->size());
    return this->data_[index];
  }

  padded_string_view substr(int offset) const noexcept {
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
  int length_;
};
}

#endif
