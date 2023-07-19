// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_PADDED_STRING_H
#define QUICK_LINT_JS_CONTAINER_PADDED_STRING_H

#include <iosfwd>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>

namespace quick_lint_js {
using Padded_String_Size = std::ptrdiff_t;

// Like std::string, but guaranteed to have several null bytes at the end.
//
// Padded_String enables using SIMD instructions without extra bounds checking.
class Padded_String {
 public:
  using Size_Type = Padded_String_Size;

  static constexpr Size_Type padding_size = 64;

  explicit Padded_String();
  explicit Padded_String(String8 &&);
  explicit Padded_String(String8_View);
  explicit Padded_String(const Char8 *) = delete;

  Padded_String(const Padded_String &) = delete;
  Padded_String &operator=(const Padded_String &) = delete;

  Padded_String(Padded_String &&);
  Padded_String &operator=(Padded_String &&);

  ~Padded_String();

  const Char8 *c_str() const noexcept { return this->data(); }

  Char8 *data() noexcept { return this->data_; }
  const Char8 *data() const noexcept { return this->data_; }

  Size_Type size() const noexcept {
    return this->size_excluding_padding_bytes_;
  }
  Size_Type padded_size() const noexcept {
    return this->size() + this->padding_size;
  }

  const Char8 &operator[](Size_Type index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index <= this->size());
    return this->data_[narrow_cast<unsigned>(index)];
  }

  void resize(Size_Type new_size);
  void resize_grow_uninitialized(Size_Type new_size);

  Char8 *begin() noexcept { return this->data(); }
  Char8 *end() noexcept { return this->data() + this->size(); }

  const Char8 *cbegin() const noexcept { return this->data(); }
  const Char8 *cend() const noexcept { return this->data() + this->size(); }

  const Char8 *null_terminator() const noexcept {
    return this->data() + this->size();
  }

  String8_View string_view() const noexcept;

  friend std::ostream &operator<<(std::ostream &, const Padded_String &);

  friend bool operator==(const Padded_String &, const Padded_String &) noexcept;
  friend bool operator!=(const Padded_String &, const Padded_String &) noexcept;
  friend bool operator==(String8_View, const Padded_String &) noexcept;
  friend bool operator!=(String8_View, const Padded_String &) noexcept;
  friend bool operator==(const Padded_String &, String8_View) noexcept;
  friend bool operator!=(const Padded_String &, String8_View) noexcept;

 private:
  void free_and_set_storage(Char8 *new_data,
                            Size_Type new_size_excluding_padding_bytes);

  Char8 *data_;
  Size_Type size_excluding_padding_bytes_;
};

class Padded_String_View {
 public:
  using Size_Type = Padded_String::Size_Type;

  /*implicit*/ Padded_String_View(const Padded_String *string)
      : data_(string->data()), length_(string->size()) {
    QLJS_ASSERT(*this->null_terminator() == u8'\0');
  }

  explicit Padded_String_View(const Char8 *begin, const Char8 *null_terminator)
      : data_(begin),
        length_(narrow_cast<Size_Type>(null_terminator - begin)) {}

  Padded_String_View(const Padded_String_View &) noexcept = default;
  Padded_String_View &operator=(const Padded_String_View &) noexcept = default;

  Padded_String_View(Padded_String_View &&) noexcept = default;
  Padded_String_View &operator=(Padded_String_View &&) noexcept = default;

  const Char8 *data() const noexcept { return this->data_; }

  Size_Type size() const noexcept { return this->length_; }
  Size_Type padded_size() const noexcept {
    return this->size() + Padded_String::padding_size;
  }

  const Char8 *null_terminator() const noexcept {
    return this->data_ + this->length_;
  }

  const Char8 &operator[](Size_Type index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index <= this->size());
    return this->data_[index];
  }

  Padded_String_View substr(Size_Type offset) const noexcept {
    return Padded_String_View(this->data() + offset, this->null_terminator());
  }

  String8_View string_view() const noexcept;

  friend std::ostream &operator<<(std::ostream &, const Padded_String_View &);

  friend bool operator==(String8_View, const Padded_String_View &) noexcept;
  friend bool operator!=(String8_View, const Padded_String_View &) noexcept;
  friend bool operator==(const Padded_String_View &, String8_View) noexcept;
  friend bool operator!=(const Padded_String_View &, String8_View) noexcept;

 private:
  const Char8 *data_;
  Size_Type length_;
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
