// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdlib>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <utility>

namespace quick_lint_js {
namespace {
std::array<Char8, Padded_String::padding_size> empty_string = {};
}

Padded_String::Padded_String() {
  this->data_ = empty_string.data();
  this->size_excluding_padding_bytes_ = 0;
}

Padded_String::Padded_String(String8&& string)
    : Padded_String(String8_View(string)) {}

Padded_String::Padded_String(String8_View string) {
  this->size_excluding_padding_bytes_ = narrow_cast<Size_Type>(string.size());
  Size_Type size_including_padding_bytes =
      this->size_excluding_padding_bytes_ + this->padding_size;
  this->data_ = reinterpret_cast<Char8*>(std::malloc(
      narrow_cast<std::size_t>(size_including_padding_bytes) * sizeof(Char8)));
  Char8* padding_bytes = std::copy(string.begin(), string.end(), this->data_);
  std::fill_n(padding_bytes, this->padding_size, u8'\0');
}

Padded_String::Padded_String(Padded_String&& other) {
  this->data_ = std::exchange(other.data_, empty_string.data());
  this->size_excluding_padding_bytes_ =
      std::exchange(other.size_excluding_padding_bytes_, 0);
}

Padded_String& Padded_String::operator=(Padded_String&& other) {
  if (this != &other) {
    this->free_and_set_storage(other.data_,
                               other.size_excluding_padding_bytes_);
    other.data_ = empty_string.data();
    other.size_excluding_padding_bytes_ = 0;
  }
  return *this;
}

Padded_String::~Padded_String() { this->free_and_set_storage(nullptr, 0); }

void Padded_String::resize(Size_Type new_size) {
  Size_Type old_size = this->size_excluding_padding_bytes_;
  if (new_size == old_size) {
    // Do nothing.
  } else if (new_size < old_size) {
    // Shrink. Do not reallocate and copy.
    std::fill_n(&this->data_[new_size], this->padding_size, u8'\0');
    this->size_excluding_padding_bytes_ = new_size;
  } else {
    // Grow. Need to reallocate and copy.
    this->resize_grow_uninitialized(new_size);
    std::fill(&this->data_[old_size], &this->data_[new_size], u8'\0');
  }
}

void Padded_String::resize_grow_uninitialized(Size_Type new_size) {
  Size_Type old_size = this->size_excluding_padding_bytes_;
  QLJS_ASSERT(new_size > old_size);

  Char8* old_data = this->data_ == empty_string.data() ? nullptr : this->data_;
  Size_Type new_size_including_padding_bytes = new_size + this->padding_size;

  Char8* new_data = reinterpret_cast<Char8*>(std::realloc(
      old_data, narrow_cast<std::size_t>(new_size_including_padding_bytes) *
                    sizeof(Char8)));
  // Only null-terminate. Do not write between &new_data[old_size] and
  // &new_data[new_size].
  std::fill_n(&new_data[new_size], this->padding_size, u8'\0');

  this->data_ = new_data;
  this->size_excluding_padding_bytes_ = new_size;
}

String8_View Padded_String::string_view() const noexcept {
  return String8_View(this->data(), narrow_cast<std::size_t>(this->size()));
}

void Padded_String::free_and_set_storage(
    Char8* new_data, Size_Type new_size_excluding_padding_bytes) {
  if (this->data_ != empty_string.data()) {
    std::free(this->data_);
  }
  this->data_ = new_data;
  this->size_excluding_padding_bytes_ = new_size_excluding_padding_bytes;
}

bool operator==(const Padded_String& x, const Padded_String& y) noexcept {
  return x.string_view() == y.string_view();
}

bool operator!=(const Padded_String& x, const Padded_String& y) noexcept {
  return !(x == y);
}

bool operator==(String8_View x, const Padded_String& y) noexcept {
  return y == x;
}

bool operator!=(String8_View x, const Padded_String& y) noexcept {
  return !(x == y);
}

bool operator==(const Padded_String& x, String8_View y) noexcept {
  return x.string_view() == y;
}

bool operator!=(const Padded_String& x, String8_View y) noexcept {
  return !(x == y);
}

String8_View Padded_String_View::string_view() const noexcept {
  return String8_View(this->data(), narrow_cast<std::size_t>(this->size()));
}

bool operator==(String8_View x, const Padded_String_View& y) noexcept {
  return y == x;
}

bool operator!=(String8_View x, const Padded_String_View& y) noexcept {
  return !(x == y);
}

bool operator==(const Padded_String_View& x, String8_View y) noexcept {
  return x.string_view() == y;
}

bool operator!=(const Padded_String_View& x, String8_View y) noexcept {
  return !(x == y);
}
}

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
