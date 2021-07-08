// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdlib>
#include <ostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <simdjson/common_defs.h>
#include <string>
#include <utility>

namespace quick_lint_js {
static_assert(padded_string::padding_size >= ::simdjson::SIMDJSON_PADDING,
              "padded_string must have enough padded to satisfy simdjson");

namespace {
std::array<char8, padded_string::padding_size> empty_string = {};
}

padded_string::padded_string() {
  this->data_ = empty_string.data();
  this->size_excluding_padding_bytes_ = 0;
}

padded_string::padded_string(string8&& string)
    : padded_string(string8_view(string)) {}

padded_string::padded_string(string8_view string) {
  this->size_excluding_padding_bytes_ = narrow_cast<size_type>(string.size());
  size_type size_including_padding_bytes =
      this->size_excluding_padding_bytes_ + this->padding_size;
  this->data_ = reinterpret_cast<char8*>(std::malloc(
      narrow_cast<std::size_t>(size_including_padding_bytes) * sizeof(char8)));
  char8* padding_bytes = std::copy(string.begin(), string.end(), this->data_);
  std::fill_n(padding_bytes, this->padding_size, u8'\0');
}

padded_string::padded_string(padded_string&& other) {
  this->data_ = std::exchange(other.data_, empty_string.data());
  this->size_excluding_padding_bytes_ =
      std::exchange(other.size_excluding_padding_bytes_, 0);
}

padded_string& padded_string::operator=(padded_string&& other) {
  if (this != &other) {
    this->free_and_set_storage(other.data_,
                               other.size_excluding_padding_bytes_);
    other.data_ = empty_string.data();
    other.size_excluding_padding_bytes_ = 0;
  }
  return *this;
}

padded_string::~padded_string() { this->free_and_set_storage(nullptr, 0); }

void padded_string::resize(size_type new_size) {
  size_type old_size = this->size_excluding_padding_bytes_;
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

void padded_string::resize_grow_uninitialized(size_type new_size) {
  size_type old_size = this->size_excluding_padding_bytes_;
  QLJS_ASSERT(new_size > old_size);

  char8* old_data = this->data_ == empty_string.data() ? nullptr : this->data_;
  size_type new_size_including_padding_bytes = new_size + this->padding_size;

  char8* new_data = reinterpret_cast<char8*>(std::realloc(
      old_data, narrow_cast<std::size_t>(new_size_including_padding_bytes) *
                    sizeof(char8)));
  // Only null-terminate. Do not write between &new_data[old_size] and
  // &new_data[new_size].
  std::fill_n(&new_data[new_size], this->padding_size, u8'\0');

  this->data_ = new_data;
  this->size_excluding_padding_bytes_ = new_size;
}

string8_view padded_string::string_view() const noexcept {
  return string8_view(this->data(), narrow_cast<std::size_t>(this->size()));
}

void padded_string::free_and_set_storage(
    char8* new_data, size_type new_size_excluding_padding_bytes) {
  if (this->data_ != empty_string.data()) {
    std::free(this->data_);
  }
  this->data_ = new_data;
  this->size_excluding_padding_bytes_ = new_size_excluding_padding_bytes;
}

bool operator==(const padded_string& x, const padded_string& y) noexcept {
  return x.string_view() == y.string_view();
}

bool operator!=(const padded_string& x, const padded_string& y) noexcept {
  return !(x == y);
}

bool operator==(string8_view x, const padded_string& y) noexcept {
  return y == x;
}

bool operator!=(string8_view x, const padded_string& y) noexcept {
  return !(x == y);
}

bool operator==(const padded_string& x, string8_view y) noexcept {
  return x.string_view() == y;
}

bool operator!=(const padded_string& x, string8_view y) noexcept {
  return !(x == y);
}

std::ostream& operator<<(std::ostream& out, const padded_string& x) {
  out << out_string8(x.string_view());
  return out;
}

string8_view padded_string_view::string_view() const noexcept {
  return string8_view(this->data(), narrow_cast<std::size_t>(this->size()));
}

std::ostream& operator<<(std::ostream& out, const padded_string_view& x) {
  out << out_string8(x.string_view());
  return out;
}

bool operator==(string8_view x, const padded_string_view& y) noexcept {
  return y == x;
}

bool operator!=(string8_view x, const padded_string_view& y) noexcept {
  return !(x == y);
}

bool operator==(const padded_string_view& x, string8_view y) noexcept {
  return x.string_view() == y;
}

bool operator!=(const padded_string_view& x, string8_view y) noexcept {
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
