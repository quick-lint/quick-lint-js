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

#include <ostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <string>
#include <utility>

namespace quick_lint_js {
padded_string::padded_string()
    : data_(narrow_cast<unsigned>(this->null_bytes_to_add), u8'\0') {}

padded_string::padded_string(string8&& string) : data_(std::move(string)) {
  this->data_.reserve(this->data_.size() +
                      narrow_cast<unsigned>(this->null_bytes_to_add));
  this->data_.append(narrow_cast<unsigned>(this->null_bytes_to_add), '\0');
}

padded_string::padded_string(const char8* string)
    : padded_string(string8(string)) {}

void padded_string::resize(int new_size) {
  this->data_.resize(narrow_cast<std::size_t>(new_size) +
                     this->null_bytes_to_add);
  std::fill(this->data_.end() - this->null_bytes_to_add, this->data_.end(),
            u8'\0');
}

string8_view padded_string::string_view() const noexcept {
  return string8_view(this->data(), narrow_cast<std::size_t>(this->size()));
}

bool operator==(string8_view x, const padded_string& y) noexcept {
  return y == x;
}

bool operator!=(string8_view x, const padded_string& y) noexcept {
  return !(x == y);
}

bool operator==(const padded_string& x, string8_view y) noexcept {
  return string8_view(x.c_str(), narrow_cast<std::size_t>(x.size())) == y;
}

bool operator!=(const padded_string& x, string8_view y) noexcept {
  return !(x == y);
}

std::ostream& operator<<(std::ostream& out, const padded_string& x) {
  out << out_string8(
      string8_view(x.c_str(), narrow_cast<std::size_t>(x.size())));
  return out;
}
}
