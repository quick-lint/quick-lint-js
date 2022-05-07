// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_BINARY_WRITER_H
#define QUICK_LINT_JS_BINARY_WRITER_H

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
// binary_writer does no bounds checking.
class binary_writer {
 public:
  explicit binary_writer(std::uint8_t *out) noexcept : out_(out) {}

  void u8(std::uint8_t data) noexcept { *this->out_++ = data; }

  void u64_le(std::uint64_t data) noexcept {
    this->out_[0] = static_cast<std::uint8_t>(data >> (8 * 0));
    this->out_[1] = static_cast<std::uint8_t>(data >> (8 * 1));
    this->out_[2] = static_cast<std::uint8_t>(data >> (8 * 2));
    this->out_[3] = static_cast<std::uint8_t>(data >> (8 * 3));
    this->out_[4] = static_cast<std::uint8_t>(data >> (8 * 4));
    this->out_[5] = static_cast<std::uint8_t>(data >> (8 * 5));
    this->out_[6] = static_cast<std::uint8_t>(data >> (8 * 6));
    this->out_[7] = static_cast<std::uint8_t>(data >> (8 * 7));
    this->out_ += 8;
  }

  std::size_t bytes_written_since(std::uint8_t *begin) const noexcept {
    return narrow_cast<std::size_t>(this->out_ - begin);
  }

 private:
  std::uint8_t *out_;
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
