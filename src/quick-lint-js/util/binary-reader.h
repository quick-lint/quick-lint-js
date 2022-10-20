// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_BINARY_READER_H
#define QUICK_LINT_JS_UTIL_BINARY_READER_H

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/function-ref.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <utility>

namespace quick_lint_js {
// checked_binary_reader calls unexpected_end_of_file on error.
//
// Invariant: When unexpected_end_of_file is called, it does not return.
class checked_binary_reader {
 public:
  explicit checked_binary_reader(const std::uint8_t* data,
                                 std::size_t data_size,
                                 function_ref<void()> unexpected_end_of_file)
      : data_(data),
        data_end_(data + data_size),
        unexpected_end_of_file_(unexpected_end_of_file) {}

  bool eof() const noexcept { return this->data_ == this->data_end_; }

  std::uint8_t u8() { return *this->advance(1); }

  std::uint32_t u32_le() {
    const std::uint8_t* d = this->advance(4);
    std::uint32_t result = (static_cast<std::uint32_t>(d[0]) << (8 * 0)) |
                           (static_cast<std::uint32_t>(d[1]) << (8 * 1)) |
                           (static_cast<std::uint32_t>(d[2]) << (8 * 2)) |
                           (static_cast<std::uint32_t>(d[3]) << (8 * 3));
    return result;
  }

  std::uint64_t u64_le() {
    const std::uint8_t* d = this->advance(8);
    std::uint64_t result = (static_cast<std::uint64_t>(d[0]) << (8 * 0)) |
                           (static_cast<std::uint64_t>(d[1]) << (8 * 1)) |
                           (static_cast<std::uint64_t>(d[2]) << (8 * 2)) |
                           (static_cast<std::uint64_t>(d[3]) << (8 * 3)) |
                           (static_cast<std::uint64_t>(d[4]) << (8 * 4)) |
                           (static_cast<std::uint64_t>(d[5]) << (8 * 5)) |
                           (static_cast<std::uint64_t>(d[6]) << (8 * 6)) |
                           (static_cast<std::uint64_t>(d[7]) << (8 * 7));
    return result;
  }

  const std::uint8_t* advance(std::size_t size) {
    if (size > this->remaining()) {
      this->unexpected_end_of_file_();
      QLJS_UNREACHABLE();
    }
    return std::exchange(this->data_, this->data_ + size);
  }

  const std::uint8_t* cursor() { return this->data_; }

  std::size_t remaining() {
    return narrow_cast<std::size_t>(this->data_end_ - this->data_);
  }

  // If end_byte exists, this function advances to *after* the found byte and
  // returns true.
  //
  // If end_byte does not exist, this function does not advance and calls
  // unexpected_end_of_file.
  void find_and_skip_byte(std::uint8_t end_byte) {
    const std::uint8_t* found =
        find_first(this->data_, this->data_end_, end_byte);
    if (found == this->data_end_) {
      this->unexpected_end_of_file_();
    }
    this->data_ = found + 1;
  }

 private:
  const std::uint8_t* data_;
  const std::uint8_t* data_end_;
  function_ref<void()> unexpected_end_of_file_;
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
