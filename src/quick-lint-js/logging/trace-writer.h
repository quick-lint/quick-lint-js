// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/logging/trace-types.h>
#include <quick-lint-js/logging/trace-writer-generated.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/binary-writer.h>
#include <string_view>

namespace quick_lint_js {
// Specialize this class to define writing for different string types.
//
// See Trace_String_Writer<std::u16string_view> for an example specialization.
template <class String>
struct Trace_String_Writer;

template <>
struct Trace_String_Writer<std::u16string_view> {
  static std::size_t string_size_u16(std::u16string_view string) {
    return string.size();
  }

  static void copy_string_u16(std::u16string_view string, char16_t* out,
                              std::size_t capacity) {
    QLJS_ASSERT(capacity >= string.size());
    std::copy(string.begin(), string.end(), out);
  }
};

template <class Func>
void Trace_Writer::append_binary(Async_Byte_Queue::Size_Type size,
                                 Func&& callback) {
  std::uint8_t* data_begin =
      reinterpret_cast<std::uint8_t*>(this->out_->append(size));
  Binary_Writer w(data_begin);
  callback(w);
  QLJS_ASSERT(w.bytes_written_since(data_begin) == size);
}

template <class String>
void Trace_Writer::write_utf16le_string(String string) {
  using String_Writer = Trace_String_Writer<String>;
  std::size_t code_unit_count = String_Writer::string_size_u16(string);
  // HACK(strager): Reserve an extra code unit for a null terminator. This is
  // required when interacting with N-API in the Visual Studio Code extension.
  std::size_t capacity = code_unit_count + 1;
  this->append_binary(8, [&](Binary_Writer& w) { w.u64_le(code_unit_count); });
  this->out_->append_aligned(
      capacity * sizeof(char16_t), alignof(char16_t), [&](void* data) {
        String_Writer::copy_string_u16(
            string, reinterpret_cast<char16_t*>(data), capacity);
        return code_unit_count * sizeof(char16_t);
      });
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
