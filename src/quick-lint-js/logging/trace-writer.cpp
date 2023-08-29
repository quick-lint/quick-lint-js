// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/binary-writer.h>

namespace quick_lint_js {
Trace_Writer::Trace_Writer(Async_Byte_Queue* out) : out_(out) {}

void Trace_Writer::commit() { this->out_->commit(); }

void Trace_Writer::write_header(const Trace_Context& context) {
  // clang-format off
  static constexpr std::uint8_t trace_header[] = {
      // CTF magic
      0xc1, 0x1f, 0xfc, 0xc1,

      // quick-lint-js metadata UUID
      0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,
      0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,
  };
  // clang-format on
  this->out_->append_copy(trace_header, std::size(trace_header));

  this->append_binary(8 + 1, [&](Binary_Writer& w) {
    w.u64_le(context.thread_id);
    w.u8(0x00);  // Compression mode
  });
}

void Trace_Writer::write_utf8_string(String8_View s) {
  this->append_binary(8, [&](Binary_Writer& w) { w.u64_le(s.size()); });
  this->out_->append_copy(s.data(), s.size());
}

void Trace_Writer::write_utf8_zstring(String8_View s) {
  QLJS_ASSERT(!contains(s, u8'\0'));
  this->out_->append_copy(s.data(), s.size());
  this->out_->append_copy(u8'\0');
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
