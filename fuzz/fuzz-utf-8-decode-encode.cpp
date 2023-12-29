// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/utf-8.h>

extern "C" {
int LLVMFuzzerTestOneInput(const std::uint8_t* data, std::size_t size) {
  using namespace quick_lint_js;

  Padded_String input(String8_View(reinterpret_cast<const Char8*>(data), size));

  const Char8* c = input.data();
  while (c != input.null_terminator()) {
    Decode_UTF8_Result r =
        decode_utf_8(Padded_String_View(c, input.null_terminator()));

    if (r.ok) {
      Char8 encode_buffer[4];
      Char8* encode_end = encode_utf_8(r.code_point, encode_buffer);
      if (encode_end - encode_buffer != r.size) {
        std::fprintf(stderr,
                     "fatal: decoding %ju gave %zd bytes but reencoding gave "
                     "%zd bytes\n",
                     narrow_cast<std::uintmax_t>(r.code_point), r.size,
                     encode_end - encode_buffer);
        std::abort();
      }
      if (std::memcmp(encode_buffer, c, r.size) != 0) {
        std::fprintf(
            stderr,
            "fatal: decoding %ju then reencoding gave different bytes\n",
            narrow_cast<std::uintmax_t>(r.code_point));
        std::abort();
      }
    }

    c += r.size;
  }

  return 0;
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
