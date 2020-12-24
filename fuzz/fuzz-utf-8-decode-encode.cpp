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

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/utf-8.h>

extern "C" {
int LLVMFuzzerTestOneInput(const std::uint8_t* data, std::size_t size) {
  using namespace quick_lint_js;

  padded_string input(string8_view(reinterpret_cast<const char8*>(data), size));

  const char8* c = input.data();
  while (c != input.null_terminator()) {
    decode_utf_8_result r =
        decode_utf_8(padded_string_view(c, input.null_terminator()));

    if (r.ok) {
      char8 encode_buffer[4];
      char8* encode_end = encode_utf_8(r.code_point, encode_buffer);
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
