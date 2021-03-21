// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

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

  string8_view input(reinterpret_cast<const char8*>(data), size);
  const char8* input_end = input.data() + input.size();
  padded_string padded_input(input);

  for (int character_count = 0;; ++character_count) {
    const char8* c = advance_lsp_characters_in_utf_8(input, character_count);
    int offset = c - input.data();
    std::ptrdiff_t counted_characters =
        count_lsp_characters_in_utf_8(&padded_input, offset);

    bool ok = true;
    if (counted_characters == character_count - 1) {
      // Code unit to count is possibly the second of a UTF-16 surrogate pair.
      decode_utf_8_result result =
          decode_utf_8(padded_string_view(c, input_end));
      bool character_needs_utf_16_surrogate_pair =
          result.ok && result.code_point >= 0x10000;
      if (!character_needs_utf_16_surrogate_pair) {
        ok = false;
      }
    } else if (counted_characters != character_count) {
      ok = false;
    }
    if (!ok) {
      std::fprintf(
          stderr,
          "fatal: advancing %d gave offset %d, then counting to %d gave %zd\n",
          character_count, offset, offset, counted_characters);
      std::abort();
    }

    if (c == input_end) {
      break;
    }
  }

  return 0;
}
}

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
