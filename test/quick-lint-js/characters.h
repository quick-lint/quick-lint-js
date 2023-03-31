// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CHARACTERS_H
#define QUICK_LINT_JS_CHARACTERS_H

#include <array>
#include <quick-lint-js/array.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
inline constexpr std::array line_terminators_except_ls_ps =
    make_array(u8"\n"_sv, u8"\r"_sv, u8"\r\n"_sv);

inline constexpr std::array ls_and_ps =
    make_array(u8"\u2028"_sv,   // 0xe2 0x80 0xa8 Line Separator
               u8"\u2029"_sv);  // 0xe2 0x80 0xa9 Paragraph Separator

inline constexpr std::array line_terminators =
    concat(line_terminators_except_ls_ps, ls_and_ps);

inline constexpr std::array control_characters_except_whitespace =
    make_array(u8"\u0000"_sv,   // NUL Null character
               u8"\u0001"_sv,   // SOH Start of Heading
               u8"\u0002"_sv,   // STX Start of Text
               u8"\u0003"_sv,   // ETX End-of-text character
               u8"\u0004"_sv,   // EOT End-of-transmission character
               u8"\u0005"_sv,   // ENQ Enquiry character
               u8"\u0006"_sv,   // ACK Acknowledge character
               u8"\u0007"_sv,   // BEL Bell character
               u8"\u0008"_sv,   // BS Backspace
               u8"\u000e"_sv,   // SO Shift Out
               u8"\u000f"_sv,   // SI Shift In
               u8"\u0010"_sv,   // DLE Data Link Escape
               u8"\u0011"_sv,   // DC1 Device Control 1
               u8"\u0012"_sv,   // DC2 Device Control 2
               u8"\u0013"_sv,   // DC3 Device Control 3
               u8"\u0014"_sv,   // DC4 Device Control 4
               u8"\u0015"_sv,   // NAK Negative-acknowledge character
               u8"\u0016"_sv,   // SYN Synchronous Idle
               u8"\u0017"_sv,   // ETB End of Transmission Block
               u8"\u0018"_sv,   // CAN Cancel character
               u8"\u0019"_sv,   // EM End of Medium
               u8"\u001a"_sv,   // SUB Substitute character
               u8"\u001b"_sv,   // ESC Escape character
               u8"\u001c"_sv,   // FS File Separator
               u8"\u001d"_sv,   // GS Group Separator
               u8"\u001e"_sv,   // RS Record Separator
               u8"\u001f"_sv,   // US Unit Separator
               u8"\u007f"_sv);  // DEL Delete

inline constexpr std::array control_characters_except_line_terminators =
    concat(control_characters_except_whitespace,
           make_array(u8"\u0009"_sv,    // HT Horizontal tab
                      u8"\u000b"_sv,    // VT Vertical tab
                      u8"\u000c"_sv));  // FF Form feed
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
