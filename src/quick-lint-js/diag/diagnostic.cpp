// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstdint>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cpp.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string_view>
#include <utility>

namespace quick_lint_js {
namespace {
std::array<char, 5> diag_code_to_string(std::uint16_t diag_code) noexcept {
  QLJS_ASSERT(diag_code <= 9999);
  return std::array<char, 5>{
      'E',
      static_cast<char>('0' + ((diag_code / 1000) % 10)),
      static_cast<char>('0' + ((diag_code / 100) % 10)),
      static_cast<char>('0' + ((diag_code / 10) % 10)),
      static_cast<char>('0' + ((diag_code / 1) % 10)),
  };
}
}

const Diagnostic_Info& get_diagnostic_info(Diag_Type type) noexcept {
  return all_diagnostic_infos[static_cast<std::ptrdiff_t>(type)];
}

std::array<char, 5> Diagnostic_Info::code_string() const noexcept {
  return diag_code_to_string(this->code);
}

QLJS_WARNING_PUSH
// GCC thinks that all_diagnostic_infos[i].code is not null-terminated, but it
// is.
QLJS_WARNING_IGNORE_GCC("-Wstringop-overflow")

std::optional<Diag_Type> diag_type_from_code_slow(
    std::string_view code) noexcept {
  for (int i = 0; i < Diag_Type_Count; ++i) {
    // TODO(strager): Parse the incoming code instead of stringifying each code
    // in the table.
    auto diag_code_string = all_diagnostic_infos[i].code_string();
    std::string_view diag_code_string_view(diag_code_string.data(),
                                           diag_code_string.size());
    if (diag_code_string_view == code) {
      return static_cast<Diag_Type>(i);
    }
  }
  return std::nullopt;
}

QLJS_WARNING_POP
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
