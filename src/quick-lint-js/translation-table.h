// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRANSLATION_TABLE_H
#define QUICK_LINT_JS_TRANSLATION_TABLE_H

#include <cstdint>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/hash-fnv.h>
#include <quick-lint-js/locale.h>
#include <quick-lint-js/translation-table-generated.h>

namespace quick_lint_js {
// See tools/compile-translations.go for documentation on the format.
struct translation_table {
  struct mapping_entry {
    std::uint32_t string_offsets[translation_table_locale_count + 1];
  };

  mapping_entry mapping_table[translation_table_mapping_table_size];
  char8 string_table[translation_table_string_table_size];
  char locale_table[translation_table_locale_table_size];

  static constexpr std::uint16_t unallocated_mapping_index = 0;

  static QLJS_CONSTEVAL std::uint16_t mapping_index_for_untranslated_string(
      std::string_view s) noexcept {
    return translation_table_const_hash_table_look_up(
        s, unallocated_mapping_index);
  }
};

extern translation_table translation_data;
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
