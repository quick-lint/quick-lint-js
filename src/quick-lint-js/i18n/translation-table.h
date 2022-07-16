// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_I18N_TRANSLATION_TABLE_H
#define QUICK_LINT_JS_I18N_TRANSLATION_TABLE_H

#include <array>
#include <cstdint>
#include <quick-lint-js/i18n/locale.h>
#include <quick-lint-js/i18n/translation-table-generated.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>

namespace quick_lint_js {
// See tools/compile-translations.go for documentation on the format.
struct translation_table {
  struct mapping_entry {
    std::uint32_t string_offsets[translation_table_locale_count + 1];
  };

  std::array<mapping_entry, translation_table_mapping_table_size> mapping_table;
  char8 string_table[translation_table_string_table_size];
  char locale_table[translation_table_locale_table_size];

  static constexpr std::uint16_t unallocated_mapping_index = 0;

  static QLJS_CONSTEVAL std::uint16_t mapping_index_for_untranslated_string(
      std::string_view s) noexcept {
    return translation_table_const_look_up(s);
  }

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_CLANG("-Wlarge-by-value-copy")
  static QLJS_CONSTEVAL
      std::array<mapping_entry, translation_table_mapping_table_size>
      absolute_mapping_table_from_relative(
          const std::array<mapping_entry, translation_table_mapping_table_size>
              &relative) {
    mapping_entry last_present_mapping = {};
    std::array<mapping_entry, translation_table_mapping_table_size> result = {};
    for (std::uint16_t i = 0; i < translation_table_mapping_table_size; ++i) {
      for (std::uint32_t locale_index = 0;
           locale_index < translation_table_locale_count + 1; ++locale_index) {
        std::uint32_t relative_offset =
            relative[i].string_offsets[locale_index];
        std::uint32_t offset = 0;
        if (relative_offset != 0) {
          offset = relative_offset +
                   last_present_mapping.string_offsets[locale_index];
          last_present_mapping.string_offsets[locale_index] = offset;
        }
        result[i].string_offsets[locale_index] = offset;
      }
    }
    return result;
  }
  QLJS_WARNING_POP
};

extern const translation_table translation_data;
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
