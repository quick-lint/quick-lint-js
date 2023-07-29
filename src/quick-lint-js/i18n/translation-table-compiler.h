// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_I18N_TRANSLATION_TABLE_COMPILER_H
#define QUICK_LINT_JS_I18N_TRANSLATION_TABLE_COMPILER_H

#include <cstdint>
#include <optional>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/i18n/po-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>

// quick-lint-js uses a custom format to store translations. Originally,
// quick-lint-js used GNU gettext's MO format, but it had a number of issues:
//
// * Storing multiple locale requires storing source strings one per
//   locale. This increases file size.
// * The hash function is weak, resulting in many hash collisions.
// * The hash function is imperfect, forcing lookup to compare a matching source
//   string in full for every translation.
// * MO files can be little endian or big endian, complicating lookup.
//
// Our custom format addresses these issues:
//
// * All locales are merged into a single data structure.
// * Lookups happen at compile time, never at run time.
// * The data is always native endian.
//
// The custom format has four parts:
// * the compile-time lookup table
// * the mapping table
// * the string table
// * the locale table
// * some constants
//
// These are the constants (C++ code):
//
//     std::uint32_t locale_count = /* ... */;
//     std::uint16_t mapping_table_size = /* ... */;
//     std::size_t strings_size = /* ... */;
//     std::size_t locale_table_size = /* ... */;
//
// The lookup table is compile-time-only. It is used to convert an untranslated
// string into an index into the mapping table. The lookup table is a sorted
// list of the untranslated strings. The sorting makes output deterministic and
// also enables binary searching.
//
// The mapping table and the locale table are run-time-only. They look like this
// (C++ code):
//
//     struct Mapping_Entry {
//       std::uint32_t string_offsets[locale_count + 1];
//     };
//     Mapping_Entry mapping_table[mapping_table_size];
//
//     char locale_table[locale_table_size] =
//       "en_US\0"
//       "de_DE\0"
//       /* ... */
//       "";  // C++ adds an extra null byte for us.
//
// Mapping_Entry::string_offsets[i] corresponds to the i-th locale listed in
// locale_table.
//
// Mapping_Entry::string_offsets[locale_count] refers to the original
// (untranslated) string.
//
// Entry 0 of the mapping table is unused.
//
// The string table contains 0-terminated UTF-8 strings. String sizes can be
// computed by calculating the difference between the first 0 byte starting at
// the string offset and the string offset.
//
// mapping_table is delta-encoded. The function decode(i, j) is implemented with
// the following logic:
//
// * If mapping_table[i].string_offsets[j] == 0, then the index into the string
//   table is 0 (i.e. the entry refers to an empty string).
// * If mapping_table[i].string_offsets[j] != 0, then the index into the string
//   table is computed as
//   (mapping_table[i].string_offsets[j] + decode(last_i, j)), where last_i is
//   the biggest value of i where
//   (last_i < i && mapping_table[last_i].string_offsets[j] != 0).

namespace quick_lint_js {
struct Translation_Table_Const_Lookup_Entry {
  String8_View untranslated;
};

struct Translation_Table_Mapping_Entry {
  // Key: index of locale in Compiled_Translation_Table.locales
  // Value: offset in Compiled_Translation_Table.string_table
  Span<std::uint32_t> string_offsets;
};

struct Compiled_Translation_Table {
  Span<Translation_Table_Const_Lookup_Entry> const_lookup_table;
  Span<Translation_Table_Mapping_Entry> absolute_mapping_table;
  Span<Translation_Table_Mapping_Entry> relative_mapping_table;
  String8_View string_table;
  Span<String8_View> locales;
  String8_View locale_table;

  // Returns an index into this->mapping_table.
  // If original_string is bogus, returns nullopt.
  std::optional<Span_Size> find_mapping_table_index_for_untranslated(
      String8_View original_string);

  Translation_Table_Mapping_Entry* look_up_mapping_by_untranslated(
      String8_View original_string);

  String8_View read_string(std::uint32_t string_offset);
};

Compiled_Translation_Table compile_translation_table(
    Span<const PO_File> files, Span<const String8_View> untranslated_strings,
    Monotonic_Allocator*);

// Return value is sorted with no duplicates.
//
// The returned span always contains an empty string at the beginning.
Span<String8_View> get_locale_names(Span<const PO_File> files,
                                    Monotonic_Allocator* allocator);

// Extracts .msgid from each PO_Entry.
//
// Return value is sorted with no duplicates.
Span<String8_View> get_all_untranslated(Span<const PO_File> files,
                                        Monotonic_Allocator* allocator);
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
