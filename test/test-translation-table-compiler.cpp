// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/i18n/po-parser.h>
#include <quick-lint-js/i18n/translation-table-compiler.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <vector>

using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
void check_table_integrity(const Compiled_Translation_Table&);

TEST(Test_Translation_Table_Compiler, no_locales_or_translation_strings) {
  Monotonic_Allocator allocator("test");
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(), Span<const String8_View>(), &allocator);

  check_table_integrity(table);
  EXPECT_THAT(table.locales, ::testing::IsEmpty());
}

TEST(Test_Translation_Table_Compiler, locales_with_no_translation_strings) {
  Monotonic_Allocator allocator("test");
  PO_File files[] = {
      {.locale = u8"en_US"_sv, .entries = Span<PO_Entry>()},
      {.locale = u8"de_DE"_sv, .entries = Span<PO_Entry>()},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files), Span<const String8_View>(), &allocator);

  check_table_integrity(table);
  EXPECT_THAT(table.locales, ElementsAreArray({u8"de_DE"_sv, u8"en_US"_sv}));
  EXPECT_EQ(table.locale_table, u8"de_DE\u0000en_US\u0000\u0000"_sv);
}

TEST(Test_Translation_Table_Compiler,
     one_locale_with_several_translation_strings) {
  Monotonic_Allocator allocator("test");
  PO_Entry fr_entries[] = {
      {u8"hello"_sv, u8"bonjour"_sv},
      {u8"goodbye"_sv, u8"au revoir"_sv},
      {u8"yes"_sv, u8"oui"_sv},
      {u8"no"_sv, u8"non"_sv},
  };
  String8_View untranslated_strings[] = {
      u8"hello"_sv,
      u8"goodbye"_sv,
      u8"yes"_sv,
      u8"no"_sv,
  };
  PO_File files[] = {
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  Translation_Table_Mapping_Entry* hello_entry =
      table.look_up_mapping_by_untranslated(u8"hello"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[0]), u8"bonjour"_sv);

  Translation_Table_Mapping_Entry* goodbye_entry =
      table.look_up_mapping_by_untranslated(u8"goodbye"_sv);
  EXPECT_EQ(table.read_string(goodbye_entry->string_offsets[0]),
            u8"au revoir"_sv);

  Translation_Table_Mapping_Entry* yes_entry =
      table.look_up_mapping_by_untranslated(u8"yes"_sv);
  EXPECT_EQ(table.read_string(yes_entry->string_offsets[0]), u8"oui"_sv);

  Translation_Table_Mapping_Entry* no_entry =
      table.look_up_mapping_by_untranslated(u8"no"_sv);
  EXPECT_EQ(table.read_string(no_entry->string_offsets[0]), u8"non"_sv);
}

TEST(Test_Translation_Table_Compiler,
     several_locales_with_common_translation_strings) {
  Monotonic_Allocator allocator("test");
  PO_Entry fr_entries[] = {
      {u8"hello"_sv, u8"bonjour"_sv},
  };
  PO_Entry de_entries[] = {
      {u8"hello"_sv, u8"hallo"_sv},
  };
  String8_View untranslated_strings[] = {
      u8"hello"_sv,
  };
  PO_File files[] = {
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
      {.locale = u8"de_DE"_sv, .entries = Span<PO_Entry>(de_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  Translation_Table_Mapping_Entry* hello_entry =
      table.look_up_mapping_by_untranslated(u8"hello"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[0]), u8"hallo"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[1]), u8"bonjour"_sv);
}

TEST(Test_Translation_Table_Compiler, translation_table_excludes_metadata) {
  Monotonic_Allocator allocator("test");
  PO_Entry fr_entries[] = {
      {u8""_sv, u8"metadata goes here"_sv},
      {u8"hello"_sv, u8"bonjour"_sv},
  };
  PO_Entry de_entries[] = {
      {u8""_sv, u8"(meta data)"_sv},
      {u8"hello"_sv, u8"hallo"_sv},
  };
  String8_View untranslated_strings[] = {
      u8"hello"_sv,
  };
  PO_File files[] = {
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
      {.locale = u8"de_DE"_sv, .entries = Span<PO_Entry>(de_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  for (Span_Size mapping_index = 0;
       mapping_index < table.absolute_mapping_table.size(); ++mapping_index) {
    Translation_Table_Mapping_Entry& mapping_entry =
        table.absolute_mapping_table[mapping_index];
    for (Span_Size locale_index = 0;
         locale_index < mapping_entry.string_offsets.size(); ++locale_index) {
      std::uint32_t string_offset = mapping_entry.string_offsets[locale_index];
      String8_View s = table.read_string(string_offset);
      EXPECT_THAT(s, ::testing::Not(::testing::AnyOf(u8"metadata goes here"_sv,
                                                     u8"(meta data)"_sv)))
          << "mapping table entry " << mapping_index << " locale "
          << locale_index << " should not have metadata string";
    }
  }
}

// FIXME(strager): This test looks identical to
// several_locales_with_common_translation_strings. I think there was a
// copy-paste mistake...
TEST(Test_Translation_Table_Compiler, mapping_entries_are_relative) {
  Monotonic_Allocator allocator("test");
  PO_Entry fr_entries[] = {
      {u8"hello"_sv, u8"bonjour"_sv},
  };
  PO_Entry de_entries[] = {
      {u8"hello"_sv, u8"hallo"_sv},
  };
  String8_View untranslated_strings[] = {
      u8"hello"_sv,
  };
  PO_File files[] = {
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
      {.locale = u8"de_DE"_sv, .entries = Span<PO_Entry>(de_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  Translation_Table_Mapping_Entry* hello_entry =
      table.look_up_mapping_by_untranslated(u8"hello"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[0]), u8"hallo"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[1]), u8"bonjour"_sv);
}

TEST(Test_Translation_Table_Compiler, fuzzy_translations_are_ignored) {
  Monotonic_Allocator allocator("test");
  PO_Entry fr_entries[] = {
      {.msgid = u8"hello"_sv, .msgstr = u8"bonjour"_sv, .is_fuzzy = false},
      {.msgid = u8"goodbye"_sv, .msgstr = u8"au revoir"_sv, .is_fuzzy = true},
  };
  String8_View untranslated_strings[] = {
      u8"hello"_sv,
      u8"goodbye"_sv,
  };
  PO_File files[] = {
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  int fr = 0;

  Translation_Table_Mapping_Entry* goodbye_entry =
      table.look_up_mapping_by_untranslated(u8"goodbye"_sv);
  EXPECT_EQ(goodbye_entry->string_offsets[fr], 0)
      << "fuzzy entry should not be translated";

  Translation_Table_Mapping_Entry* hello_entry =
      table.look_up_mapping_by_untranslated(u8"hello"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[fr]), u8"bonjour"_sv)
      << "non-fuzzy entry should still be translated";
}

TEST(Test_Translation_Table_Compiler, untranslated_entries_are_ignored) {
  Monotonic_Allocator allocator("test");
  PO_Entry fr_entries[] = {
      {u8"hello"_sv, u8"bonjour"_sv},
      {u8"goodbye"_sv, u8""_sv},
  };
  String8_View untranslated_strings[] = {
      u8"hello"_sv,
      u8"goodbye"_sv,
  };
  PO_File files[] = {
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  int fr = 0;

  Translation_Table_Mapping_Entry* goodbye_entry =
      table.look_up_mapping_by_untranslated(u8"goodbye"_sv);
  EXPECT_EQ(goodbye_entry->string_offsets[fr], 0)
      << "untranslated entry should not be translated";

  Translation_Table_Mapping_Entry* hello_entry =
      table.look_up_mapping_by_untranslated(u8"hello"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[fr]), u8"bonjour"_sv)
      << "translated entry should still be translated";
}

TEST(Test_Translation_Table_Compiler,
     untranslated_entries_are_after_last_locale) {
  Monotonic_Allocator allocator("test");
  PO_Entry de_entries[] = {
      {u8"hello"_sv, u8"hallo"_sv},
  };
  PO_Entry fr_entries[] = {
      {u8"hello"_sv, u8"bonjour"_sv},
  };
  String8_View untranslated_strings[] = {
      u8"hello"_sv,
      u8"goodbye"_sv,
  };
  PO_File files[] = {
      {.locale = u8"de_DE"_sv, .entries = Span<PO_Entry>(de_entries)},
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  int de = 0;
  int fr = de + 1;
  int untranslated = fr + 1;  // After all locales.

  Translation_Table_Mapping_Entry* hello_entry =
      table.look_up_mapping_by_untranslated(u8"hello"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[untranslated]),
            u8"hello"_sv);

  Translation_Table_Mapping_Entry* goodbye_entry =
      table.look_up_mapping_by_untranslated(u8"goodbye"_sv);
  EXPECT_EQ(table.read_string(goodbye_entry->string_offsets[untranslated]),
            u8"goodbye"_sv);
}

TEST(Test_Translation_Table_Compiler,
     translations_without_untranslated_string_are_ignored) {
  Monotonic_Allocator allocator("test");
  PO_Entry fr_entries[] = {
      {u8"hello"_sv, u8"bonjour"_sv},
      {u8"goodbye"_sv, u8"au revoir"_sv},
  };
  String8_View untranslated_strings[] = {
      u8"hello"_sv,
  };
  PO_File files[] = {
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  int fr = 0;

  Translation_Table_Mapping_Entry* goodbye_entry =
      table.look_up_mapping_by_untranslated(u8"goodbye"_sv);
  EXPECT_EQ(goodbye_entry, nullptr)
      << "translated string without associated untranslated string should not "
         "have an entry at all";

  Translation_Table_Mapping_Entry* hello_entry =
      table.look_up_mapping_by_untranslated(u8"hello"_sv);
  EXPECT_EQ(table.read_string(hello_entry->string_offsets[fr]), u8"bonjour"_sv)
      << "translated string with associated untranslated string string should "
         "exist";
}

TEST(Test_Translation_Table_Compiler,
     relative_mappings_are_0_for_missing_strings) {
  Monotonic_Allocator allocator("test");
  PO_Entry fr_entries[] = {
      {u8"a"_sv, u8"[a]"_sv},
      {u8"b"_sv, u8"[b]"_sv},
      {u8"d"_sv, u8"[d]"_sv},
  };
  String8_View untranslated_strings[] = {
      u8"a"_sv,
      u8"b"_sv,
      u8"c"_sv,
      u8"d"_sv,
  };
  PO_File files[] = {
      {.locale = u8"fr_FR"_sv, .entries = Span<PO_Entry>(fr_entries)},
  };
  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(files),
      Span<const String8_View>(untranslated_strings), &allocator);
  check_table_integrity(table);

  // clang-format off
  Char8 expected_string_table[] = {
    0x0,                    // 0: ""
    0x5b, 0x61, 0x5d, 0x0,  // 1: "[a]"
    0x5b, 0x62, 0x5d, 0x0,  // 5: "[b]"
    0x5b, 0x64, 0x5d, 0x0,  // 9: "[d]"
    0x61, 0x0,              // 13: "a"
    0x62, 0x0,              // 15: "b"
    0x63, 0x0,              // 17: "c"
    0x64, 0x0,              // 19: "d"
  };
  // clang-format on
  EXPECT_THAT(table.string_table, ElementsAreArray(expected_string_table));

  Span_Size fr = 0;
  Span<Translation_Table_Mapping_Entry> r = table.relative_mapping_table;
  // clang-format off
  EXPECT_EQ(r[1].string_offsets[fr], 1) << "'a' -> '[a]' (1) relative to 0";
  EXPECT_EQ(r[2].string_offsets[fr], 4) << "'b' -> '[b]' (5) relative to '[a]' (1)";
  EXPECT_EQ(r[3].string_offsets[fr], 0) << "'c' -> (none)";
  EXPECT_EQ(r[4].string_offsets[fr], 4) << "'d' -> '[d]' (9) relative to '[b]' (5)";
  // clang-format on
}

void check_table_integrity(const Compiled_Translation_Table& table) {
  Monotonic_Allocator allocator("check_table_integrity");

  EXPECT_FALSE(table.absolute_mapping_table.empty())
      << "mapping table should never be empty";
  for (Span_Size i = 0; i < table.const_lookup_table.size(); ++i) {
    EXPECT_FALSE(table.const_lookup_table[i].untranslated.empty())
        << "const original table entry " << i
        << " should not have empty Untranslated";
  }

  // +1 is for the untranslated string's slot. See
  // NOTE[untranslated-locale-slot].
  Span_Size string_slot_count = table.locales.size() + 1;

  if (!table.absolute_mapping_table.empty()) {
    const Translation_Table_Mapping_Entry& null_mapping =
        table.absolute_mapping_table[0];
    for (Span_Size j = 0; j < null_mapping.string_offsets.size(); ++j) {
      std::uint32_t string_offset = null_mapping.string_offsets[j];
      EXPECT_EQ(table.string_table[string_offset], u8'\0')
          << "first mapping entry locale " << j
          << " should point to empty string";
    }
  }
  for (Span_Size i = 0; i < table.absolute_mapping_table.size(); ++i) {
    const Translation_Table_Mapping_Entry& mapping_entry =
        table.absolute_mapping_table[i];
    EXPECT_EQ(mapping_entry.string_offsets.size(), string_slot_count)
        << "mapping table entry " << i
        << " should have one string per locale, plus an entry for the "
           "untranslated string (see NOTE[untranslated-locale-slot])";

    for (Span_Size j = 0; j < mapping_entry.string_offsets.size(); ++j) {
      String8_View string_data =
          table.string_table.substr(mapping_entry.string_offsets[j]);
      EXPECT_THAT(string_data, ::testing::Contains(0))
          << "mapping table entry " << i << " locale " << j
          << " should point to string with null terminator\n"
          << "string data: " << out_string8(string_data);
    }
  }

  EXPECT_EQ(table.absolute_mapping_table.size(),
            table.relative_mapping_table.size());
  Span<std::uint32_t> last_present_string_offsets =
      allocator.allocate_span<std::uint32_t>(string_slot_count);
  for (Span_Size i = 1; i < table.absolute_mapping_table.size(); ++i) {
    const Translation_Table_Mapping_Entry& absolute_entry =
        table.absolute_mapping_table[i];
    const Translation_Table_Mapping_Entry& relative_entry =
        table.relative_mapping_table[i];
    for (Span_Size j = 0; j < string_slot_count; ++j) {
      std::uint32_t string_offset = absolute_entry.string_offsets[j];
      std::uint32_t expected_relative_offset =
          string_offset == 0 ? 0
                             : string_offset - last_present_string_offsets[j];
      EXPECT_EQ(relative_entry.string_offsets[j], expected_relative_offset)
          << "relative mapping table entry " << i << " locale " << j;
      if (string_offset != 0) {
        last_present_string_offsets[j] = string_offset;
      }
    }
  }
}
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
