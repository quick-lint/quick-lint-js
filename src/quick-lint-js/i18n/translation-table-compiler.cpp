// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/i18n/po-parser.h>
#include <quick-lint-js/i18n/translation-table-compiler.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/algorithm.h>

namespace quick_lint_js {
namespace {
struct String_Table {
 public:
  explicit String_Table(Monotonic_Allocator* allocator)
      : strings_("String_Table::strings", allocator) {}

  // Returns the offset of the string in the table.
  std::uint32_t add_string(String8_View string) {
    std::uint32_t offset = narrow_cast<std::uint32_t>(strings_.size());
    strings_ += string;
    strings_ += u8'\0';
    return offset;
  }

  String8_View freeze() {
    Span<Char8> span = this->strings_.get_and_release();
    return make_string_view(span.begin(), span.end());
  }

 private:
  Vector<Char8> strings_;
};
}

std::optional<Span_Size>
Compiled_Translation_Table::find_mapping_table_index_for_untranslated(
    String8_View original_string) {
  for (Span_Size i = 0; i < this->const_lookup_table.size(); ++i) {
    if (this->const_lookup_table[i].untranslated == original_string) {
      return i + 1;
    }
  }
  return std::nullopt;
}

Translation_Table_Mapping_Entry*
Compiled_Translation_Table::look_up_mapping_by_untranslated(
    String8_View original_string) {
  std::optional<Span_Size> index =
      this->find_mapping_table_index_for_untranslated(original_string);
  if (!index.has_value()) {
    return nullptr;
  }
  return &this->absolute_mapping_table[*index];
}

String8_View Compiled_Translation_Table::read_string(
    std::uint32_t string_offset) {
  String8_View string_data = this->string_table.substr(string_offset);
  std::size_t null_terminator_index = string_data.find(u8'\0');
  QLJS_ALWAYS_ASSERT(null_terminator_index != string_data.npos);
  return string_data.substr(0, null_terminator_index);
}

Compiled_Translation_Table compile_translation_table(
    Span<const PO_File> files, Span<const String8_View> untranslated_strings,
    Monotonic_Allocator* allocator) {
  Compiled_Translation_Table table;

  Span<const String8_View> keys = untranslated_strings;

  {
    Vector<String8_View> locale_names("compile_translation_table locale_names",
                                      allocator);
    for (const PO_File& file : files) {
      locale_names.push_back(file.locale);
    }
    // Sort to make output deterministic.
    sort(locale_names);

    table.locales = locale_names.get_and_release();
  }

  String_Table locale_table(allocator);
  for (String8_View locale_name : table.locales) {
    locale_table.add_string(locale_name);
  }
  // Write an extra null terminator so find_locale knows the bounds of the
  // locale table. See NOTE[locale-list-null-terminator].
  locale_table.add_string(u8""_sv);
  table.locale_table = locale_table.freeze();

  table.const_lookup_table =
      allocator->allocate_span<Translation_Table_Const_Lookup_Entry>(
          keys.size());
  for (Span_Size i = 0; i < keys.size(); ++i) {
    table.const_lookup_table[i].untranslated = keys[i];
  }

  // +1 is for the untranslated string's slot. See
  // NOTE[untranslated-locale-slot].
  Span_Size string_slot_count = table.locales.size() + 1;

  String_Table string_table(allocator);
  string_table.add_string(u8""_sv);
  Span_Size mapping_table_size = keys.size() + 1;
  table.absolute_mapping_table =
      allocator->allocate_span<Translation_Table_Mapping_Entry>(
          mapping_table_size);
  for (Span_Size i = 0; i < mapping_table_size; ++i) {
    Translation_Table_Mapping_Entry& mapping_entry =
        table.absolute_mapping_table[i];
    mapping_entry.string_offsets =
        allocator->allocate_span<std::uint32_t>(string_slot_count);
  }
  for (Span_Size locale_index = 0; locale_index < table.locales.size();
       ++locale_index) {
    auto file_it =
        find_unique_existing_if(files, [&](const PO_File& file) -> bool {
          return file.locale == table.locales[locale_index];
        });
    for (const PO_Entry& entry : file_it->entries) {
      if (!entry.is_metadata() && entry.has_translation()) {
        std::optional<Span_Size> index =
            table.find_mapping_table_index_for_untranslated(entry.msgid);
        if (index.has_value()) {
          table.absolute_mapping_table[*index].string_offsets[locale_index] =
              string_table.add_string(entry.msgstr);
        }
      }
    }
  }
  // Generate the untranslated slot. See NOTE[untranslated-locale-slot].
  Span_Size untranslated_locale_index = table.locales.size();
  for (String8_View untranslated_string : untranslated_strings) {
    std::optional<Span_Size> index =
        table.find_mapping_table_index_for_untranslated(untranslated_string);
    table.absolute_mapping_table[index.value()]
        .string_offsets[untranslated_locale_index] =
        string_table.add_string(untranslated_string);
  }

  table.relative_mapping_table =
      allocator->allocate_span<Translation_Table_Mapping_Entry>(
          mapping_table_size);
  table.relative_mapping_table[0].string_offsets =
      allocator->allocate_span<std::uint32_t>(string_slot_count);
  Span<std::uint32_t> last_present_string_offsets =
      allocator->allocate_span<std::uint32_t>(string_slot_count);
  for (Span_Size i = 1; i < mapping_table_size; ++i) {
    Translation_Table_Mapping_Entry* relative_entry =
        &table.relative_mapping_table[i];
    relative_entry->string_offsets =
        allocator->allocate_span<std::uint32_t>(string_slot_count);

    Translation_Table_Mapping_Entry* absolute_entry =
        &table.absolute_mapping_table[i];
    for (Span_Size slot_index = 0; slot_index < string_slot_count;
         ++slot_index) {
      std::uint32_t string_offset = absolute_entry->string_offsets[slot_index];
      if (string_offset != 0) {
        std::uint32_t previous_string_offset =
            last_present_string_offsets[slot_index];
        relative_entry->string_offsets[slot_index] =
            string_offset - previous_string_offset;
        last_present_string_offsets[slot_index] = string_offset;
      }
    }
  }

  table.string_table = string_table.freeze();
  return table;
}

Span<String8_View> get_all_untranslated(Span<const PO_File> files,
                                        Monotonic_Allocator* allocator) {
  Vector<String8_View> all_untranslated("get_all_untranslated all_untranslated",
                                        allocator);
  auto add_untranslated = [&](String8_View untranslated) -> void {
    bool is_duplicate = contains(all_untranslated, untranslated);
    if (!is_duplicate) {
      all_untranslated.push_back(untranslated);
    }
  };
  for (const PO_File& file : files) {
    for (const PO_Entry& entry : file.entries) {
      if (!entry.is_metadata()) {
        add_untranslated(entry.msgid);
      }
    }
  }
  // Sort to make output deterministic.
  sort(all_untranslated);
  return all_untranslated.get_and_release();
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
