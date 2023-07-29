// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/i18n/po-parser.h>
#include <quick-lint-js/i18n/translation-table-compiler.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/reflection/cxx-parser.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/utf-8.h>
#include <string_view>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wshadow=compatible-local")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
Span<PO_Entry> parse_po_file(const char* po_file_path, Monotonic_Allocator*);

String8_View po_path_to_locale_name(const char* po_path);

struct String_Table {
  struct Entry {
    // Copies the string.
    explicit Entry(String8_View string, Monotonic_Allocator* allocator);

    Bump_Vector<const char*, Monotonic_Allocator> origin_file_paths;
    String8_View string;
  };

  void add_string(String8_View string, const char* origin_file_path);

  Linked_Vector<Entry> entries{&this->allocator_};
  Hash_Map<String8_View, Entry*> string_to_entry;

  Monotonic_Allocator allocator_{"String_Table::allocator_"};
};

void find_strings_in_file(const char* input_path,
                          String_Table& out_string_table);
void write_messages_po_template(const String_Table&, const char* path);
void write_po_string_literal(Output_Stream&, String8_View);

void write_file_header(Output_Stream&);
void write_translation_table_header(const Compiled_Translation_Table&,
                                    const char* path);
void write_translation_table_source(const Compiled_Translation_Table&,
                                    const char* path);
void write_translation_test_header(
    Span<const PO_File>, Span<const String8_View> untranslated_strings,
    const char* path);
void write_copyright_footer(Output_Stream&);

void dump_string_table(String8_View strings, String8_View line_prefix,
                       Output_Stream&);
void dump_string_literal_body(String8_View, Output_Stream&);
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  std::vector<const char*> po_file_paths;
  std::vector<const char*> source_file_paths;
  const char* output_messages_pot_path = nullptr;
  const char* output_translation_table_cpp_path = nullptr;
  const char* output_translation_table_h_path = nullptr;
  const char* output_translation_table_test_path = nullptr;
  Arg_Parser parser(argc, argv);
  QLJS_ARG_PARSER_LOOP(parser) {
    QLJS_ARGUMENT(const char* argument) {
      std::fprintf(stderr, "error: unrecognized option: %s\n", argument);
      std::exit(2);
    }

    QLJS_OPTION(const char* arg_value, "--po"sv) {
      po_file_paths.push_back(arg_value);
    }

    QLJS_OPTION(const char* arg_value, "--source"sv) {
      source_file_paths.push_back(arg_value);
    }

    QLJS_OPTION(const char* arg_value, "--output-messages-pot"sv) {
      output_messages_pot_path = arg_value;
    }

    QLJS_OPTION(const char* arg_value, "--output-translation-table-cpp"sv) {
      output_translation_table_cpp_path = arg_value;
    }

    QLJS_OPTION(const char* arg_value, "--output-translation-table-h"sv) {
      output_translation_table_h_path = arg_value;
    }

    QLJS_OPTION(const char* arg_value, "--output-translation-table-test"sv) {
      output_translation_table_test_path = arg_value;
    }

    QLJS_UNRECOGNIZED_OPTION(const char* unrecognized) {
      std::fprintf(stderr, "error: unrecognized option: %s\n", unrecognized);
      std::exit(2);
    }
  }

  String_Table string_table;
  for (const char* source_file_path : source_file_paths) {
    find_strings_in_file(source_file_path, string_table);
  }
  write_messages_po_template(string_table, output_messages_pot_path);

  Monotonic_Allocator allocator("main");
  Bump_Vector<PO_File, Monotonic_Allocator> po_files("PO files", &allocator);
  for (const char* po_file_path : po_file_paths) {
    po_files.push_back(PO_File{
        .locale = po_path_to_locale_name(po_file_path),
        .entries = parse_po_file(po_file_path, &allocator),
    });
  }

  // TODO(strager): Reuse String_Table instead of parsing the messages.pot file
  // we just wrote.
  {
    PO_File& template_file = po_files.push_back(PO_File{
        .locale = u8""_sv,
        .entries = parse_po_file(output_messages_pot_path, &allocator),
    });
    for (PO_Entry& entry : template_file.entries) {
      // Force the source strings to map to themselves.
      entry.msgstr = entry.msgid;
    }
  }

  Span<const String8_View> untranslated_strings =
      get_all_untranslated(Span<const PO_File>(po_files), &allocator);

  Compiled_Translation_Table table = compile_translation_table(
      Span<const PO_File>(po_files), untranslated_strings, &allocator);
  write_translation_table_header(table, output_translation_table_h_path);
  write_translation_table_source(table, output_translation_table_cpp_path);
  write_translation_test_header(Span<const PO_File>(po_files),
                                untranslated_strings,
                                output_translation_table_test_path);

  return 0;
}

namespace quick_lint_js {
namespace {
Span<PO_Entry> parse_po_file(const char* po_file_path,
                             Monotonic_Allocator* allocator) {
  Result<Padded_String, Read_File_IO_Error> po_file_data =
      read_file(po_file_path);
  if (!po_file_data.ok()) {
    std::fprintf(stderr, "fatal: %s\n", po_file_data.error_to_string().c_str());
    std::exit(1);
  }
  CLI_Locator locator(&*po_file_data);
  Span<PO_Entry> entries =
      parse_po_file(&*po_file_data, po_file_path, &locator, allocator);
  // Sort to make output deterministic.
  sort(entries, [](const PO_Entry& lhs, const PO_Entry& rhs) {
    return lhs.msgid < rhs.msgid;
  });
  return entries;
}

void write_file_header(Output_Stream& out) {
  out.append_copy(
      u8"// Code generated by tools/compile-translations.cpp. DO NOT EDIT.\n"_sv);
  // TODO(strager): List all input files.
  out.append_copy(u8"// source: po/*.po\n\n"_sv);
  out.append_copy(u8"// Copyright (C) 2020  Matthew \"strager\" Glazar\n"_sv);
  out.append_copy(
      u8"// See end of file for extended copyright information.\n"_sv);
}

String8_View po_path_to_locale_name(const char* po_path) {
  std::string_view file_name = path_file_name(po_path);
  std::size_t extension_start = file_name.find('.');
  if (extension_start == file_name.npos) {
    std::fprintf(stderr,
                 "fatal: failed to determine locale name from path: %s\n",
                 po_path);
    std::exit(1);
  }
  std::string_view base_name = file_name.substr(0, extension_start);
  return to_string8_view(base_name);
}

String_Table::Entry::Entry(String8_View string, Monotonic_Allocator* allocator)
    : origin_file_paths("String_Table::Entry::file_paths", allocator) {
  Char8* new_string =
      allocator->allocate_uninitialized_array<Char8>(string.size());
  Char8* new_string_end = std::copy(string.begin(), string.end(), new_string);
  this->string = make_string_view(new_string, new_string_end);
}

void String_Table::add_string(String8_View string,
                              const char* origin_file_path) {
  auto existing_it = this->string_to_entry.find(string);
  Entry* entry;
  if (existing_it == this->string_to_entry.end()) {
    entry = &this->entries.emplace_back(string, &this->allocator_);
    auto [_it, inserted] =
        this->string_to_entry.try_emplace(entry->string, entry);
    QLJS_ASSERT(inserted);
  } else {
    entry = existing_it->second;
  }

  if (!contains(entry->origin_file_paths, origin_file_path)) {
    entry->origin_file_paths.push_back(origin_file_path);
  }
}

void find_strings_in_file(const char* input_path,
                          String_Table& out_string_table) {
  Result<Padded_String, Read_File_IO_Error> file = read_file(input_path);
  if (!file.ok()) {
    std::fprintf(stderr, "fatal: %s\n", file.error_to_string().c_str());
    std::exit(1);
  }
  CLI_Locator locator(&*file);

  Padded_String_View remaining = &*file;
  for (;;) {
    static constexpr String8_View marker = u8"QLJS_TRANSLATABLE("_sv;
    std::size_t marker_index = remaining.string_view().find(marker);
    if (marker_index == String8_View::npos) {
      break;
    }
    std::size_t string_index = marker_index + marker.size();
    remaining = remaining.substr(narrow_cast<Padded_String_Size>(string_index));

    CXX_Lexer lexer(remaining, input_path, &locator);
    if (lexer.peek().type != CXX_Token_Type::string_literal) {
      lexer.fatal();
    }
    out_string_table.add_string(lexer.peek().decoded_string, input_path);
    remaining =
        Padded_String_View(lexer.remaining(), remaining.null_terminator());
  }
}

void write_messages_po_template(const String_Table& strings,
                                Output_Stream& out) {
  out.append_copy(
      u8R"(# Code generated by tools/update-translator-sources. DO NOT EDIT.
# source: (various)
# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew "strager" Glazar
#
# This file is part of quick-lint-js.
#
# quick-lint-js is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# quick-lint-js is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: PACKAGE VERSION\n"
"Report-Msgid-Bugs-To: strager.nds@gmail.com\n"
"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"Language: \n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"
)"_sv);

  strings.entries.for_each([&](const String_Table::Entry& entry) {
    for (const char* origin_file_path : entry.origin_file_paths) {
      out.append_copy(u8"\n#: "_sv);
      out.append_copy(to_string8_view(origin_file_path));
    }
    out.append_copy(u8"\nmsgid "_sv);
    write_po_string_literal(out, entry.string);
    out.append_copy(u8"\nmsgstr \"\"\n"_sv);
  });
}

void write_messages_po_template(const String_Table& strings, const char* path) {
  Result<Platform_File, Write_File_IO_Error> file = open_file_for_writing(path);
  if (!file.ok()) {
    std::fprintf(stderr, "error: %s\n", file.error_to_string().c_str());
    std::exit(1);
  }
  File_Output_Stream out(file->ref());
  write_messages_po_template(strings, out);
  out.flush();
}

void write_po_string_literal(Output_Stream& out, String8_View string) {
  out.append_copy(u8'"');
  for (Char8 c : string) {
    if (c == u8'"' || c == u8'\\') {
      out.append_copy(u8'\\');
      out.append_copy(c);
    } else {
      // TODO(strager): Escape other characters?
      out.append_copy(c);
    }
  }
  out.append_copy(u8'"');
}

void write_translation_table_header(const Compiled_Translation_Table& table,
                                    Output_Stream& out) {
  write_file_header(out);

  out.append_copy(
      u8R"(
#ifndef QUICK_LINT_JS_I18N_TRANSLATION_TABLE_GENERATED_H
#define QUICK_LINT_JS_I18N_TRANSLATION_TABLE_GENERATED_H

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/sorted-search.h>
#include <quick-lint-js/i18n/translation-table.h>
#include <quick-lint-js/port/consteval.h>
#include <string_view>

namespace quick_lint_js {
using namespace std::literals::string_view_literals;

)"_sv);
  out.append_copy(
      u8"constexpr std::uint32_t translation_table_locale_count = "_sv);
  out.append_decimal_integer(table.locales.size() - 1);
  out.append_copy(u8";\n"_sv);
  out.append_copy(
      u8"constexpr std::uint16_t translation_table_mapping_table_size = "_sv);
  out.append_decimal_integer(table.relative_mapping_table.size());
  out.append_copy(u8";\n"_sv);
  out.append_copy(
      u8"constexpr std::size_t translation_table_string_table_size = "_sv);
  out.append_decimal_integer(table.string_table.size());
  out.append_copy(u8";\n"_sv);
  out.append_copy(
      u8"constexpr std::size_t translation_table_locale_table_size = "_sv);
  out.append_decimal_integer(table.locale_table.size());
  out.append_copy(u8";\n"_sv);
  out.append_copy(u8"\n"_sv);

  out.append_copy(
      u8R"(QLJS_CONSTEVAL std::uint16_t translation_table_const_look_up(
    std::string_view untranslated) {
  // clang-format off
  constexpr std::string_view const_lookup_table[] = {
)"_sv);
  for (const Translation_Table_Const_Lookup_Entry& const_lookup_entry :
       table.const_lookup_table) {
    out.append_copy(u8"          \""_sv);
    dump_string_literal_body(const_lookup_entry.untranslated, out);
    out.append_copy(u8"\"sv,\n"_sv);
  }
  out.append_copy(
      u8R"(  };
  // clang-format on

  auto it = sorted_search(std::begin(const_lookup_table),
                          std::end(const_lookup_table), untranslated);
  if (it != std::end(const_lookup_table)) {
    return std::uint16_t((it - std::begin(const_lookup_table)) + 1);
  }

  // If you see an error with the following line, translation-table-generated.h
  // is out of date. Run tools/update-translator-sources to rebuild this file.
  QLJS_CONSTEXPR_ASSERT(false);

  return 0;
}
}

#endif

)"_sv);
  write_copyright_footer(out);
}

void write_translation_table_header(const Compiled_Translation_Table& table,
                                    const char* path) {
  Result<Platform_File, Write_File_IO_Error> file = open_file_for_writing(path);
  if (!file.ok()) {
    std::fprintf(stderr, "error: %s\n", file.error_to_string().c_str());
    std::exit(1);
  }
  File_Output_Stream out(file->ref());
  write_translation_table_header(table, out);
  out.flush();
}

void write_translation_table_source(const Compiled_Translation_Table& table,
                                    Output_Stream& out) {
  write_file_header(out);

  out.append_copy(
      u8R"(
#include <array>
#include <quick-lint-js/i18n/translation-table.h>

namespace quick_lint_js {
const Translation_Table translation_data = {
    .mapping_table = Translation_Table::absolute_mapping_table_from_relative({{
)"_sv);
  std::vector<String8> mapping_table_lines;
  std::size_t max_line_length = 0;
  Memory_Output_Stream temp;
  for (const Translation_Table_Mapping_Entry& mapping_entry :
       table.relative_mapping_table) {
    temp.append_copy(u8"        {"_sv);
    bool need_comma = false;
    for (std::uint32_t string_offset : mapping_entry.string_offsets) {
      if (need_comma) {
        temp.append_copy(u8", "_sv);
      }
      temp.append_decimal_integer(string_offset);
      need_comma = true;
    }
    temp.append_copy(u8"},"_sv);
    temp.flush();
    String8 line = temp.get_flushed_string8();
    temp.clear();
    if (line.size() > max_line_length) {
      max_line_length = line.size();
    }
    mapping_table_lines.push_back(std::move(line));
  }
  for (String8& line : mapping_table_lines) {
    out.append_copy(line);
    for (std::size_t i = 0; i < max_line_length - line.size(); ++i) {
      out.append_copy(u8' ');
    }
    out.append_copy(u8"  //\n"_sv);
  }
  out.append_copy(
      u8R"(    }}),

    // clang-format off
    .string_table =
)"_sv);
  dump_string_table(table.string_table, u8"        u8"_sv, out);

  out.append_copy(
      u8R"(,
    // clang-format on

    .locale_table =
)"_sv);

  dump_string_table(table.locale_table, u8"        ", out);

  out.append_copy(
      u8R"(,
};
}

)"_sv);
  write_copyright_footer(out);
}

void write_translation_table_source(const Compiled_Translation_Table& table,
                                    const char* path) {
  Result<Platform_File, Write_File_IO_Error> file = open_file_for_writing(path);
  if (!file.ok()) {
    std::fprintf(stderr, "error: %s\n", file.error_to_string().c_str());
    std::exit(1);
  }
  File_Output_Stream out(file->ref());
  write_translation_table_source(table, out);
  out.flush();
}

void write_translation_test_header(
    Span<const PO_File> po_files, Span<const String8_View> untranslated_strings,
    Output_Stream& out) {
  Monotonic_Allocator allocator("write_translation_test_header");

  Bump_Vector<String8_View, Monotonic_Allocator> locale_names(
      "compile_translation_table locale_names", &allocator);
  for (const PO_File& file : po_files) {
    if (!file.locale.empty()) {  // TODO(strager): Remove this 'if'.
      locale_names.push_back(file.locale);
    }
  }
  locale_names.push_back(u8""_sv);  // Untranslated locale.
  // Sort to make output deterministic.
  sort(locale_names);

  // Returns the untranslated string if there is no translation.
  auto look_up_translation = [&](String8_View locale_name,
                                 String8_View untranslated) -> String8_View {
    for (const PO_File& po_file : po_files) {
      if (po_file.locale == locale_name) {
        for (const PO_Entry& entry : po_file.entries) {
          if (entry.has_translation() && entry.msgid == untranslated) {
            return entry.msgstr;
          }
        }
      }
    }
    return untranslated;
  };

  write_file_header(out);

  out.append_copy(
      u8R"(
#ifndef QUICK_LINT_JS_TEST_TRANSLATION_TABLE_GENERATED_H
#define QUICK_LINT_JS_TEST_TRANSLATION_TABLE_GENERATED_H

#include <quick-lint-js/i18n/translation.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
// clang-format off
inline constexpr const char *test_locale_names[] = {
)"_sv);

  for (String8_View locale_name : locale_names) {
    out.append_copy(u8"    \""_sv);
    dump_string_literal_body(locale_name, out);
    out.append_copy(u8"\",\n"_sv);
  }

  out.append_copy(
      u8R"(};
// clang-format on

struct Translated_String {
  Translatable_Message translatable;
  const Char8 *expected_per_locale[)"_sv);
  out.append_decimal_integer(locale_names.size());
  out.append_copy(
      u8R"(];
};

// clang-format off
inline const Translated_String test_translation_table[)"_sv);
  out.append_decimal_integer(untranslated_strings.size());
  out.append_copy(
      u8R"(] = {
)"_sv);

  for (String8_View untranslated : untranslated_strings) {
    out.append_copy(u8"    {\n        \""_sv);
    dump_string_literal_body(untranslated, out);
    out.append_copy(u8"\"_translatable,\n        {\n"_sv);
    for (String8_View locale_name : locale_names) {
      out.append_copy(u8"            u8\""_sv);
      dump_string_literal_body(look_up_translation(locale_name, untranslated),
                               out);
      out.append_copy(u8"\",\n"_sv);
    }
    out.append_copy(u8"        },\n    },\n"_sv);
  }

  out.append_copy(
      u8R"(};
// clang-format on
}

#endif

)"_sv);
  write_copyright_footer(out);
}

void write_translation_test_header(
    Span<const PO_File> po_files, Span<const String8_View> untranslated_strings,
    const char* path) {
  Result<Platform_File, Write_File_IO_Error> file = open_file_for_writing(path);
  if (!file.ok()) {
    std::fprintf(stderr, "error: %s\n", file.error_to_string().c_str());
    std::exit(1);
  }
  File_Output_Stream out(file->ref());
  write_translation_test_header(po_files, untranslated_strings, out);
  out.flush();
}

void write_copyright_footer(Output_Stream& out) {
  out.append_copy(
      u8R"(// quick-lint-js finds bugs in JavaScript programs.
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
)"_sv);
}

void dump_string_table(String8_View strings, String8_View line_prefix,
                       Output_Stream& out) {
  while (!strings.empty()) {
    out.append_copy(line_prefix);
    out.append_copy(u8'"');
    std::size_t string_length = strings.find(u8'\0');
    QLJS_ASSERT(string_length != strings.npos);
    String8_View s = strings.substr(0, string_length);
    dump_string_literal_body(s, out);
    strings = strings.substr(string_length + 1);
    if (!strings.empty()) {
      // C++ adds a \0 for us, so we don't need to add one ourselves.
      out.append_copy(u8"\\0"_sv);
    }
    out.append_copy(u8'"');
    if (!strings.empty()) {
      out.append_copy(u8'\n');
    }
  }
}

void dump_string_literal_body(String8_View s, Output_Stream& out) {
  // FIXME(strager): We should avoid this copy. decode_utf_8 should accept an
  // unpadded String8_View.
  Padded_String s_copy(s);
  Padded_String_View s_view(&s_copy);
  while (!s_view.empty()) {
    Decode_UTF8_Result decode_result = decode_utf_8(s_view);
    QLJS_ASSERT(decode_result.ok);
    char32_t c = decode_result.code_point;
    if (c < 0x20 || c >= 0x7f) {
      if (c >= 0x10000) {
        out.append_copy(u8"\\U"_sv);
        out.append_fixed_hexadecimal_integer(c, 8);
      } else {
        out.append_copy(u8"\\u"_sv);
        out.append_fixed_hexadecimal_integer(c, 4);
      }
    } else if (c == U'\\' || c == U'"') {
      out.append_copy(u8'\\');
      out.append_copy(static_cast<Char8>(c));
    } else {
      out.append_copy(static_cast<Char8>(c));
    }
    s_view = s_view.substr(decode_result.size);
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
