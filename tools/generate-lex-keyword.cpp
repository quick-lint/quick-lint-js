// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// generate-lex-keyword creates a perfect hash table. The input is
// quick-lint-js' list of keywords, and the output is C++ code to look up in
// that perfect hash table (lex-keyword-generated.cpp).
//
// generate-lex-keyword works by picking a table size and seeding a hash
// function [1] with an arbitrary integer. If that hash function causes a
// collision between any two keys, a different seed is tried. If, after a few
// thousand attempts, no collision-free seed was found, the table size is
// increased and the hashing is attempted again.
//
// [1] See the Keyword_Lexer class in <quick-lint-js/fe/keyword-lexer.h> for
//     details on the hash function.

#include <algorithm>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/fe/keyword-lexer.h>
#include <quick-lint-js/fe/keyword-list.h>
#include <quick-lint-js/generated-source.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/bit.h>
#include <quick-lint-js/port/type-traits.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
struct Generate_Lex_Keyword_Options {
  const char* output_path = nullptr;
};

Generate_Lex_Keyword_Options parse_generate_lex_keyword_options(int argc,
                                                                char** argv) {
  Generate_Lex_Keyword_Options o;

  Arg_Parser parser(argc, argv);
  QLJS_ARG_PARSER_LOOP(parser) {
    QLJS_ARGUMENT(const char* argument) {
      std::fprintf(stderr, "error: unexpected argument: %s\n", argument);
      std::exit(2);
    }

    QLJS_OPTION(const char* arg_value, "--output"sv) {
      o.output_path = arg_value;
    }

    QLJS_UNRECOGNIZED_OPTION(const char* unrecognized) {
      std::fprintf(stderr, "error: unrecognized option: %s\n", unrecognized);
      std::exit(2);
    }
  }

  return o;
}

void analyze_keys(const std::vector<std::string_view>& keys) {
  bool ok = true;

  for (std::string_view key : keys) {
    std::size_t length = key.size();
    if (length < Keyword_Lexer::minimum_key_length) {
      std::fprintf(stderr,
                   "error: minimum keyword length is %zu, but found keyword "
                   "with length %zu: %.*s\n",
                   Keyword_Lexer::minimum_key_length, length,
                   narrow_cast<int>(length), key.data());
      ok = false;
    }
    if (length > Keyword_Lexer::maximum_key_length) {
      std::fprintf(stderr,
                   "error: maximum keyword length is %zu, but found keyword "
                   "with length %zu: %.*s\n",
                   Keyword_Lexer::maximum_key_length, length,
                   narrow_cast<int>(length), key.data());
      ok = false;
    }
  }

  if (ok) {  // Do not call select() with invalid keys.
    std::unordered_map<Keyword_Lexer::Selection_Type, std::string_view>
        selections;
    for (std::string_view key : keys) {
      Keyword_Lexer::Selection_Type selection =
          Keyword_Lexer::select(key.data(), key.size());
      auto collision = selections.find(selection);
      if (collision != selections.end()) {
        std::fprintf(stderr,
                     "error: character selection algorithm detected collision "
                     "between keys:\n  %.*s\n  %.*s\n",
                     narrow_cast<int>(collision->second.size()),
                     collision->second.data(), narrow_cast<int>(key.size()),
                     key.data());
        ok = false;
      }
      selections.emplace(selection, key);
    }
  }

  if (!ok) {
    std::exit(1);
  }
}

class Table_Seed {
 public:
  Keyword_Lexer::Seed_Type get() const { return this->seed_; }

  void next() { this->seed_ += 1; }

 private:
  // Arbitrary.
  Keyword_Lexer::Seed_Type seed_ = 0x811c9dc5;  // FNV-1a 32-bit basis.
};

struct Hash_Table {
  struct Table_Entry {
    std::string_view key;

    // If 'generation' is out of sync with the hash_table's 'generation', then
    // this entry is vacant thus other variables in this entry should be
    // ignored.
    std::uint32_t generation = 0;

    bool is_taken(std::uint32_t current_generation) const {
      return current_generation == this->generation && !this->key.empty();
    }

    void take(std::string_view key, std::uint32_t generation) {
      QLJS_ASSERT(!key.empty());
      this->key = key;
      this->generation = generation;
    }
  };

  std::vector<Table_Entry> entries;
  std::uint32_t current_generation = 0;
  Table_Seed seed;

  bool try_fill(const std::vector<std::string_view>& keys) {
    this->current_generation += 1;
    for (std::string_view key : keys) {
      Keyword_Lexer::Selection_Type selection =
          Keyword_Lexer::select(key.data(), key.size());
      Keyword_Lexer::Hash_Type hash =
          Keyword_Lexer::mix(selection, this->seed.get());
      Keyword_Lexer::Hash_Type index =
          static_cast<Keyword_Lexer::Hash_Type>(hash % this->entries.size());
      Table_Entry& entry = this->entries[index];
      if (entry.is_taken(this->current_generation)) {
        return false;
      }
      entry.take(key, this->current_generation);
    }
    return true;
  }
};

Hash_Table make_hash_table(const std::vector<std::string_view>& keys) {
  constexpr int attempts_per_table_size = 80'000;
  constexpr std::size_t max_table_size = 1024;

  std::size_t table_size = bit_ceil(keys.size());
  for (;;) {
    Hash_Table t;
    t.entries.resize(table_size);
    for (int attempt = 0; attempt < attempts_per_table_size; ++attempt) {
      if (t.try_fill(keys)) {
        return t;
      }
      t.seed.next();
    }

    std::size_t next_table_size = table_size * 2;
    if (next_table_size > max_table_size) {
      std::fprintf(stderr,
                   "error: failed to generate table; gave up after trying "
                   "table size %zu\n",
                   table_size);
      std::exit(1);
    }
    table_size = next_table_size;
  }
}

struct String_Table {
  std::string data;
  std::vector<std::size_t> word_starts;

  bool is_word_start(std::size_t data_index) const {
    return std::find(this->word_starts.begin(), this->word_starts.end(),
                     data_index) != this->word_starts.end();
  }
};

String_Table make_string_table(const std::vector<std::string_view>& keys) {
  String_Table strings;

  // Sorting in reverse order lets us merge common prefixes, like for
  // "as", "assert", and "asserts".
  std::vector<std::string_view> sorted_keys(keys);
  std::sort(
      sorted_keys.begin(), sorted_keys.end(),
      [](std::string_view a, std::string_view b) -> bool { return a > b; });

  for (std::string_view key : sorted_keys) {
    if (strings.data.find(key) == strings.data.npos) {
      strings.word_starts.push_back(strings.data.size());
      strings.data.append(key);
    }
  }

  strings.word_starts.push_back(strings.data.size());
  strings.data.resize(strings.data.size() + Keyword_Lexer::padding_size);

  return strings;
}

void dump_table_code(const Hash_Table& t, const String_Table& strings,
                     Output_Stream& out) {
  out.append_literal(
      u8R"(// Code generated by tools/generate-lex-keyword.cpp. DO NOT EDIT.
// source: src/quick-lint-js/fe/keyword-lexer.h
// source: src/quick-lint-js/fe/keyword-list.h

)"_sv);
  write_file_copyright_begin(out);

  out.append_literal(
      u8R"(#include <cstddef>
#include <cstdint>
#include <quick-lint-js/fe/keyword-lexer.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
namespace {
)"_sv);

  out.append_literal(u8"constexpr std::size_t hash_table_size = "_sv);
  out.append_decimal_integer(t.entries.size());
  out.append_literal(u8"LLU;\n"_sv);
  out.append_literal(u8"constexpr std::size_t string_table_size = "_sv);
  out.append_decimal_integer(strings.data.size());
  out.append_literal(u8"LLU;\n"_sv);
  out.append_literal(u8"\n"_sv);
  out.append_literal(u8"constexpr Keyword_Lexer::Seed_Type hash_seed = "_sv);
  out.append_decimal_integer(t.seed.get());
  out.append_literal(u8"LLU;\n"_sv);

  out.append_literal(
      u8R"(
struct Entry {
  std::uint16_t key_index;  // Index into tables::string_table.
  std::uint8_t key_length;
  Token_Type value;
};

// The two tables are combined into one table to reduce the number of
// instructions dedicated to address computation in the lookup function. (I did
// not measure whether this technique actually improves performance, though.)
struct Tables_Type {
  Entry hash_table[hash_table_size];
  Char8 string_table[string_table_size];
};

)"_sv);

  out.append_literal(u8"// clang-format off\n"_sv);
  out.append_literal(u8"constexpr Tables_Type tables = {\n"_sv);
  out.append_literal(u8"    .hash_table =\n"_sv);
  out.append_literal(u8"        {\n"_sv);
  for (const Hash_Table::Table_Entry& e : t.entries) {
    out.append_literal(u8"            {"_sv);
    if (e.is_taken(t.current_generation)) {
      out.append_decimal_integer(strings.data.find(e.key));
      out.append_literal(u8", "_sv);
      out.append_decimal_integer(e.key.size());
      out.append_literal(u8", Token_Type::kw_"_sv);
      out.append_copy(to_string8_view(e.key));
    } else {
      out.append_literal(u8"0, 0, Token_Type::identifier"_sv);
    }
    out.append_literal(u8"},\n"_sv);
  }
  out.append_literal(u8"        },\n"_sv);

  out.append_literal(u8"\n"_sv);
  out.append_literal(u8"    .string_table =\n"_sv);
  QLJS_ASSERT(!strings.data.empty());
  QLJS_ASSERT(strings.data.back() == '\0');
  for (std::size_t i = 0; i < strings.data.size() - 1; ++i) {
    if (strings.is_word_start(i)) {
      if (i != 0) {
        out.append_literal(u8"\"\n"_sv);
      }
      out.append_literal(u8"        u8\""_sv);
    }
    char c = strings.data[i];
    if (c == '\0') {
      out.append_literal(u8"\\0"_sv);
    } else {
      out.append_copy(static_cast<Char8>(c));
    }
  }
  out.append_literal(u8"\",\n"_sv);

  out.append_literal(u8"};\n"_sv);
  out.append_literal(u8"// clang-format on"_sv);

  out.append_literal(
      u8R"(
}

Token_Type Lexer::identifier_token_type(String8_View identifier) {
  std::size_t identifier_size = identifier.size();

  Keyword_Lexer::Selection_Type selection =
      Keyword_Lexer::select(identifier.data(), identifier_size);
  Keyword_Lexer::Hash_Type hash = Keyword_Lexer::mix(selection, hash_seed);
  Keyword_Lexer::Hash_Type index =
      static_cast<Keyword_Lexer::Hash_Type>(hash % hash_table_size);

  const Entry& e = tables.hash_table[index];
  const Char8* e_key = &tables.string_table[e.key_index];

  // NOTE(strager): Use a result variable to encourage compilers to generate
  // conditional store instructions. Conditional stores can improve performance
  // significantly because lookups are somewhat unpredictable. (Unfortunately,
  // no compiler reliably generates conditional stores.)
  Token_Type result = e.value;
  if (!Keyword_Lexer::key_strings_equal(e_key, identifier.data(),
                                        identifier_size)) {
    result = Token_Type::identifier;
  }
  if (identifier_size != e.key_length) {
    result = Token_Type::identifier;
  }
  return result;
}
}
)"_sv);

  write_file_copyright_end(out);
}

void dump_table_code(const Hash_Table& t, const String_Table& strings,
                     const char* file_path) {
  Memory_Output_Stream out;
  dump_table_code(t, strings, out);
  out.write_file_if_different_or_exit(file_path);
}
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  Generate_Lex_Keyword_Options o =
      parse_generate_lex_keyword_options(argc, argv);
  if (o.output_path == nullptr) {
    std::fprintf(stderr, "error: missing --output path\n");
    return 2;
  }

  std::vector<std::string_view> keys = {
#define QLJS_KEYWORD(k) #k,
      QLJS_X_KEYWORDS
#undef QLJS_KEYWORD
  };

  analyze_keys(keys);
  Hash_Table t = make_hash_table(keys);
  String_Table strings = make_string_table(keys);
  dump_table_code(t, strings, o.output_path);

  return 0;
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
