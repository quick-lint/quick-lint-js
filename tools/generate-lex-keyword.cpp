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
// [1] See the keyword_lexer class in <quick-lint-js/fe/keyword-lexer.h> for
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
struct generate_lex_keyword_options {
  const char* output_path = nullptr;
};

generate_lex_keyword_options parse_generate_lex_keyword_options(int argc,
                                                                char** argv) {
  generate_lex_keyword_options o;

  arg_parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      std::fprintf(stderr, "error: unexpected argument: %s\n", argument);
      std::exit(2);
    } else if (const char* arg_value =
                   parser.match_option_with_value("--output"sv)) {
      o.output_path = arg_value;
    } else {
      const char* unrecognized = parser.match_anything();
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
    if (length < keyword_lexer::minimum_key_length) {
      std::fprintf(stderr,
                   "error: minimum keyword length is %zu, but found keyword "
                   "with length %zu: %.*s\n",
                   keyword_lexer::minimum_key_length, length,
                   narrow_cast<int>(length), key.data());
      ok = false;
    }
    if (length > keyword_lexer::maximum_key_length) {
      std::fprintf(stderr,
                   "error: maximum keyword length is %zu, but found keyword "
                   "with length %zu: %.*s\n",
                   keyword_lexer::maximum_key_length, length,
                   narrow_cast<int>(length), key.data());
      ok = false;
    }
  }

  if (ok) {  // Do not call select() with invalid keys.
    std::unordered_map<keyword_lexer::selection_type, std::string_view>
        selections;
    for (std::string_view key : keys) {
      keyword_lexer::selection_type selection =
          keyword_lexer::select(key.data(), key.size());
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

class table_seed {
 public:
  keyword_lexer::seed_type get() const { return this->seed_; }

  void next() { this->seed_ += 1; }

 private:
  // Arbitrary.
  keyword_lexer::seed_type seed_ = 0x811c9dc5;  // FNV-1a 32-bit basis.
};

struct hash_table {
  struct table_entry {
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

  std::vector<table_entry> entries;
  std::uint32_t current_generation = 0;
  table_seed seed;

  bool try_fill(const std::vector<std::string_view>& keys) {
    this->current_generation += 1;
    for (std::string_view key : keys) {
      keyword_lexer::selection_type selection =
          keyword_lexer::select(key.data(), key.size());
      keyword_lexer::hash_type hash =
          keyword_lexer::mix(selection, this->seed.get());
      keyword_lexer::hash_type index =
          static_cast<keyword_lexer::hash_type>(hash % this->entries.size());
      table_entry& entry = this->entries[index];
      if (entry.is_taken(this->current_generation)) {
        return false;
      }
      entry.take(key, this->current_generation);
    }
    return true;
  }
};

hash_table make_hash_table(const std::vector<std::string_view>& keys) {
  constexpr int attempts_per_table_size = 50'000;
  constexpr std::size_t max_table_size = 1024;

  std::size_t table_size = bit_ceil(keys.size());
  for (;;) {
    hash_table t;
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

struct string_table {
  std::string data;
  std::vector<std::size_t> word_starts;

  bool is_word_start(std::size_t data_index) const {
    return std::find(this->word_starts.begin(), this->word_starts.end(),
                     data_index) != this->word_starts.end();
  }
};

string_table make_string_table(const std::vector<std::string_view>& keys) {
  string_table strings;

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
  strings.data.resize(strings.data.size() + keyword_lexer::padding_size);

  return strings;
}

void dump_table_code(const hash_table& t, const string_table& strings,
                     FILE* f) {
  std::fprintf(
      f, "%s",
      R"(// Code generated by tools/generate-lex-keyword.cpp. DO NOT EDIT.
// source: src/quick-lint-js/fe/keyword-lexer.h
// source: src/quick-lint-js/fe/keyword-list.h

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/fe/keyword-lexer.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
namespace {)");

  std::fprintf(f, R"(
constexpr std::size_t hash_table_size = %zuLLU;
constexpr std::size_t string_table_size = %zuLLU;

constexpr keyword_lexer::seed_type hash_seed = %lluLLU;
)",
               t.entries.size(), strings.data.size(),
               static_cast<unsigned long long>(t.seed.get()));

  std::fprintf(f, "%s", R"(
struct entry {
  std::uint16_t key_index;  // Index into tables::string_table.
  std::uint8_t key_length;
  token_type value;
};

// The two tables are combined into one table to reduce the number of
// instructions dedicated to address computation in the lookup function. (I did
// not measure whether this technique actually improves performance, though.)
struct tables_type {
  entry hash_table[hash_table_size];
  char8 string_table[string_table_size];
};

)");

  std::fprintf(f, "constexpr tables_type tables = {\n");
  std::fprintf(f, "    .hash_table =\n");
  std::fprintf(f, "        {\n");
  for (const hash_table::table_entry& e : t.entries) {
    std::fprintf(f, "            {");
    if (e.is_taken(t.current_generation)) {
      std::fprintf(f, "%zu, %zu, token_type::kw_%.*s", strings.data.find(e.key),
                   e.key.size(), narrow_cast<int>(e.key.size()), e.key.data());
    } else {
      std::fprintf(f, "0, 0, token_type::identifier");
    }
    std::fprintf(f, "},\n");
  }
  std::fprintf(f, "        },\n");

  std::fprintf(f, "\n");
  std::fprintf(f, "    .string_table =\n");
  QLJS_ASSERT(!strings.data.empty());
  QLJS_ASSERT(strings.data.back() == '\0');
  for (std::size_t i = 0; i < strings.data.size() - 1; ++i) {
    if (strings.is_word_start(i)) {
      if (i != 0) {
        std::fprintf(f, "\"\n");
      }
      std::fprintf(f, "        u8\"");
    }
    char c = strings.data[i];
    if (c == '\0') {
      std::fprintf(f, "\\0");
    } else {
      std::fputc(c, f);
    }
  }
  std::fprintf(f, "\",\n");

  std::fprintf(f, "};");

  std::fprintf(f, "%s", R"(
}

token_type lexer::identifier_token_type(string8_view identifier) noexcept {
  std::size_t identifier_size = identifier.size();

  keyword_lexer::selection_type selection =
      keyword_lexer::select(identifier.data(), identifier_size);
  keyword_lexer::hash_type hash = keyword_lexer::mix(selection, hash_seed);
  keyword_lexer::hash_type index =
      static_cast<keyword_lexer::hash_type>(hash % hash_table_size);

  const entry& e = tables.hash_table[index];
  const char8* e_key = &tables.string_table[e.key_index];

  // NOTE(strager): Use a result variable to encourage compilers to generate
  // conditional store instructions. Conditional stores can improve performance
  // significantly because lookups are somewhat unpredictable. (Unfortunately,
  // no compiler reliably generates conditional stores.)
  token_type result = e.value;
  if (!keyword_lexer::key_strings_equal(e_key, identifier.data(),
                                        identifier_size)) {
    result = token_type::identifier;
  }
  if (identifier_size != e.key_length) {
    result = token_type::identifier;
  }
  return result;
}
}
)");

  std::fprintf(f, "%s", R"(
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
)");
}

void dump_table_code(const hash_table& t, const string_table& strings,
                     const char* file_path) {
  FILE* f = std::fopen(file_path, "wb");
  if (f == nullptr) {
    std::fprintf(stderr, "error: failed to open %s for writing: %s\n",
                 file_path, std::strerror(errno));
    std::exit(1);
  }
  dump_table_code(t, strings, f);
  if (std::fclose(f) != 0) {
    std::fprintf(stderr, "error: failed to write to %s: %s\n", file_path,
                 std::strerror(errno));
    std::exit(1);
  }
}
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  generate_lex_keyword_options o =
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
  hash_table t = make_hash_table(keys);
  string_table strings = make_string_table(keys);
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
