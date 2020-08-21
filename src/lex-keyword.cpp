/* C++ code produced by gperf version 3.1 */
/* Command-line: gperf src/lex-keyword.gperf  */
/* Computed positions: -k'1-2' */

#if !(                                                                         \
    (' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) && ('%' == 37) && \
    ('&' == 38) && ('\'' == 39) && ('(' == 40) && (')' == 41) &&               \
    ('*' == 42) && ('+' == 43) && (',' == 44) && ('-' == 45) && ('.' == 46) && \
    ('/' == 47) && ('0' == 48) && ('1' == 49) && ('2' == 50) && ('3' == 51) && \
    ('4' == 52) && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) && \
    ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) && ('=' == 61) && \
    ('>' == 62) && ('?' == 63) && ('A' == 65) && ('B' == 66) && ('C' == 67) && \
    ('D' == 68) && ('E' == 69) && ('F' == 70) && ('G' == 71) && ('H' == 72) && \
    ('I' == 73) && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) && \
    ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) && ('R' == 82) && \
    ('S' == 83) && ('T' == 84) && ('U' == 85) && ('V' == 86) && ('W' == 87) && \
    ('X' == 88) && ('Y' == 89) && ('Z' == 90) && ('[' == 91) &&                \
    ('\\' == 92) && (']' == 93) && ('^' == 94) && ('_' == 95) &&               \
    ('a' == 97) && ('b' == 98) && ('c' == 99) && ('d' == 100) &&               \
    ('e' == 101) && ('f' == 102) && ('g' == 103) && ('h' == 104) &&            \
    ('i' == 105) && ('j' == 106) && ('k' == 107) && ('l' == 108) &&            \
    ('m' == 109) && ('n' == 110) && ('o' == 111) && ('p' == 112) &&            \
    ('q' == 113) && ('r' == 114) && ('s' == 115) && ('t' == 116) &&            \
    ('u' == 117) && ('v' == 118) && ('w' == 119) && ('x' == 120) &&            \
    ('y' == 121) && ('z' == 122) && ('{' == 123) && ('|' == 124) &&            \
    ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error \
    "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

// #line 12 "src/lex-keyword.gperf"

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <quick-lint-js/lex.h>
#include <quick-lint-js/warning.h>
#include <stddef.h>
#include <string.h>
#include <string_view>

QLJS_WARNING_IGNORE_CLANG("-Wold-style-cast")
QLJS_WARNING_IGNORE_CLANG("-Wshorten-64-to-32")
QLJS_WARNING_IGNORE_CLANG("-Wzero-as-null-pointer-constant")
QLJS_WARNING_IGNORE_GCC("-Wconversion")
QLJS_WARNING_IGNORE_GCC("-Wold-style-cast")
QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")

namespace quick_lint_js {
namespace {
// #line 45 "src/lex-keyword.gperf"
struct keyword_entry {
  int string_offset;
  int index;
};

// Keep this list in sync with the token_type enum.;
/* maximum key range = 63, duplicates = 0 */

class lexer_keyword {
 private:
  static inline unsigned int hash(const char *str, size_t len);

 public:
  static const struct keyword_entry *look_up(const char *str, size_t len);
};

inline unsigned int lexer_keyword::hash(const char *str, size_t len) {
  static const unsigned char asso_values[] = {
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 5,  15, 0,  5,  0,  15, 20, 50, 15, 66, 66,
      40, 15, 25, 0,  66, 66, 0,  20, 0,  10, 30, 5,  15, 45, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
      66, 66, 66, 66};
  return len + asso_values[static_cast<unsigned char>(str[1])] +
         asso_values[static_cast<unsigned char>(str[0])];
}

struct stringpool_t {
  char stringpool_str3[sizeof("try")];
  char stringpool_str4[sizeof("true")];
  char stringpool_str5[sizeof("const")];
  char stringpool_str6[sizeof("return")];
  char stringpool_str7[sizeof("do")];
  char stringpool_str8[sizeof("continue")];
  char stringpool_str9[sizeof("case")];
  char stringpool_str10[sizeof("catch")];
  char stringpool_str11[sizeof("delete")];
  char stringpool_str12[sizeof("default")];
  char stringpool_str13[sizeof("debugger")];
  char stringpool_str15[sizeof("await")];
  char stringpool_str17[sizeof("of")];
  char stringpool_str18[sizeof("for")];
  char stringpool_str19[sizeof("from")];
  char stringpool_str20[sizeof("break")];
  char stringpool_str21[sizeof("export")];
  char stringpool_str22[sizeof("extends")];
  char stringpool_str23[sizeof("get")];
  char stringpool_str24[sizeof("with")];
  char stringpool_str25[sizeof("false")];
  char stringpool_str26[sizeof("static")];
  char stringpool_str27[sizeof("as")];
  char stringpool_str28[sizeof("new")];
  char stringpool_str30[sizeof("async")];
  char stringpool_str31[sizeof("switch")];
  char stringpool_str32[sizeof("if")];
  char stringpool_str33[sizeof("function")];
  char stringpool_str34[sizeof("void")];
  char stringpool_str35[sizeof("super")];
  char stringpool_str36[sizeof("import")];
  char stringpool_str37[sizeof("finally")];
  char stringpool_str38[sizeof("var")];
  char stringpool_str39[sizeof("null")];
  char stringpool_str42[sizeof("in")];
  char stringpool_str43[sizeof("let")];
  char stringpool_str44[sizeof("else")];
  char stringpool_str45[sizeof("class")];
  char stringpool_str50[sizeof("instanceof")];
  char stringpool_str51[sizeof("typeof")];
  char stringpool_str54[sizeof("this")];
  char stringpool_str55[sizeof("throw")];
  char stringpool_str60[sizeof("while")];
  char stringpool_str65[sizeof("yield")];
};
static const struct stringpool_t stringpool_contents = {
    "try",    "true",    "const",      "return",   "do",       "continue",
    "case",   "catch",   "delete",     "default",  "debugger", "await",
    "of",     "for",     "from",       "break",    "export",   "extends",
    "get",    "with",    "false",      "static",   "as",       "new",
    "async",  "switch",  "if",         "function", "void",     "super",
    "import", "finally", "var",        "null",     "in",       "let",
    "else",   "class",   "instanceof", "typeof",   "this",     "throw",
    "while",  "yield"};
#define stringpool ((const char *)&stringpool_contents)
const struct keyword_entry *lexer_keyword::look_up(const char *str,
                                                   size_t len) {
  enum {
    TOTAL_KEYWORDS = 44,
    MIN_WORD_LENGTH = 2,
    MAX_WORD_LENGTH = 10,
    MIN_HASH_VALUE = 3,
    MAX_HASH_VALUE = 65
  };

  static const struct keyword_entry wordlist[] = {
      {-1, 0},
      {-1, 0},
      {-1, 0},
      // #line 89 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str3, 37},
      // #line 88 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str4, 36},
      // #line 59 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str5, 7},
      // #line 82 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str6, 30},
      // #line 64 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str7, 12},
      // #line 60 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str8, 8},
      // #line 56 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str9, 4},
      // #line 57 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str10, 5},
      // #line 63 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str11, 11},
      // #line 62 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str12, 10},
      // #line 61 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str13, 9},
      {-1, 0},
      // #line 54 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str15, 2},
      {-1, 0},
      // #line 81 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str17, 29},
      // #line 70 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str18, 18},
      // #line 71 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str19, 19},
      // #line 55 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str20, 3},
      // #line 66 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str21, 14},
      // #line 67 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str22, 15},
      // #line 73 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str23, 21},
      // #line 94 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str24, 42},
      // #line 68 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str25, 16},
      // #line 83 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str26, 31},
      // #line 52 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str27, 0},
      // #line 79 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str28, 27},
      {-1, 0},
      // #line 53 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str30, 1},
      // #line 85 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str31, 33},
      // #line 74 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str32, 22},
      // #line 72 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str33, 20},
      // #line 92 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str34, 40},
      // #line 84 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str35, 32},
      // #line 75 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str36, 23},
      // #line 69 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str37, 17},
      // #line 91 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str38, 39},
      // #line 80 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str39, 28},
      {-1, 0},
      {-1, 0},
      // #line 76 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str42, 24},
      // #line 78 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str43, 26},
      // #line 65 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str44, 13},
      // #line 58 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str45, 6},
      {-1, 0},
      {-1, 0},
      {-1, 0},
      {-1, 0},
      // #line 77 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str50, 25},
      // #line 90 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str51, 38},
      {-1, 0},
      {-1, 0},
      // #line 86 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str54, 34},
      // #line 87 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str55, 35},
      {-1, 0},
      {-1, 0},
      {-1, 0},
      {-1, 0},
      // #line 93 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str60, 41},
      {-1, 0},
      {-1, 0},
      {-1, 0},
      {-1, 0},
      // #line 95 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str65, 43}};

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH) {
    unsigned int key = hash(str, len);

    if (key <= MAX_HASH_VALUE) {
      int o = wordlist[key].string_offset;
      if (o >= 0) {
        const char *s = o + stringpool;

        if (*str == *s && !strncmp(str + 1, s + 1, len - 1) && s[len] == '\0')
          return &wordlist[key];
      }
    }
  }
  return 0;
}
// #line 96 "src/lex-keyword.gperf"

}  // namespace

token_type lexer::identifier_token_type(std::string_view identifier) noexcept {
  const keyword_entry *entry =
      lexer_keyword::look_up(identifier.data(), identifier.size());
  if (entry) {
    return lexer::keyword_from_index(entry->index);
  } else {
    return token_type::identifier;
  }
}
}  // namespace quick_lint_js
