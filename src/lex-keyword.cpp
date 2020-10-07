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

#include <quick-lint-js/char8.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/warning.h>
#include <stddef.h>
#include <string.h>

QLJS_WARNING_IGNORE_CLANG("-Wold-style-cast")
QLJS_WARNING_IGNORE_CLANG("-Wshorten-64-to-32")
QLJS_WARNING_IGNORE_CLANG("-Wzero-as-null-pointer-constant")
QLJS_WARNING_IGNORE_GCC("-Wconversion")
QLJS_WARNING_IGNORE_GCC("-Wold-style-cast")
QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")
QLJS_WARNING_IGNORE_MSVC(4267)  // Conversion from 'size_t' to 'unsigned int'.

namespace quick_lint_js {
namespace {
// #line 46 "src/lex-keyword.gperf"
struct keyword_entry {
  int string_offset;
  token_type type;
};
/* maximum key range = 58, duplicates = 0 */

class lexer_keyword {
 private:
  static inline unsigned int hash(const char *str, size_t len);

 public:
  static const struct keyword_entry *look_up(const char *str, size_t len);
};

inline unsigned int lexer_keyword::hash(const char *str, size_t len) {
  static const unsigned char asso_values[] = {
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 5,  25, 0,  5,  0,  25, 45, 50, 5,  61, 61,
      40, 20, 35, 0,  61, 61, 0,  15, 0,  0,  15, 5,  10, 30, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
      61, 61, 61, 61};
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
  char stringpool_str14[sizeof("with")];
  char stringpool_str15[sizeof("await")];
  char stringpool_str16[sizeof("export")];
  char stringpool_str17[sizeof("extends")];
  char stringpool_str18[sizeof("set")];
  char stringpool_str19[sizeof("void")];
  char stringpool_str20[sizeof("super")];
  char stringpool_str21[sizeof("static")];
  char stringpool_str22[sizeof("as")];
  char stringpool_str23[sizeof("var")];
  char stringpool_str25[sizeof("async")];
  char stringpool_str26[sizeof("switch")];
  char stringpool_str27[sizeof("of")];
  char stringpool_str28[sizeof("for")];
  char stringpool_str29[sizeof("from")];
  char stringpool_str30[sizeof("break")];
  char stringpool_str31[sizeof("import")];
  char stringpool_str32[sizeof("if")];
  char stringpool_str33[sizeof("function")];
  char stringpool_str35[sizeof("false")];
  char stringpool_str36[sizeof("typeof")];
  char stringpool_str37[sizeof("finally")];
  char stringpool_str38[sizeof("new")];
  char stringpool_str39[sizeof("null")];
  char stringpool_str40[sizeof("yield")];
  char stringpool_str42[sizeof("in")];
  char stringpool_str43[sizeof("let")];
  char stringpool_str44[sizeof("else")];
  char stringpool_str45[sizeof("class")];
  char stringpool_str48[sizeof("get")];
  char stringpool_str50[sizeof("instanceof")];
  char stringpool_str54[sizeof("this")];
  char stringpool_str55[sizeof("throw")];
  char stringpool_str60[sizeof("while")];
};
static const struct stringpool_t stringpool_contents = {
    "try",    "true",   "const",   "return",  "do",       "continue",
    "case",   "catch",  "delete",  "default", "debugger", "with",
    "await",  "export", "extends", "set",     "void",     "super",
    "static", "as",     "var",     "async",   "switch",   "of",
    "for",    "from",   "break",   "import",  "if",       "function",
    "false",  "typeof", "finally", "new",     "null",     "yield",
    "in",     "let",    "else",    "class",   "get",      "instanceof",
    "this",   "throw",  "while"};
#define stringpool ((const char *)&stringpool_contents)
const struct keyword_entry *lexer_keyword::look_up(const char *str,
                                                   size_t len) {
  enum {
    TOTAL_KEYWORDS = 45,
    MIN_WORD_LENGTH = 2,
    MAX_WORD_LENGTH = 10,
    MIN_HASH_VALUE = 3,
    MAX_HASH_VALUE = 60
  };

  static const struct keyword_entry wordlist[] = {
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 90 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str3,
       token_type::kw_try},
      // #line 89 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str4,
       token_type::kw_true},
      // #line 59 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str5,
       token_type::kw_const},
      // #line 82 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str6,
       token_type::kw_return},
      // #line 64 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str7,
       token_type::kw_do},
      // #line 60 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str8,
       token_type::kw_continue},
      // #line 56 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str9,
       token_type::kw_case},
      // #line 57 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str10,
       token_type::kw_catch},
      // #line 63 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str11,
       token_type::kw_delete},
      // #line 62 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str12,
       token_type::kw_default},
      // #line 61 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str13,
       token_type::kw_debugger},
      // #line 95 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str14,
       token_type::kw_with},
      // #line 54 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str15,
       token_type::kw_await},
      // #line 66 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str16,
       token_type::kw_export},
      // #line 67 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str17,
       token_type::kw_extends},
      // #line 83 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str18,
       token_type::kw_set},
      // #line 93 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str19,
       token_type::kw_void},
      // #line 85 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str20,
       token_type::kw_super},
      // #line 84 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str21,
       token_type::kw_static},
      // #line 52 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str22,
       token_type::kw_as},
      // #line 92 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str23,
       token_type::kw_var},
      {-1, static_cast<token_type>(0)},
      // #line 53 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str25,
       token_type::kw_async},
      // #line 86 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str26,
       token_type::kw_switch},
      // #line 81 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str27,
       token_type::kw_of},
      // #line 70 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str28,
       token_type::kw_for},
      // #line 71 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str29,
       token_type::kw_from},
      // #line 55 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str30,
       token_type::kw_break},
      // #line 75 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str31,
       token_type::kw_import},
      // #line 74 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str32,
       token_type::kw_if},
      // #line 72 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str33,
       token_type::kw_function},
      {-1, static_cast<token_type>(0)},
      // #line 68 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str35,
       token_type::kw_false},
      // #line 91 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str36,
       token_type::kw_typeof},
      // #line 69 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str37,
       token_type::kw_finally},
      // #line 79 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str38,
       token_type::kw_new},
      // #line 80 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str39,
       token_type::kw_null},
      // #line 96 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str40,
       token_type::kw_yield},
      {-1, static_cast<token_type>(0)},
      // #line 76 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str42,
       token_type::kw_in},
      // #line 78 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str43,
       token_type::kw_let},
      // #line 65 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str44,
       token_type::kw_else},
      // #line 58 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str45,
       token_type::kw_class},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 73 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str48,
       token_type::kw_get},
      {-1, static_cast<token_type>(0)},
      // #line 77 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str50,
       token_type::kw_instanceof},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 87 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str54,
       token_type::kw_this},
      // #line 88 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str55,
       token_type::kw_throw},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 94 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str60,
       token_type::kw_while}};

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
// #line 97 "src/lex-keyword.gperf"

}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
token_type lexer::identifier_token_type(string8_view identifier) noexcept {
  const keyword_entry *entry = lexer_keyword::look_up(
      reinterpret_cast<const char *>(identifier.data()), identifier.size());
  if (entry) {
    return entry->type;
  } else {
    return token_type::identifier;
  }
}
QLJS_WARNING_POP
}
