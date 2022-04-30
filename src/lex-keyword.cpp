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

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/token.h>
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
// #line 34 "src/lex-keyword.gperf"
struct keyword_entry {
  int string_offset;
  token_type type;
};
/* maximum key range = 94, duplicates = 0 */

class lexer_keyword {
 private:
  static inline unsigned int hash(const char *str, size_t len);

 public:
  static const struct keyword_entry *look_up(const char *str, size_t len);
};

inline unsigned int lexer_keyword::hash(const char *str, size_t len) {
  static const unsigned char asso_values[] = {
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 5,  0,  10, 45, 0,  0,  35, 50, 5,  96, 96,
      40, 50, 10, 0,  30, 96, 0,  20, 5,  20, 25, 40, 15, 25, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      96, 96, 96, 96};
  return len + asso_values[static_cast<unsigned char>(str[1])] +
         asso_values[static_cast<unsigned char>(str[0])];
}

struct stringpool_t {
  char stringpool_str2[sizeof("of")];
  char stringpool_str3[sizeof("for")];
  char stringpool_str4[sizeof("from")];
  char stringpool_str5[sizeof("break")];
  char stringpool_str6[sizeof("return")];
  char stringpool_str7[sizeof("if")];
  char stringpool_str8[sizeof("try")];
  char stringpool_str9[sizeof("true")];
  char stringpool_str10[sizeof("false")];
  char stringpool_str12[sizeof("finally")];
  char stringpool_str13[sizeof("new")];
  char stringpool_str14[sizeof("enum")];
  char stringpool_str15[sizeof("const")];
  char stringpool_str17[sizeof("in")];
  char stringpool_str18[sizeof("continue")];
  char stringpool_str19[sizeof("case")];
  char stringpool_str20[sizeof("catch")];
  char stringpool_str21[sizeof("export")];
  char stringpool_str22[sizeof("extends")];
  char stringpool_str23[sizeof("set")];
  char stringpool_str24[sizeof("interface")];
  char stringpool_str25[sizeof("instanceof")];
  char stringpool_str27[sizeof("as")];
  char stringpool_str28[sizeof("function")];
  char stringpool_str29[sizeof("void")];
  char stringpool_str30[sizeof("async")];
  char stringpool_str31[sizeof("static")];
  char stringpool_str33[sizeof("var")];
  char stringpool_str34[sizeof("null")];
  char stringpool_str35[sizeof("yield")];
  char stringpool_str36[sizeof("typeof")];
  char stringpool_str37[sizeof("private")];
  char stringpool_str38[sizeof("get")];
  char stringpool_str39[sizeof("protected")];
  char stringpool_str42[sizeof("package")];
  char stringpool_str43[sizeof("let")];
  char stringpool_str44[sizeof("else")];
  char stringpool_str45[sizeof("super")];
  char stringpool_str47[sizeof("do")];
  char stringpool_str49[sizeof("with")];
  char stringpool_str50[sizeof("await")];
  char stringpool_str51[sizeof("delete")];
  char stringpool_str52[sizeof("default")];
  char stringpool_str53[sizeof("debugger")];
  char stringpool_str55[sizeof("class")];
  char stringpool_str56[sizeof("public")];
  char stringpool_str59[sizeof("this")];
  char stringpool_str60[sizeof("throw")];
  char stringpool_str61[sizeof("import")];
  char stringpool_str65[sizeof("implements")];
  char stringpool_str66[sizeof("switch")];
  char stringpool_str95[sizeof("while")];
};
static const struct stringpool_t stringpool_contents = {
    "of",      "for",        "from",      "break",      "return",  "if",
    "try",     "true",       "false",     "finally",    "new",     "enum",
    "const",   "in",         "continue",  "case",       "catch",   "export",
    "extends", "set",        "interface", "instanceof", "as",      "function",
    "void",    "async",      "static",    "var",        "null",    "yield",
    "typeof",  "private",    "get",       "protected",  "package", "let",
    "else",    "super",      "do",        "with",       "await",   "delete",
    "default", "debugger",   "class",     "public",     "this",    "throw",
    "import",  "implements", "switch",    "while"};
#define stringpool ((const char *)&stringpool_contents)
const struct keyword_entry *lexer_keyword::look_up(const char *str,
                                                   size_t len) {
  enum {
    TOTAL_KEYWORDS = 52,
    MIN_WORD_LENGTH = 2,
    MAX_WORD_LENGTH = 10,
    MIN_HASH_VALUE = 2,
    MAX_HASH_VALUE = 95
  };

  static const struct keyword_entry wordlist[] = {
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 72 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str2,
       token_type::kw_of},
      // #line 59 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str3,
       token_type::kw_for},
      // #line 60 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str4,
       token_type::kw_from},
      // #line 43 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str5,
       token_type::kw_break},
      // #line 77 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str6,
       token_type::kw_return},
      // #line 63 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str7,
       token_type::kw_if},
      // #line 85 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str8,
       token_type::kw_try},
      // #line 84 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str9,
       token_type::kw_true},
      // #line 57 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str10,
       token_type::kw_false},
      {-1, static_cast<token_type>(0)},
      // #line 58 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str12,
       token_type::kw_finally},
      // #line 70 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str13,
       token_type::kw_new},
      // #line 54 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str14,
       token_type::kw_enum},
      // #line 47 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str15,
       token_type::kw_const},
      {-1, static_cast<token_type>(0)},
      // #line 66 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str17,
       token_type::kw_in},
      // #line 48 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str18,
       token_type::kw_continue},
      // #line 44 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str19,
       token_type::kw_case},
      // #line 45 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str20,
       token_type::kw_catch},
      // #line 55 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str21,
       token_type::kw_export},
      // #line 56 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str22,
       token_type::kw_extends},
      // #line 78 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str23,
       token_type::kw_set},
      // #line 68 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str24,
       token_type::kw_interface},
      // #line 67 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str25,
       token_type::kw_instanceof},
      {-1, static_cast<token_type>(0)},
      // #line 40 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str27,
       token_type::kw_as},
      // #line 61 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str28,
       token_type::kw_function},
      // #line 88 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str29,
       token_type::kw_void},
      // #line 41 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str30,
       token_type::kw_async},
      // #line 79 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str31,
       token_type::kw_static},
      {-1, static_cast<token_type>(0)},
      // #line 87 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str33,
       token_type::kw_var},
      // #line 71 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str34,
       token_type::kw_null},
      // #line 91 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str35,
       token_type::kw_yield},
      // #line 86 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str36,
       token_type::kw_typeof},
      // #line 74 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str37,
       token_type::kw_private},
      // #line 62 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str38,
       token_type::kw_get},
      // #line 75 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str39,
       token_type::kw_protected},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 73 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str42,
       token_type::kw_package},
      // #line 69 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str43,
       token_type::kw_let},
      // #line 53 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str44,
       token_type::kw_else},
      // #line 80 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str45,
       token_type::kw_super},
      {-1, static_cast<token_type>(0)},
      // #line 52 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str47,
       token_type::kw_do},
      {-1, static_cast<token_type>(0)},
      // #line 90 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str49,
       token_type::kw_with},
      // #line 42 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str50,
       token_type::kw_await},
      // #line 51 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str51,
       token_type::kw_delete},
      // #line 50 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str52,
       token_type::kw_default},
      // #line 49 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str53,
       token_type::kw_debugger},
      {-1, static_cast<token_type>(0)},
      // #line 46 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str55,
       token_type::kw_class},
      // #line 76 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str56,
       token_type::kw_public},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 82 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str59,
       token_type::kw_this},
      // #line 83 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str60,
       token_type::kw_throw},
      // #line 65 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str61,
       token_type::kw_import},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 64 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str65,
       token_type::kw_implements},
      // #line 81 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str66,
       token_type::kw_switch},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      {-1, static_cast<token_type>(0)},
      // #line 89 "src/lex-keyword.gperf"
      {(int)(size_t) & ((struct stringpool_t *)0)->stringpool_str95,
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
// #line 92 "src/lex-keyword.gperf"

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
