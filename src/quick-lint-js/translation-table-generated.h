// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRANSLATION_TABLE_GENERATED_H
#define QUICK_LINT_JS_TRANSLATION_TABLE_GENERATED_H

// This file is **GENERATED** by tools/compile-translations.go.

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/consteval.h>
#include <quick-lint-js/hash-fnv.h>
#include <quick-lint-js/translation-table.h>
#include <string_view>

namespace quick_lint_js {
using namespace std::literals::string_view_literals;

constexpr std::uint32_t translation_table_locale_count = 4;
constexpr std::uint16_t translation_table_mapping_table_size = 257;
constexpr std::size_t translation_table_string_table_size = 46498;
constexpr std::size_t translation_table_locale_table_size = 24;

QLJS_CONSTEVAL std::uint16_t translation_table_const_hash_table_look_up(
    std::string_view untranslated) {
  struct const_hash_entry {
    std::uint16_t mapping_table_index;
    const char* untranslated;
  };

  // clang-format off
  constexpr const_hash_entry const_hash_table[] = {
          {0, ""},
          {0, ""},
          {95, "expected parameter for arrow function, but got a literal instead"},
          {72, "commas are not allowed after spread parameter"},
          {0, ""},
          {0, ""},
          {149, "missing body for finally clause"},
          {68, "children end here"},
          {8, "'.' operator needs a key name; use + to concatenate strings; use [] to access with a dynamic key"},
          {220, "unexpected '\\' in identifier"},
          {56, "cannot assign to loop variable in for of/in loop"},
          {152, "missing body for while loop"},
          {41, "a {{0} b }} c"},
          {245, "use of undeclared variable: {0}"},
          {0, ""},
          {231, "unexpected expression; missing key for object entry"},
          {256, "~~~ invalid string, do not use outside benchmark ~~~"},
          {0, ""},
          {237, "unexpected token in variable declaration; expected variable name"},
          {251, "while loop is missing '{1}' around condition"},
          {21, "'with' statement"},
          {240, "unmatched indexing bracket"},
          {141, "missing 'while (condition)' for do-while statement"},
          {186, "new variable shadows existing variable"},
          {243, "unused arrow function"},
          {213, "unclosed identifier escape sequence"},
          {0, ""},
          {1, "\"global-groups\" entries must be strings"},
          {52, "attribute has wrong capitalization; write '{1}' instead"},
          {0, ""},
          {85, "expected ')' to close function call"},
          {0, ""},
          {121, "incomplete export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'"},
          {0, ""},
          {0, ""},
          {0, ""},
          {104, "for loop needs an iterable, or condition and update clauses"},
          {3, "\"globals\" descriptor \"shadowable\" property must be a boolean"},
          {94, "expected hexadecimal digits in Unicode escape sequence"},
          {0, ""},
          {157, "missing comparison; '{1}' does not extend to the right side of '{0}'"},
          {248, "variable declared here"},
          {0, ""},
          {45, "arrow is here"},
          {54, "break can only be used inside of a loop or switch"},
          {60, "cannot declare variable named keyword '{0}'"},
          {198, "redundant delete statement on variable"},
          {92, "expected expression before newline"},
          {25, "'}' is not allowed directly in JSX text; write {{'}'} instead"},
          {134, "mismatched JSX tags; expected '</{1}>'"},
          {131, "legacy octal literals may not contain underscores"},
          {0, ""},
          {71, "code point out of range"},
          {91, "expected expression after 'case'"},
          {2, "\"global-groups\" must be a boolean or an array"},
          {194, "opening '<{1}>' tag here"},
          {178, "missing property name after '.' operator"},
          {166, "missing initializer in const declaration"},
          {61, "cannot export variable named 'let'"},
          {101, "exporting requires 'default'"},
          {19, "'in' disallowed in C-style for loop initializer"},
          {47, "assignment to const global variable"},
          {88, "expected 'from \"name_of_module.mjs\"'"},
          {109, "function call started here"},
          {254, "with statement needs parentheses around expression"},
          {0, ""},
          {7, "'**' operator cannot be used after unary '{1}' without parentheses"},
          {26, "BigInt literal contains decimal point"},
          {163, "missing for loop header"},
          {170, "missing name of exported function"},
          {0, ""},
          {230, "unexpected control character"},
          {238, "unicode byte order mark (BOM) cannot appear before #! at beginning of script"},
          {150, "missing body for function"},
          {0, ""},
          {10, "'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead"},
          {55, "cannot access private identifier outside class"},
          {169, "missing name of exported class"},
          {31, "React/JSX is not yet implemented"},
          {175, "missing parentheses around left-hand side of '**'"},
          {0, ""},
          {0, ""},
          {0, ""},
          {120, "imported variable declared here"},
          {207, "this {0} looks fishy"},
          {0, ""},
          {195, "private properties are not allowed in object literals"},
          {253, "with statement is missing '{1}' around expression"},
          {0, ""},
          {23, "'{0}' is not allowed for strings; use {1} instead"},
          {158, "missing condition for if statement"},
          {129, "label named 'await' not allowed in async function"},
          {181, "missing semicolon between condition and update parts of for loop"},
          {0, ""},
          {0, ""},
          {144, "missing body for 'if' statement"},
          {0, ""},
          {0, ""},
          {73, "commas are not allowed between class methods"},
          {0, ""},
          {0, ""},
          {125, "invalid function parameter"},
          {0, ""},
          {0, ""},
          {79, "do-while loop needs parentheses around condition"},
          {75, "const variable declared here"},
          {208, "this {1} looks fishy"},
          {98, "expected variable name for 'import'-'as'"},
          {199, "remove '{0}' to update an existing variable"},
          {160, "missing condition for while statement"},
          {252, "while loop needs parentheses around condition"},
          {184, "missing variable name"},
          {147, "missing body for class"},
          {122, "indexing requires an expression"},
          {44, "array started here"},
          {48, "assignment to const variable"},
          {0, ""},
          {234, "unexpected statement before first switch case, expected 'case' or 'default'"},
          {17, "'for' loop"},
          {42, "an 'if' statement"},
          {0, ""},
          {0, ""},
          {50, "assignment to imported variable"},
          {255, "{0} with no bindings"},
          {27, "BigInt literal contains exponent"},
          {0, ""},
          {118, "if statement is missing '{1}' around condition"},
          {113, "functions/methods should not have '=>'"},
          {0, ""},
          {18, "'if' statement"},
          {64, "cannot import variable named keyword '{0}'"},
          {232, "unexpected identifier in expression; missing operator before"},
          {0, ""},
          {137, "missing ':' in conditional expression"},
          {136, "missing '...' in JSX attribute spread"},
          {123, "invalid UTF-8 sequence"},
          {138, "missing '<>' and '</>' to enclose multiple children"},
          {0, ""},
          {172, "missing operand for operator"},
          {222, "unexpected 'catch' without 'try'"},
          {63, "cannot import 'let'"},
          {69, "classes cannot be named 'let'"},
          {0, ""},
          {193, "octal number literal has no digits"},
          {9, "'=' changes variables; to compare, use '===' instead"},
          {0, ""},
          {20, "'while' loop"},
          {139, "missing '=' after variable"},
          {59, "cannot declare and export variable with 'export default'"},
          {46, "assigning to 'async' in a for-of loop requires parentheses"},
          {154, "missing catch or finally clause for try statement"},
          {0, ""},
          {0, ""},
          {235, "unexpected token"},
          {0, ""},
          {0, ""},
          {0, ""},
          {236, "unexpected token in export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'"},
          {0, ""},
          {0, ""},
          {106, "for-of loop expression cannot have semicolons"},
          {0, ""},
          {0, ""},
          {34, "a 'do-while' loop"},
          {0, ""},
          {0, ""},
          {105, "for-in loop expression cannot have semicolons"},
          {0, ""},
          {0, ""},
          {39, "a function statement is not allowed as the body of {1:singular}"},
          {35, "a 'for' loop"},
          {65, "cannot reference private variables without object; use 'this.'"},
          {148, "missing body for do-while loop"},
          {0, ""},
          {225, "unexpected '{0}'"},
          {249, "variable used before declaration: {0}"},
          {67, "character is not allowed in identifiers"},
          {0, ""},
          {0, ""},
          {16, "'else' has no corresponding 'if'"},
          {204, "stray comma in let statement"},
          {0, ""},
          {0, ""},
          {0, ""},
          {177, "missing parentheses around self-invoked function"},
          {0, ""},
          {100, "expected {1:singular}"},
          {0, ""},
          {182, "missing semicolon between init and condition parts of for loop"},
          {0, ""},
          {189, "number literal contains trailing underscore(s)"},
          {0, ""},
          {0, ""},
          {0, ""},
          {0, ""},
          {128, "keywords cannot contain escape sequences"},
          {0, ""},
          {247, "variable assigned before its declaration"},
          {111, "function declared here"},
          {0, ""},
          {0, ""},
          {242, "unopened block comment"},
          {90, "expected '{{'"},
          {135, "missing ',' between variable declarations"},
          {22, "'{0}' found here"},
          {76, "continue can only be used inside of a loop"},
          {0, ""},
          {140, "missing 'if' after 'else'"},
          {58, "cannot declare 'yield' inside generator function"},
          {188, "number literal contains consecutive underscores"},
          {0, ""},
          {219, "unexpected '@'"},
          {159, "missing condition for switch statement"},
          {171, "missing name or parentheses for function"},
          {0, ""},
          {180, "missing semicolon after statement"},
          {115, "generator function '*' belongs before function name"},
          {0, ""},
          {0, ""},
          {0, ""},
          {203, "stray comma in function parameter"},
          {0, ""},
          {190, "object literal started here"},
          {226, "unexpected characters in binary literal"},
          {0, ""},
          {0, ""},
          {80, "do-while statement starts here"},
          {103, "extra ',' is not allowed between function call arguments"},
          {30, "JSON syntax error"},
          {179, "missing property name between '.' and '.'"},
          {0, ""},
          {0, ""},
          {200, "return statement returns nothing (undefined)"},
          {156, "missing comma between object literal entries"},
          {14, "'await' is only allowed in async functions"},
          {108, "free {1} and {0} {1} {2}"},
          {146, "missing body for catch clause"},
          {173, "missing operator between expression and arrow function"},
          {49, "assignment to const variable before its declaration"},
          {209, "try statement starts here"},
          {32, "RegExp literal flags cannot contain Unicode escapes"},
          {86, "expected ',' between object literal entries"},
          {187, "newline is not allowed between 'async' and arrow function parameter list"},
          {244, "use 'while' instead to loop until a condition is false"},
          {84, "existing variable declared here"},
          {99, "expected {1:headlinese}"},
          {0, ""},
          {38, "a class statement is not allowed as the body of {1:singular}"},
          {241, "unmatched parenthesis"},
          {183, "missing value for object property"},
          {197, "redeclaration of variable: {0}"},
          {0, ""},
          {0, ""},
          {0, ""},
          {0, ""},
          {97, "expected variable name for 'catch'"},
          {107, "forwarding exports are only allowed in export-from"},
          {246, "variable already declared here"},
          {0, ""},
          {153, "missing body for {1:headlinese}"},
          {0, ""},
          {28, "C-style for loop is missing its third component"},
          {37, "a 'with' statement"},
          {51, "assignment to undeclared variable"},
          {201, "see here"},
          {217, "unclosed template"},
          {0, ""},
          {202, "something happened"},
          {0, ""},
          {0, ""},
          {0, ""},
          {165, "missing header and body for 'for' loop"},
          {224, "unexpected 'finally' without 'try'"},
          {89, "expected 'from' before module specifier"},
          {29, "C-style for loops have only three semicolon-separated components"},
          {102, "exporting requires '{{' and '}'"},
          {196, "redeclaration of global variable"},
          {66, "cannot update variable with '{0}' while declaring it"},
          {0, ""},
          {216, "unclosed string literal"},
          {192, "octal literal may not have exponent"},
          {239, "unmatched '}'"},
          {127, "invalid lone literal in object literal"},
          {110, "function called before declaration in block scope: {0}"},
          {0, ""},
          {0, ""},
          {12, "'async static' is not allowed; write 'static async' instead"},
          {119, "if statement needs parentheses around condition"},
          {0, ""},
          {0, ""},
          {116, "here"},
          {218, "unexpected '#'"},
          {221, "unexpected 'case' outside switch statement"},
          {70, "code point in Unicode escape sequence must not be greater than U+10FFFF"},
          {210, "unclosed block comment"},
          {6, "\"globals\" must be an object"},
          {36, "a 'while' loop"},
          {228, "unexpected characters in number literal"},
          {0, ""},
          {0, ""},
          {0, ""},
          {168, "missing name of class"},
          {0, ""},
          {0, ""},
          {0, ""},
          {87, "expected 'as' between '{1}' and '{2}'"},
          {0, ""},
          {0, ""},
          {132, "let statement cannot declare variables named 'let'"},
          {0, ""},
          {0, ""},
          {143, "missing body for 'for' loop"},
          {112, "function starts here"},
          {43, "another invalid string, do not use outside benchmark"},
          {57, "cannot declare 'await' inside async function"},
          {0, ""},
          {227, "unexpected characters in hex literal"},
          {96, "expected parameter for arrow function, but got an expression instead"},
          {62, "cannot export variable named keyword '{0}'"},
          {0, ""},
          {0, ""},
          {212, "unclosed code block; expected '}' by end of file"},
          {0, ""},
          {53, "binary number literal has no digits"},
          {126, "invalid hex escape sequence: {0}"},
          {145, "missing body for 'switch' statement"},
          {13, "'await' cannot be followed by an arrow function; use 'async' instead"},
          {151, "missing body for try statement"},
          {164, "missing function parameter list"},
          {5, "\"globals\" descriptor must be a boolean or an object"},
          {82, "escaping '-' is not allowed in tag names; write '-' instead"},
          {161, "missing end of array; expected ']'"},
          {142, "missing arrow operator for arrow function"},
          {124, "invalid expression left of assignment"},
          {0, ""},
          {162, "missing expression between parentheses"},
          {176, "missing parentheses around operand of '{0}'"},
          {0, ""},
          {0, ""},
          {24, "'{0}' operator cannot be used before '**' without parentheses"},
          {0, ""},
          {0, ""},
          {0, ""},
          {0, ""},
          {0, ""},
          {250, "what is this '{1}' nonsense?"},
          {40, "a lexical declaration is not allowed as the body of {1:singular}"},
          {0, ""},
          {11, "'?' creates a conditional expression"},
          {133, "methods should not use the 'function' keyword"},
          {185, "misspelled React attribute; write '{1}' instead"},
          {0, ""},
          {4, "\"globals\" descriptor \"writable\" property must be a boolean"},
          {214, "unclosed object literal; expected '}'"},
          {191, "octal literal may not have decimal"},
          {167, "missing name in function statement"},
          {0, ""},
          {155, "missing catch variable name between parentheses"},
          {215, "unclosed regexp literal"},
          {0, ""},
          {0, ""},
          {0, ""},
          {223, "unexpected 'default' outside switch statement"},
          {78, "do-while loop is missing '{1}' around condition"},
          {74, "const fields within classes are only allowed in TypeScript, not JavaScript"},
          {117, "hex number literal has no digits"},
          {206, "switch statement needs parentheses around condition"},
          {93, "expected expression before semicolon"},
          {130, "legacy octal literal may not be BigInt"},
          {0, ""},
          {15, "'do-while' loop"},
          {83, "event attributes must be camelCase: '{1}'"},
          {77, "depth limit exceeded"},
          {0, ""},
          {174, "missing parameters for arrow function"},
          {33, "TypeScript's 'enum' feature is not yet implemented by quick-lint-js"},
          {81, "escaped character is not allowed in identifiers"},
          {0, ""},
          {233, "unexpected literal in parameter list; expected parameter name"},
          {114, "generator function '*' belongs after keyword function"},
          {211, "unclosed class; expected '}' by end of file"},
          {205, "switch statement is missing '{1}' around condition"},
          {0, ""},
          {0, ""},
          {229, "unexpected characters in octal literal"},
  };
  // clang-format on

  std::uint64_t hash = hash_fnv_1a_64(untranslated, 14695981039346658028ULL);
  std::uint64_t table_size = 385;
  for (std::uint64_t attempt = 0; attempt <= 4; ++attempt) {
    const const_hash_entry& hash_entry =
        const_hash_table[(hash + attempt * attempt) % table_size];
    if (hash_entry.mapping_table_index == 0) {
      break;
    }
    if (hash_entry.untranslated == untranslated) {
      return hash_entry.mapping_table_index;
    }
  }

  // If you see an error with the following line, translation-table-generated.h
  // is out of date. Run tools/update-translator-sources to rebuild this file.
  QLJS_CONSTEXPR_ASSERT(false);

  return 0;
}
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
