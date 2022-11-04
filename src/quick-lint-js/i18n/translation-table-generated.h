// Code generated by tools/compile-translations.go. DO NOT EDIT.
// source: po/*.po

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

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

constexpr std::uint32_t translation_table_locale_count = 5;
constexpr std::uint16_t translation_table_mapping_table_size = 395;
constexpr std::size_t translation_table_string_table_size = 74251;
constexpr std::size_t translation_table_locale_table_size = 35;

QLJS_CONSTEVAL std::uint16_t translation_table_const_look_up(
    std::string_view untranslated) {
  // clang-format off
  constexpr std::string_view const_lookup_table[] = {
          "\"global-groups\" entries must be strings"sv,
          "\"global-groups\" must be a boolean or an array"sv,
          "\"globals\" descriptor \"shadowable\" property must be a boolean"sv,
          "\"globals\" descriptor \"writable\" property must be a boolean"sv,
          "\"globals\" descriptor must be a boolean or an object"sv,
          "\"globals\" must be an object"sv,
          "'**' operator cannot be used after unary '{1}' without parentheses"sv,
          "'.' is not allowed after generic arguments; write [\"{1}\"] instead"sv,
          "'.' operator needs a key name; use + to concatenate strings; use [] to access with a dynamic key"sv,
          "'=' changes variables; to compare, use '===' instead"sv,
          "'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead"sv,
          "'?' creates a conditional expression"sv,
          "'as const' is only allowed on literals (array, object, string, boolean) and enum members"sv,
          "'as const' located here"sv,
          "'async export' is not allowed; write 'export async' instead"sv,
          "'async static' is not allowed; write 'static async' instead"sv,
          "'await' cannot be followed by an arrow function; use 'async' instead"sv,
          "'await' is only allowed in async functions"sv,
          "'do-while' loop"sv,
          "'else' has no corresponding 'if'"sv,
          "'extends' must be before 'implements'"sv,
          "'for' loop"sv,
          "'function async' is not allowed; write 'async function' instead"sv,
          "'function' is here"sv,
          "'if' statement"sv,
          "'in' disallowed in C-style for loop initializer"sv,
          "'private' is not allowed in JavaScript"sv,
          "'protected' is not allowed in JavaScript"sv,
          "'public' is not allowed in JavaScript"sv,
          "'readonly static' is not allowed; write 'static readonly' instead"sv,
          "'readonly' only works with array types and tuple types"sv,
          "'this' must be the first parameter"sv,
          "'this' parameter not allowed when destructuring"sv,
          "'this' parameters are not allowed in JavaScript"sv,
          "'this' parameters are not allowed in arrow functions"sv,
          "'type' cannot be used twice in export"sv,
          "'type' cannot be used twice in import"sv,
          "'while' loop"sv,
          "'with' statement"sv,
          "'{0} []' is always '{1}'"sv,
          "'{0}' found here"sv,
          "'{0}' is not allowed for strings; use {1} instead"sv,
          "'{0}' is not allowed on methods"sv,
          "'{0}' operator cannot be used before '**' without parentheses"sv,
          "'}' is not allowed directly in JSX text; write {{'}'} instead"sv,
          "BigInt literal contains decimal point"sv,
          "BigInt literal contains exponent"sv,
          "C-style for loop is missing its third component"sv,
          "C-style for loops have only three semicolon-separated components"sv,
          "JSON syntax error"sv,
          "React/JSX is not allowed in TypeScript code"sv,
          "React/JSX is not allowed in vanilla JavaScript code"sv,
          "RegExp literal flags cannot contain Unicode escapes"sv,
          "TypeScript 'as' type assertions are not allowed in JavaScript"sv,
          "TypeScript 'implements' is not allowed in JavaScript"sv,
          "TypeScript <Type> type assertions are not allowed in JSX mode"sv,
          "TypeScript assignment-asserted fields are not supported in JavaScript"sv,
          "TypeScript generics are not allowed in JavaScript code"sv,
          "TypeScript import aliases are not allowed in JavaScript"sv,
          "TypeScript interface fields cannot be initalized"sv,
          "TypeScript interface methods cannot be marked 'async'"sv,
          "TypeScript interface methods cannot be marked as a generator"sv,
          "TypeScript interface methods cannot contain a body"sv,
          "TypeScript interface properties are always public and cannot be marked protected"sv,
          "TypeScript interface properties cannot be 'static'"sv,
          "TypeScript namespaces are not allowed in JavaScript"sv,
          "TypeScript non-null assertion is not allowed on parameters"sv,
          "TypeScript non-null assertions are not allowed in JavaScript"sv,
          "TypeScript optional parameter requires parentheses"sv,
          "TypeScript optional parameter with type annotation requires parentheses"sv,
          "TypeScript optional parameters are not allowed in JavaScript"sv,
          "TypeScript optional properties are not allowed in JavaScript code"sv,
          "TypeScript type annotation requires parentheses"sv,
          "TypeScript type annotations are not allowed in JavaScript code"sv,
          "TypeScript type exports are not allowed in JavaScript"sv,
          "TypeScript type imports are not allowed in JavaScript"sv,
          "TypeScript type imports cannot import both default and named exports"sv,
          "TypeScript types are not allowed in JavaScript"sv,
          "TypeScript's 'enum' feature is not allowed in JavaScript"sv,
          "TypeScript's 'interface' feature is not allowed in JavaScript code"sv,
          "TypeScript's 'readonly' feature is not allowed in JavaScript code"sv,
          "a 'do-while' loop"sv,
          "a 'for' loop"sv,
          "a 'while' loop"sv,
          "a 'with' statement"sv,
          "a class statement is not allowed as the body of {1:singular}"sv,
          "a function statement is not allowed as the body of {1:singular}"sv,
          "a labelled statement"sv,
          "a lexical declaration is not allowed as the body of {1:singular}"sv,
          "a {{0} b }} c"sv,
          "abstract fields cannot have default values"sv,
          "abstract methods cannot be marked 'async'"sv,
          "abstract methods cannot be marked as a generator"sv,
          "abstract methods cannot contain a body"sv,
          "abstract properties are not allowed in interfaces"sv,
          "abstract properties are only allowed in abstract classes"sv,
          "an 'if' statement"sv,
          "another invalid string, do not use outside benchmark"sv,
          "array started here"sv,
          "arrow is here"sv,
          "assigning to 'async' in a for-of loop requires parentheses"sv,
          "assignment to const global variable"sv,
          "assignment to const variable"sv,
          "assignment to const variable before its declaration"sv,
          "assignment to imported variable"sv,
          "assignment to undeclared variable"sv,
          "assignment-asserted field must have a type annotation"sv,
          "assignment-asserted fields are not supported in interfaces"sv,
          "assignment-assertion fields cannot have default values"sv,
          "attribute has wrong capitalization; write '{1}' instead"sv,
          "binary number literal has no digits"sv,
          "break can only be used inside of a loop or switch"sv,
          "cannot access private identifier outside class"sv,
          "cannot assign to loop variable in for of/in loop"sv,
          "cannot declare 'await' inside async function"sv,
          "cannot declare 'yield' inside generator function"sv,
          "cannot declare and export variable with 'export default'"sv,
          "cannot declare variable named keyword '{0}'"sv,
          "cannot delete variables in TypeScript"sv,
          "cannot export variable named 'let'"sv,
          "cannot export variable named keyword '{0}'"sv,
          "cannot import 'let'"sv,
          "cannot import variable named keyword '{0}'"sv,
          "cannot reference private variables without object; use 'this.'"sv,
          "cannot update variable with '{0}' while declaring it"sv,
          "cannot use '...' on 'this' parameter"sv,
          "catch variable can only be typed as '*', 'any', or 'unknown'"sv,
          "character is not allowed in identifiers"sv,
          "children end here"sv,
          "class is not marked abstract"sv,
          "classes cannot be named 'let'"sv,
          "code point in Unicode escape sequence must not be greater than U+10FFFF"sv,
          "code point out of range"sv,
          "commas are not allowed after spread parameter"sv,
          "commas are not allowed between class methods"sv,
          "computed enum member name must be a simple string"sv,
          "computed value disables enum autoincrement"sv,
          "const fields within classes are only allowed in TypeScript, not JavaScript"sv,
          "const variable declared here"sv,
          "continue can only be used inside of a loop"sv,
          "depth limit exceeded"sv,
          "do-while loop is missing '{1}' around condition"sv,
          "do-while loop needs parentheses around condition"sv,
          "do-while statement starts here"sv,
          "duplicated case clause in switch statement"sv,
          "enum member name cannot be numeric"sv,
          "enum member needs initializer"sv,
          "escaped character is not allowed in identifiers"sv,
          "escaping '-' is not allowed in tag names; write '-' instead"sv,
          "event attributes must be camelCase: '{1}'"sv,
          "existing variable declared here"sv,
          "expected ')' to close function call"sv,
          "expected ',' between object literal entries"sv,
          "expected '?' to mark tuple element as optional"sv,
          "expected 'as' between '{1}' and '{2}'"sv,
          "expected 'from \"name_of_module.mjs\"'"sv,
          "expected 'from' before module specifier"sv,
          "expected '{{'"sv,
          "expected at least one parameter in generic parameter list"sv,
          "expected expression after 'case'"sv,
          "expected expression before newline"sv,
          "expected expression before semicolon"sv,
          "expected hexadecimal digits in Unicode escape sequence"sv,
          "expected parameter for arrow function, but got a literal instead"sv,
          "expected parameter for arrow function, but got an expression instead"sv,
          "expected variable name for 'catch'"sv,
          "expected variable name for 'import'-'as'"sv,
          "expected {1:headlinese}"sv,
          "expected {1:singular}"sv,
          "exporting requires 'default'"sv,
          "exporting requires '{{' and '}'"sv,
          "extra ',' is not allowed between enum members"sv,
          "extra ',' is not allowed between function call arguments"sv,
          "field declared here"sv,
          "field marked abstract here"sv,
          "first parameter starts here"sv,
          "for loop needs an iterable, or condition and update clauses"sv,
          "for-in loop expression cannot have semicolons"sv,
          "for-of loop expression cannot have semicolons"sv,
          "forwarding exports are only allowed in export-from"sv,
          "free {1} and {0} {1} {2}"sv,
          "function call started here"sv,
          "function called before declaration in block scope: {0}"sv,
          "function declared here"sv,
          "function overload signature cannot have generator '*'"sv,
          "function overload signature must be named '{1}'"sv,
          "function starts here"sv,
          "functions/methods should not have '=>'"sv,
          "generator function '*' belongs after keyword function"sv,
          "generator function '*' belongs before function name"sv,
          "generic arrow function needs ',' here in TSX"sv,
          "here"sv,
          "here is the assignment assertion operator"sv,
          "hex number literal has no digits"sv,
          "if statement is missing '{1}' around condition"sv,
          "if statement needs parentheses around condition"sv,
          "imported variable declared here"sv,
          "incomplete export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'"sv,
          "index signature must be a field, not a method"sv,
          "index signatures require a value type"sv,
          "indexing requires an expression"sv,
          "initializer starts here"sv,
          "integer cannot be represented and will be rounded to '{1}'"sv,
          "interface properties are always public and cannot be private"sv,
          "interface properties cannot be marked public explicitly"sv,
          "interfaces cannot contain static blocks"sv,
          "invalid UTF-8 sequence"sv,
          "invalid expression left of assignment"sv,
          "invalid function parameter"sv,
          "invalid hex escape sequence: {0}"sv,
          "invalid lone literal in object literal"sv,
          "invalid usage of ? as a prefix or suffix in the a type expression, use '| void' instead"sv,
          "keywords cannot contain escape sequences"sv,
          "label named 'await' not allowed in async function"sv,
          "labelled statement"sv,
          "leading commas are not allowed in generic parameter lists"sv,
          "legacy octal literal may not be BigInt"sv,
          "legacy octal literals may not contain underscores"sv,
          "let statement cannot declare variables named 'let'"sv,
          "lower case letters compared with toUpperCase"sv,
          "methods cannot be readonly"sv,
          "methods should not use the 'function' keyword"sv,
          "mismatched JSX tags; expected '</{1}>'"sv,
          "missing ',' between variable declarations"sv,
          "missing ',', ';', or newline between object type entries"sv,
          "missing '...' in JSX attribute spread"sv,
          "missing ':' in conditional expression"sv,
          "missing '<>' and '</>' to enclose multiple children"sv,
          "missing '=' after variable"sv,
          "missing 'if' after 'else'"sv,
          "missing 'while (condition)' for do-while statement"sv,
          "missing TypeScript type"sv,
          "missing arrow operator for arrow function"sv,
          "missing body for 'for' loop"sv,
          "missing body for 'if' statement"sv,
          "missing body for 'switch' statement"sv,
          "missing body for TypeScript interface"sv,
          "missing body for catch clause"sv,
          "missing body for class"sv,
          "missing body for do-while loop"sv,
          "missing body for finally clause"sv,
          "missing body for function"sv,
          "missing body for try statement"sv,
          "missing body for while loop"sv,
          "missing body for {1:headlinese}"sv,
          "missing catch or finally clause for try statement"sv,
          "missing catch variable name between parentheses"sv,
          "missing comma between generic parameters"sv,
          "missing comma between object literal entries"sv,
          "missing comparison; '{1}' does not extend to the right side of '{0}'"sv,
          "missing condition for if statement"sv,
          "missing condition for switch statement"sv,
          "missing condition for while statement"sv,
          "missing end of array; expected ']'"sv,
          "missing expression between parentheses"sv,
          "missing for loop header"sv,
          "missing function parameter list"sv,
          "missing header and body for 'for' loop"sv,
          "missing initializer in const declaration"sv,
          "missing name for class method"sv,
          "missing name for element in named tuple type"sv,
          "missing name in function statement"sv,
          "missing name of class"sv,
          "missing name of exported class"sv,
          "missing name of exported function"sv,
          "missing name or parentheses for function"sv,
          "missing operand for operator"sv,
          "missing operator between expression and arrow function"sv,
          "missing parameters for arrow function"sv,
          "missing parentheses around left-hand side of '**'"sv,
          "missing parentheses around operand of '{0}'"sv,
          "missing parentheses around parameter"sv,
          "missing parentheses around self-invoked function"sv,
          "missing property name after '.' operator"sv,
          "missing property name between '.' and '.'"sv,
          "missing quotes around module name '{0}'"sv,
          "missing semicolon after abstract method"sv,
          "missing semicolon after field"sv,
          "missing semicolon after index signature"sv,
          "missing semicolon after interface method"sv,
          "missing semicolon after statement"sv,
          "missing semicolon between condition and update parts of for loop"sv,
          "missing semicolon between init and condition parts of for loop"sv,
          "missing type between '{1}' and '{0}'"sv,
          "missing value for object property"sv,
          "missing variable name"sv,
          "misspelled React attribute; write '{1}' instead"sv,
          "move the 'extends' clause before 'implements' here"sv,
          "new variable shadows existing variable"sv,
          "newline is not allowed after 'abstract'"sv,
          "newline is not allowed after 'interface'"sv,
          "newline is not allowed after 'namespace'"sv,
          "newline is not allowed after 'type'"sv,
          "newline is not allowed between 'async' and 'function'"sv,
          "newline is not allowed between 'async' and arrow function parameter list"sv,
          "newline is not allowed between field name and '!'"sv,
          "number literal contains consecutive underscores"sv,
          "number literal contains trailing underscore(s)"sv,
          "object literal started here"sv,
          "octal literal may not have decimal"sv,
          "octal literal may not have exponent"sv,
          "octal number literal has no digits"sv,
          "only one comma is allowed between or after generic parameters"sv,
          "only optional tuple elements can follow this optional tuple element"sv,
          "opening '<{1}>' tag here"sv,
          "optional parameter cannot have both '?' and initializer; remove '?'"sv,
          "optional tuple elements cannot come after spread elements"sv,
          "overloaded function '{0}' declared here"sv,
          "prior spread element is here"sv,
          "private properties are not allowed in object literals"sv,
          "redeclaration of global variable"sv,
          "redeclaration of variable: {0}"sv,
          "redundant 'await'"sv,
          "redundant delete statement on variable"sv,
          "remove '{0}' to update an existing variable"sv,
          "remove this 'type'"sv,
          "return statement returns nothing (undefined)"sv,
          "see here"sv,
          "something happened"sv,
          "spread starts here"sv,
          "spread tuple elements cannot be optional"sv,
          "stray comma in function parameter"sv,
          "stray comma in let statement"sv,
          "switch statement is missing '{1}' around condition"sv,
          "switch statement needs parentheses around condition"sv,
          "this case will run instead"sv,
          "this tuple type is a named tuple type because at least one element has a name"sv,
          "this {0} looks fishy"sv,
          "this {1} looks fishy"sv,
          "try statement starts here"sv,
          "unclosed block comment"sv,
          "unclosed class; expected '}' by end of file"sv,
          "unclosed code block; expected '}' by end of file"sv,
          "unclosed identifier escape sequence"sv,
          "unclosed interface; expected '}' by end of file"sv,
          "unclosed object literal; expected '}'"sv,
          "unclosed regexp literal"sv,
          "unclosed string literal"sv,
          "unclosed template"sv,
          "unexpected '#'"sv,
          "unexpected ':' in expression; did you mean 'as'?"sv,
          "unexpected '?'"sv,
          "unexpected '?' when destructuring"sv,
          "unexpected '@'"sv,
          "unexpected '\\' in identifier"sv,
          "unexpected 'case' outside switch statement"sv,
          "unexpected 'catch' without 'try'"sv,
          "unexpected 'default' outside switch statement"sv,
          "unexpected 'finally' without 'try'"sv,
          "unexpected '{0}'"sv,
          "unexpected characters in binary literal"sv,
          "unexpected characters in hex literal"sv,
          "unexpected characters in number literal"sv,
          "unexpected characters in octal literal"sv,
          "unexpected control character"sv,
          "unexpected expression; missing key for object entry"sv,
          "unexpected identifier in expression; missing operator before"sv,
          "unexpected literal in parameter list; expected parameter name"sv,
          "unexpected statement before first switch case, expected 'case' or 'default'"sv,
          "unexpected token"sv,
          "unexpected token in export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'"sv,
          "unexpected token in variable declaration; expected variable name"sv,
          "unicode byte order mark (BOM) cannot appear before #! at beginning of script"sv,
          "unmatched '}'"sv,
          "unmatched indexing bracket"sv,
          "unmatched parenthesis"sv,
          "unopened block comment"sv,
          "upper case letters compared with toLowerCase"sv,
          "use ':' instead of '=' in object literals"sv,
          "use ':' instead of 'as' to type a function parameter"sv,
          "use 'while' instead to loop until a condition is false"sv,
          "use of undeclared type: {0}"sv,
          "use of undeclared variable: {0}"sv,
          "using '{0}' against a class literal always returns '{1}'"sv,
          "using '{0}' against a regular expression literal always returns '{1}'"sv,
          "using '{0}' against an array literal does not compare items"sv,
          "using '{0}' against an arrow function always returns '{1}'"sv,
          "using '{0}' against an object literal always returns '{1}'"sv,
          "variable already declared here"sv,
          "variable assigned before its declaration"sv,
          "variable declared here"sv,
          "variable used before declaration: {0}"sv,
          "what is this '{1}' nonsense?"sv,
          "while loop is missing '{1}' around condition"sv,
          "while loop needs parentheses around condition"sv,
          "with statement is missing '{1}' around expression"sv,
          "with statement needs parentheses around expression"sv,
          "write 'const' instead of '{0}' here"sv,
          "write the type assertion with 'as' here instead"sv,
          "{0} classes are not allowed in JavaScript"sv,
          "{0} is not the name of a parameter"sv,
          "{0} with no bindings"sv,
          "{1:headlinese} value must be a compile-time constant"sv,
          "~~~ invalid string, do not use outside benchmark ~~~"sv,
  };
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
