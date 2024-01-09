// Code generated by tools/compile-translations.cpp. DO NOT EDIT.
// source: po/*.po

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/sorted-search.h>
#include <quick-lint-js/port/consteval.h>
#include <string_view>

namespace quick_lint_js {
using namespace std::literals::string_view_literals;

constexpr std::uint32_t translation_table_locale_count = 5;
constexpr std::uint16_t translation_table_mapping_table_size = 606;
constexpr std::size_t translation_table_string_table_size = 82482;
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
          "\"jsx-mode\" must be a string; try \"none\" or \"react\""sv,
          "'!' (definite assignment assertion) cannot be used with an initial value"sv,
          "'!' (definite assignment assertion) is not allowed on 'declare' variables"sv,
          "'!' here treated as the TypeScript non-null assertion operator"sv,
          "'&' here"sv,
          "'**' operator cannot be used after unary '{1}' without parentheses"sv,
          "',' should be ';' instead"sv,
          "'.' is not allowed after generic arguments; write [\"{1}\"] instead"sv,
          "'.' operator needs a key name; use + to concatenate strings; use [] to access with a dynamic key"sv,
          "'...' belongs before the tuple element name, not before the type"sv,
          "'...' belongs only before the tuple element name, not also before the type"sv,
          "'...' goes here"sv,
          "':' should be 'extends' instead"sv,
          "'=' changes variables; to compare, use '===' instead"sv,
          "'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead"sv,
          "'?' belongs after the tuple element name, not after the type"sv,
          "'?' belongs only after the tuple element name, not also after the type"sv,
          "'?' creates a conditional expression"sv,
          "'?' goes here"sv,
          "'^' is the XOR operator; to exponentiate, use '**' instead"sv,
          "'abstract' here"sv,
          "'accessor' is not allowed for TypeScript interface fields"sv,
          "'accessor' keyword is not allowed on getters or setters"sv,
          "'accessor' keyword is not allowed on methods"sv,
          "'as const' is only allowed on literals (array, object, string, boolean) and enum members"sv,
          "'as const' located here"sv,
          "'async export' is not allowed; write 'export async' instead"sv,
          "'async static' is not allowed; write 'static async' instead"sv,
          "'async' keyword is not allowed on getters or setters"sv,
          "'await' cannot be followed by an arrow function; use 'async' instead"sv,
          "'await' is only allowed in async functions"sv,
          "'declare class' cannot contain static block"sv,
          "'declare class' fields cannot be initalized"sv,
          "'declare class' methods cannot be marked 'async'"sv,
          "'declare class' methods cannot be marked as a generator"sv,
          "'declare class' methods cannot contain a body"sv,
          "'declare function' cannot be marked 'async'"sv,
          "'declare function' cannot be marked as a generator"sv,
          "'declare function' cannot have a body"sv,
          "'declare function' here"sv,
          "'declare namespace' cannot contain statements, only declarations"sv,
          "'declare namespace' starts here"sv,
          "'declare {1}' cannot have initializer"sv,
          "'declare {1}' started here"sv,
          "'declare' here"sv,
          "'declare' is not allowed for TypeScript interface fields"sv,
          "'declare' or 'export' is required for {1} in .d.ts files"sv,
          "'declare' should not be written inside a 'declare namespace'"sv,
          "'declare' specified here"sv,
          "'do-while' loop"sv,
          "'else' has no corresponding 'if'"sv,
          "'export =' is not allowed; write 'export default' or 'module.exports =' (CommonJS) instead"sv,
          "'export as namespace' is not allowed in a namespace or module"sv,
          "'export as namespace' is only allowed in TypeScript .d.ts files"sv,
          "'export' keyword here"sv,
          "'export' must be on either all function signatures or none of them"sv,
          "'extends' must be before 'implements'"sv,
          "'for' loop"sv,
          "'function async' is not allowed; write 'async function' instead"sv,
          "'function' is here"sv,
          "'if' statement"sv,
          "'in' disallowed in C-style for loop initializer"sv,
          "'infer' is only allowed between 'extends' and '?' in conditional types"sv,
          "'out in' is not allowed; write 'in out' instead"sv,
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
          "'typeof' type"sv,
          "'while' loop"sv,
          "'with' statement"sv,
          "'{0} []' is always '{1}'"sv,
          "'{0}' access specifier must precede '{1}'"sv,
          "'{0}' already written here"sv,
          "'{0}' found here"sv,
          "'{0}' here"sv,
          "'{0}' is missing on overloaded method"sv,
          "'{0}' is not allowed for strings; use {1} instead"sv,
          "'{0}' is not allowed in TypeScript overload signatures"sv,
          "'{0}' is not allowed on methods"sv,
          "'{0}' is not allowed with '{1}'"sv,
          "'{0}' must precede '{1}'"sv,
          "'{0}' operator cannot be used before '**' without parentheses"sv,
          "'{0}' variance specifier cannot be listed twice"sv,
          "'{1}' is missing on overload signature"sv,
          "'{1}' statement starts here"sv,
          "'}' is not allowed directly in JSX text; write {{'}'} instead"sv,
          ".d.ts files cannot contain statements, only declarations"sv,
          "BigInt literal contains decimal point"sv,
          "BigInt literal contains exponent"sv,
          "C-style for loop is missing its third component"sv,
          "C-style for loops have only three semicolon-separated components"sv,
          "JSON syntax error"sv,
          "JSX prop is missing an expression"sv,
          "Keywords in TypeScript does not allow escape characters"sv,
          "React/JSX is not allowed in TypeScript code"sv,
          "React/JSX is not allowed in vanilla JavaScript code"sv,
          "RegExp literal flags cannot contain Unicode escapes"sv,
          "TypeScript 'as' type assertions are not allowed in JavaScript"sv,
          "TypeScript 'declare abstract class' is not allowed in JavaScript"sv,
          "TypeScript 'declare class' is not allowed in JavaScript"sv,
          "TypeScript 'declare function' is not allowed in JavaScript"sv,
          "TypeScript 'declare global' is not allowed in JavaScript"sv,
          "TypeScript 'declare global' is not allowed in namespaces"sv,
          "TypeScript 'declare module' with string name is not allowed in namespaces"sv,
          "TypeScript 'declare {1}' is not allowed in JavaScript"sv,
          "TypeScript 'declare' fields are now allowed in JavaScript"sv,
          "TypeScript 'implements' is not allowed in JavaScript"sv,
          "TypeScript 'satisfies' operator is not allowed in JavaScript"sv,
          "TypeScript <Type> type assertions are not allowed in JSX mode"sv,
          "TypeScript assignment-asserted fields are not supported in JavaScript"sv,
          "TypeScript generics are not allowed in JavaScript code"sv,
          "TypeScript global declaration block must use 'declare'"sv,
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
          "TypeScript overload signature can only have one semicolon"sv,
          "TypeScript parameter decorators are not allowed in JavaScript"sv,
          "TypeScript parameter properties are not allowed in JavaScript"sv,
          "TypeScript requires whitespace between '>' and '=' here"sv,
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
          "a 'typeof' type"sv,
          "a 'while' loop"sv,
          "a 'with' statement"sv,
          "a catch variable"sv,
          "a class"sv,
          "a class statement is not allowed as the body of {1:singular}"sv,
          "a class's 'extends' clause"sv,
          "a class's 'implements' clause"sv,
          "a const variable"sv,
          "a decorator exists here before 'export'"sv,
          "a function"sv,
          "a function statement is not allowed as the body of {1:singular}"sv,
          "a generic parameter"sv,
          "a labelled statement"sv,
          "a let variable"sv,
          "a lexical declaration is not allowed as the body of {1:singular}"sv,
          "a namespace"sv,
          "a parameter"sv,
          "a type"sv,
          "a type alias"sv,
          "a variable"sv,
          "a {{0} b }} c"sv,
          "abstract fields cannot have default values"sv,
          "abstract methods cannot be marked 'async'"sv,
          "abstract methods cannot be marked as a generator"sv,
          "abstract methods cannot contain a body"sv,
          "abstract properties are not allowed in interfaces"sv,
          "abstract properties are only allowed in abstract classes"sv,
          "abstract properties cannot be static"sv,
          "accessors cannot be optional"sv,
          "an 'if' statement"sv,
          "an enum"sv,
          "an import alias"sv,
          "an imported type"sv,
          "an imported variable"sv,
          "an index signature parameter"sv,
          "an interface"sv,
          "an interface's 'extends' clause"sv,
          "another invalid string, do not use outside benchmark"sv,
          "array started here"sv,
          "arrow is here"sv,
          "assertion signatures are only allowed as function return types"sv,
          "assigning to 'async' in a for-of loop requires parentheses"sv,
          "assignment assertion is not allowed on fields be marked 'declare'"sv,
          "assignment to const global variable"sv,
          "assignment to const variable before its declaration"sv,
          "assignment to imported variable"sv,
          "assignment to undeclared variable"sv,
          "assignment-asserted field must have a type annotation"sv,
          "assignment-asserted fields are not allowed in 'declare class'"sv,
          "assignment-asserted fields are not supported in interfaces"sv,
          "assignment-assertion fields cannot have default values"sv,
          "attribute has wrong capitalization; write '{1}' instead"sv,
          "binary number literal has no digits"sv,
          "break can only be used inside of a loop or switch"sv,
          "cannot 'export default' from inside a namespace"sv,
          "cannot access private identifier outside class"sv,
          "cannot assign to loop variable in for of/in loop"sv,
          "cannot assign to {1:singular}"sv,
          "cannot declare 'await' inside async function"sv,
          "cannot declare 'yield' inside generator function"sv,
          "cannot declare and export variable with 'export default'"sv,
          "cannot declare variable named keyword '{0}'"sv,
          "cannot delete variables in TypeScript"sv,
          "cannot export variable named 'let'"sv,
          "cannot export variable named keyword '{0}'"sv,
          "cannot import 'let'"sv,
          "cannot import a module from inside a 'declare namespace'"sv,
          "cannot import variable named keyword '{0}'"sv,
          "cannot reference private variables without object; use 'this.'"sv,
          "cannot update variable with '{0}' while declaring it"sv,
          "cannot use '...' on 'this' parameter"sv,
          "cannot use 'declare' keyword with 'import'"sv,
          "cannot use multiple `export default` statements in one module"sv,
          "cannot use type directly in its own definition"sv,
          "catch variable"sv,
          "catch variable can only be typed as '*', 'any', or 'unknown'"sv,
          "character is not allowed in identifiers"sv,
          "children end here"sv,
          "class"sv,
          "class 'extends' clause"sv,
          "class 'implements' clause"sv,
          "class is not marked abstract"sv,
          "classes cannot be named 'let'"sv,
          "code point in Unicode escape sequence must not be greater than U+10FFFF"sv,
          "code point out of range"sv,
          "commas are not allowed after spread parameter"sv,
          "commas are not allowed between class methods"sv,
          "computed enum member name must be a simple string"sv,
          "computed value disables enum autoincrement"sv,
          "const fields within classes are only allowed in TypeScript, not JavaScript"sv,
          "const variable"sv,
          "const variable declared here"sv,
          "const variables cannot have '!' (definite assignment assertion)"sv,
          "containing 'declare namespace' starts here"sv,
          "containing namespace or module declared here"sv,
          "continue can only be used inside of a loop"sv,
          "decorator belongs immediately before this overloaded method"sv,
          "decorator starts here"sv,
          "decorators are not allowed inside TypeScript interfaces"sv,
          "decorators are not allowed on abstract properties"sv,
          "decorators may be before 'export' or here, but not in both locations"sv,
          "decorators must appear after overload signatures"sv,
          "decorators must appear before '{1}"sv,
          "depth limit exceeded"sv,
          "do-while loop is missing '{1}' around condition"sv,
          "do-while loop needs parentheses around condition"sv,
          "do-while statement starts here"sv,
          "duplicated case clause in switch statement"sv,
          "enum"sv,
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
          "expected expression after '('"sv,
          "expected expression after 'case'"sv,
          "expected expression before newline"sv,
          "expected expression before semicolon"sv,
          "expected hexadecimal digits in Unicode escape sequence"sv,
          "expected parameter for arrow function, but got a literal instead"sv,
          "expected parameter for arrow function, but got an expression instead"sv,
          "expected variable name after '...'"sv,
          "expected variable name for 'catch'"sv,
          "expected variable name for 'import'-'as'"sv,
          "expected {1:headlinese}"sv,
          "expected {1:singular}"sv,
          "export default previously appeared here"sv,
          "exporting requires 'default'"sv,
          "exporting requires '{{' and '}'"sv,
          "extra ',' is not allowed between enum members"sv,
          "extra ',' is not allowed between function call arguments"sv,
          "field declared here"sv,
          "field marked abstract here"sv,
          "field was declared as an accessor here"sv,
          "first parameter starts here"sv,
          "for loop needs an iterable, or condition and update clauses"sv,
          "for-in loop expression cannot have semicolons"sv,
          "for-of loop expression cannot have semicolons"sv,
          "forwarding exports are only allowed in export-from"sv,
          "free {1} and {0} {1} {2}"sv,
          "function"sv,
          "function call started here"sv,
          "function called before declaration in block scope: {0}"sv,
          "function declared here"sv,
          "function overload signature cannot have generator '*'"sv,
          "function overload signature must be named '{1}'"sv,
          "function parameter cannot be parenthesized"sv,
          "function starts here"sv,
          "functions in .d.ts files cannot be async; return a Promise type"sv,
          "functions in .d.ts files cannot be generators; return a Generator type"sv,
          "functions in .d.ts files cannot have a body"sv,
          "functions/methods should not have '=>'"sv,
          "generator function '*' belongs after keyword function"sv,
          "generator function '*' belongs before function name"sv,
          "generic arrow function needs ',' here in TSX"sv,
          "generic parameter"sv,
          "getters and setters cannot be generators"sv,
          "getters and setters cannot have overload signatures"sv,
          "here"sv,
          "here is the assignment assertion operator"sv,
          "hex number literal has no digits"sv,
          "if statement is missing '{1}' around condition"sv,
          "if statement needs parentheses around condition"sv,
          "import alias"sv,
          "imported type"sv,
          "imported variable"sv,
          "imported variable declared here"sv,
          "incomplete export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'"sv,
          "index signature must be a field, not a method"sv,
          "index signature parameter"sv,
          "index signatures require a value type"sv,
          "index starts here"sv,
          "indexing requires an expression"sv,
          "initial value was given here"sv,
          "initializer starts here"sv,
          "inside namespace here"sv,
          "integer cannot be represented and will be rounded to '{1}'"sv,
          "interface"sv,
          "interface 'extends' clause"sv,
          "interface properties are always public and cannot be private"sv,
          "interface properties cannot be marked public explicitly"sv,
          "interfaces cannot contain static blocks"sv,
          "invalid UTF-8 sequence"sv,
          "invalid expression left of assignment"sv,
          "invalid function parameter"sv,
          "invalid hex escape sequence: {0}"sv,
          "invalid lone literal in object literal"sv,
          "keywords cannot contain escape sequences"sv,
          "label named 'await' not allowed in async function"sv,
          "labelled statement"sv,
          "leading commas are not allowed in generic parameter lists"sv,
          "legacy octal literal may not be BigInt"sv,
          "legacy octal literals may not contain underscores"sv,
          "let statement cannot declare variables named 'let'"sv,
          "let variable"sv,
          "lower case letters compared with toUpperCase"sv,
          "method starts here"sv,
          "methods cannot be marked 'declare'"sv,
          "methods cannot be readonly"sv,
          "methods should not use the 'function' keyword"sv,
          "misleading use of ',' operator in conditional statement"sv,
          "misleading use of ',' operator in index"sv,
          "mismatched JSX tags; expected '</{1}>'"sv,
          "missing ',' between array elements"sv,
          "missing ',' between variable declarations"sv,
          "missing ',', ';', or newline between object type entries"sv,
          "missing '...' in JSX attribute spread"sv,
          "missing ':' in conditional expression"sv,
          "missing '<>' and '</>' to enclose multiple children"sv,
          "missing '=' after variable"sv,
          "missing 'break;' or '// fallthrough' comment between statement and 'case'"sv,
          "missing 'export' keyword for function"sv,
          "missing 'if' after 'else'"sv,
          "missing 'new' in constructor type"sv,
          "missing 'while (condition)' for do-while statement"sv,
          "missing TypeScript type"sv,
          "missing arrow operator for arrow function"sv,
          "missing body for 'for' loop"sv,
          "missing body for 'if' statement"sv,
          "missing body for 'switch' statement"sv,
          "missing body for TypeScript interface"sv,
          "missing body for TypeScript namespace"sv,
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
          "missing class method or field after decorator"sv,
          "missing comma between generic parameters"sv,
          "missing comma between object literal entries"sv,
          "missing comparison; '{1}' does not extend to the right side of '{0}'"sv,
          "missing condition for if statement"sv,
          "missing condition for switch statement"sv,
          "missing condition for while statement"sv,
          "missing end of array; expected ']'"sv,
          "missing expression after type assertion"sv,
          "missing expression between parentheses"sv,
          "missing expression in placeholder within template literal"sv,
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
          "missing parameter name"sv,
          "missing parameters for arrow function"sv,
          "missing parentheses around left-hand side of '**'"sv,
          "missing parentheses around operand of '{0}'"sv,
          "missing parentheses around parameter"sv,
          "missing parentheses around self-invoked function"sv,
          "missing property name after '.' operator"sv,
          "missing property name between '.' and '.'"sv,
          "missing quotes around module name '{0}'"sv,
          "missing semicolon after 'declare class' method"sv,
          "missing semicolon after abstract method"sv,
          "missing semicolon after field"sv,
          "missing semicolon after index signature"sv,
          "missing semicolon after interface method"sv,
          "missing semicolon after method overload signature"sv,
          "missing semicolon after statement"sv,
          "missing semicolon between condition and update parts of for loop"sv,
          "missing semicolon between init and condition parts of for loop"sv,
          "missing type between '{1}' and '{0}'"sv,
          "missing value for object property"sv,
          "missing variable name"sv,
          "misspelled React attribute; write '{1}' instead"sv,
          "move the 'extends' clause before 'implements' here"sv,
          "move the parameter decorator before '{0}' here"sv,
          "namespace"sv,
          "namespace alias cannot use 'import type'"sv,
          "namespace starts here"sv,
          "new variable shadows existing variable"sv,
          "newline is not allowed after 'abstract'"sv,
          "newline is not allowed after 'asserts'"sv,
          "newline is not allowed after 'export declare'"sv,
          "newline is not allowed after 'interface'"sv,
          "newline is not allowed after 'type'"sv,
          "newline is not allowed after '{0}'"sv,
          "newline is not allowed after '{0}' modifier in generic parameter"sv,
          "newline is not allowed before '<'"sv,
          "newline is not allowed before 'extends'"sv,
          "newline is not allowed before 'is'"sv,
          "newline is not allowed between 'async' and 'function'"sv,
          "newline is not allowed between 'async' and arrow function parameter list"sv,
          "newline is not allowed between '{0}' and the method name"sv,
          "newline is not allowed between field name and '!'"sv,
          "newline is not allowed between variable name and '!'"sv,
          "nullish coalescing operator does nothing when left operand is never null"sv,
          "number literal contains consecutive underscores"sv,
          "number literal contains trailing underscore(s)"sv,
          "object literal started here"sv,
          "octal literal may not have decimal"sv,
          "octal literal may not have exponent"sv,
          "octal number literal has no digits"sv,
          "only one comma is allowed between or after generic parameters"sv,
          "only optional tuple elements can follow this optional tuple element"sv,
          "opening '<{1}>' tag here"sv,
          "optional parameter cannot be followed by a required parameter"sv,
          "optional parameter cannot have both '?' and initializer; remove '?'"sv,
          "optional tuple elements cannot come after spread elements"sv,
          "original semicolon is here"sv,
          "overload signature must have the correct access specifier ('{1}')"sv,
          "overload signature must match modifiers on this overload method"sv,
          "overloaded function '{0}' declared here"sv,
          "overloaded method is marked '{0}'"sv,
          "override properties are not allowed in interfaces"sv,
          "parameter"sv,
          "parameter decorator must be before other modifiers"sv,
          "parameter decorators are not allowed in 'declare class'"sv,
          "parameter decorators are not allowed in abstract methods"sv,
          "parameter decorators are only allowed in class methods"sv,
          "parameter properties are not allowed in 'declare class'"sv,
          "parameter properties are only allowed in class constructors"sv,
          "parameter properties cannot be a rest parameter"sv,
          "parameter properties cannot be destructured"sv,
          "parentheses are required around 'infer {1}'"sv,
          "prior spread element is here"sv,
          "private identifiers are not allowed for 'declare' fields; use 'private' instead"sv,
          "private properties are not allowed in object literals"sv,
          "property declared 'abstract' here"sv,
          "property declared static here"sv,
          "property declared using '{0}' here"sv,
          "redeclaration of global variable"sv,
          "redeclaration of variable: {0}"sv,
          "redundant 'await'"sv,
          "redundant delete statement on variable"sv,
          "remove '{0}' to update an existing variable"sv,
          "remove this 'type'"sv,
          "return statement returns nothing (undefined)"sv,
          "see here"sv,
          "semicolon is not allowed after decorators"sv,
          "something happened"sv,
          "space is required between '<' and '<' inside {1:headlinese}"sv,
          "spread starts here"sv,
          "spread tuple elements cannot be optional"sv,
          "static block starts here"sv,
          "static blocks cannot have a decorator"sv,
          "stray comma in function parameter"sv,
          "stray comma in let statement"sv,
          "string module name is only allowed with 'declare module'"sv,
          "switch statement is missing '{1}' around condition"sv,
          "switch statement needs parentheses around condition"sv,
          "this case will run instead"sv,
          "this required parameter appears after the optional parameter"sv,
          "this tuple type is a named tuple type because at least one element has a name"sv,
          "this {0} looks fishy"sv,
          "try statement starts here"sv,
          "type"sv,
          "type alias"sv,
          "type annotation is required when using '!' (definite assignment assertion)"sv,
          "type predicates are only allowed as function return types"sv,
          "type {0} is being defined here"sv,
          "unclosed block comment"sv,
          "unclosed class; expected '}' by end of file"sv,
          "unclosed code block; expected '}' by end of file"sv,
          "unclosed identifier escape sequence"sv,
          "unclosed interface; expected '}' by end of file"sv,
          "unclosed object literal; expected '}'"sv,
          "unclosed regexp literal"sv,
          "unclosed string literal"sv,
          "unclosed template"sv,
          "unexpected '!' after variable name"sv,
          "unexpected '#'"sv,
          "unexpected '...'; expected expression"sv,
          "unexpected ':' in expression; did you mean 'as'?"sv,
          "unexpected '?'"sv,
          "unexpected '?' in type; use '| void' to make an optional type"sv,
          "unexpected '?' when destructuring"sv,
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
          "unexpected whitespace between '!' and '=='"sv,
          "unicode byte order mark (BOM) cannot appear before #! at beginning of script"sv,
          "unintuitive operator precedence when using & and '{0}'; '{0}' evaluates before &"sv,
          "unknown JSX mode; try \"none\" or \"react\""sv,
          "unmatched '}'"sv,
          "unmatched indexing bracket"sv,
          "unmatched parenthesis"sv,
          "unopened block comment"sv,
          "upper case letters compared with toLowerCase"sv,
          "use ':' instead of '=' in object literals"sv,
          "use ':' instead of '{0}' to type a function parameter"sv,
          "use 'while' instead to loop until a condition is false"sv,
          "use of undeclared type: {0}"sv,
          "use of undeclared variable: {0}"sv,
          "using '{0}' against a class literal always returns '{1}'"sv,
          "using '{0}' against a regular expression literal always returns '{1}'"sv,
          "using '{0}' against an array literal does not compare items"sv,
          "using '{0}' against an arrow function always returns '{1}'"sv,
          "using '{0}' against an object literal always returns '{1}'"sv,
          "using a '.' after a '?.' might fail, since '?.' might return 'undefined'."sv,
          "variable"sv,
          "variable already declared here"sv,
          "variable assigned before its declaration"sv,
          "variable assignment to self is no-op"sv,
          "variable declared here"sv,
          "variable used before declaration: {0}"sv,
          "what is this '{1}' nonsense?"sv,
          "while loop is missing '{1}' around condition"sv,
          "while loop needs parentheses around condition"sv,
          "with statement is missing '{1}' around expression"sv,
          "with statement needs parentheses around expression"sv,
          "write 'const' instead of '{0}' here"sv,
          "write '{1}' here or remove it from the overload signature"sv,
          "write the decorator before here"sv,
          "write the type assertion with 'as' here instead"sv,
          "{0} classes are not allowed in JavaScript"sv,
          "{0} is not the name of a parameter"sv,
          "{0} with no bindings"sv,
          "{1:headlinese} declared here"sv,
          "{1:headlinese} value must be a compile-time constant"sv,
          "{1} cannot have an initializer in a .d.ts file"sv,
          "~~~ invalid string, do not use outside benchmark ~~~"sv,
  };
  // clang-format on

  auto it = sorted_search(std::begin(const_lookup_table),
                          std::end(const_lookup_table), untranslated);
  if (it != std::end(const_lookup_table)) {
    return std::uint16_t((it - std::begin(const_lookup_table)) + 1);
  }

  // If you see an error with the following line, translation-table-generated.h
  // is out of date.
  QLJS_CONSTEXPR_ASSERT(false);

  return 0;
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
