// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TEST_TRANSLATION_TABLE_GENERATED_H
#define QUICK_LINT_JS_TEST_TRANSLATION_TABLE_GENERATED_H

// This file is **GENERATED** by tools/compile-translations.go.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/translation.h>

namespace quick_lint_js {
// clang-format off
inline constexpr const char *test_locale_names[] = {
    "",
    "de",
    "en@loud",
    "fr_FR",
    "sv_SE",
};
// clang-format on

struct translated_string {
  translatable_message translatable;
  const char8 *expected_per_locale[5];
};

// clang-format off
inline constexpr translated_string test_translation_table[] = {
    {
        "\"global-groups\" entries must be strings"_translatable,
        {
            u8"\"global-groups\" entries must be strings",
            u8"Eintr\u00e4ge in \"global-groups\" m\u00fcssen Strings sein",
            u8"\"GLOBAL-GROUPS\" ENTRIES MUST BE STRINGS",
            u8"les entr\u00e9es de type \"global-groups\" doivent \u00eatre des cha\u00eenes de caract\u00e8res",
            u8"\"global-groups\" entries must be strings",
        },
    },
    {
        "\"global-groups\" must be a boolean or an array"_translatable,
        {
            u8"\"global-groups\" must be a boolean or an array",
            u8"\"global-groups\" muss entweder ein Boolean oder ein Array sein",
            u8"\"GLOBAL-GROUPS\" MUST BE A BOOLEAN OR AN ARRAY",
            u8"\"global-groups\" doit \u00eatre un bool\u00e9en ou un tableau",
            u8"\"global-groups\" must be a boolean or an array",
        },
    },
    {
        "\"globals\" descriptor \"shadowable\" property must be a boolean"_translatable,
        {
            u8"\"globals\" descriptor \"shadowable\" property must be a boolean",
            u8"Die \"shadowable\"-Eigenschaft des \"globals\"-Deskriptor muss ein Boolean sein",
            u8"\"GLOBALS\" DESCRIPTOR \"SHADOWABLE\" PROPERTY MUST BE A BOOLEAN",
            u8"pour le descripteur \"globals\" la propri\u00e9t\u00e9 \"shadowable\" doit \u00eare un bool\u00e9en",
            u8"\"globals\" descriptor \"shadowable\" property must be a boolean",
        },
    },
    {
        "\"globals\" descriptor \"writable\" property must be a boolean"_translatable,
        {
            u8"\"globals\" descriptor \"writable\" property must be a boolean",
            u8"Die \"writable\"-Eigenschaft des \"globals\"-Deskriptor muss ein Boolean sein",
            u8"\"GLOBALS\" DESCRIPTOR \"WRITABLE\" PROPERTY MUST BE A BOOLEAN",
            u8"pour le descripteur \"globals\" la propri\u00e9t\u00e9 \"writable\" doit \u00eatre un bool\u00e9en",
            u8"\"globals\" descriptor \"writable\" property must be a boolean",
        },
    },
    {
        "\"globals\" descriptor must be a boolean or an object"_translatable,
        {
            u8"\"globals\" descriptor must be a boolean or an object",
            u8"Der \"globals\"-Deskriptor muss entweder ein Boolean oder ein Array sein",
            u8"\"GLOBALS\" DESCRIPTOR MUST BE A BOOLEAN OR AN OBJECT",
            u8"le descripteur \"globals\" doit \u00eatre un bool\u00e9en ou un objet",
            u8"\"globals\" descriptor must be a boolean or an object",
        },
    },
    {
        "\"globals\" must be an object"_translatable,
        {
            u8"\"globals\" must be an object",
            u8"\"globles\" muss ein Objekt sein",
            u8"\"GLOBALS\" MUST BE AN OBJECT",
            u8"\"globals\" doit \u00eatre un objet",
            u8"\"globals\" must be an object",
        },
    },
    {
        "'**' operator cannot be used after unary '{1}' without parentheses"_translatable,
        {
            u8"'**' operator cannot be used after unary '{1}' without parentheses",
            u8"Ohne Klammern kann der Operator '**' nicht nach un\u00e4rem '{1}' verwendet werden",
            u8"'**' operator cannot be used after unary '{1}' without parentheses",
            u8"'**' operator cannot be used after unary '{1}' without parentheses",
            u8"'**' operator cannot be used after unary '{1}' without parentheses",
        },
    },
    {
        "'.' operator needs a key name; use + to concatenate strings; use [] to access with a dynamic key"_translatable,
        {
            u8"'.' operator needs a key name; use + to concatenate strings; use [] to access with a dynamic key",
            u8"Der '.'-Operator erfordert einen Schl\u00fcsselnamen; + verwenden, um Strings zu verketten; [] verwenden, um auf dynamische Schl\u00fcssel zuzugreifen",
            u8"'.' OPERATOR NEEDS A KEY NAME; USE + TO CONCATENATE STRINGS; USE [] TO ACCESS WITH A DYNAMIC KEY",
            u8"l'op\u00e9rateur '.' n\u00e9cessite un nom cl\u00e9 ; utiliser + pour concat\u00e9ner des cha\u00eenes de caract\u00e8res ; utiliser [] pour un acc\u00e8s avec une cl\u00e9 dynamique",
            u8"'.' operation beh\u00f6ver nyckelords namn; anv\u00e4nd + f\u00f6r att konkatenera str\u00e4ng; anv\u00e4nd [] f\u00f6r att komma \u00e5t med dynamisk nyckel",
        },
    },
    {
        "'=' changes variables; to compare, use '===' instead"_translatable,
        {
            u8"'=' changes variables; to compare, use '===' instead",
            u8"'=' ver\u00e4ndert Variable. F\u00fcr Vergleich '===' anstattdessen verwenden",
            u8"'=' changes variables; to compare, use '===' instead",
            u8"'=' changes variables; to compare, use '===' instead",
            u8"'=' changes variables; to compare, use '===' instead",
        },
    },
    {
        "'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead"_translatable,
        {
            u8"'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead",
            u8"'>' darf nicht direkt in JSX-Text verwendet werden. Anstattdessen {{'>} oder &gt; schreiben.",
            u8"'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead",
            u8"'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead",
            u8"'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead",
        },
    },
    {
        "'?' creates a conditional expression"_translatable,
        {
            u8"'?' creates a conditional expression",
            u8"'?' erzeugt einen Ausdruck mit tern\u00e4rem Operator",
            u8"'?' CREATES A CONDITIONAL EXPRESSION",
            u8"'?' cr\u00e9\u00e9 une expression conditionnelle",
            u8"'?' skapar vilkorsuttryck",
        },
    },
    {
        "'async static' is not allowed; write 'static async' instead"_translatable,
        {
            u8"'async static' is not allowed; write 'static async' instead",
            u8"'async static' ist ung\u00fcltig. 'static asysc' anstattdessen verwenden.",
            u8"'ASYNC STATIC' IS NOT ALLOWED; WRITE 'STATIC ASYNC' INSTEAD",
            u8"'async static' n'est pas autoris\u00e9 ; utiliser plut\u00f4t 'static async'",
            u8"'async static' is not allowed; write 'static async' instead",
        },
    },
    {
        "'await' cannot be followed by an arrow function; use 'async' instead"_translatable,
        {
            u8"'await' cannot be followed by an arrow function; use 'async' instead",
            u8"'await' kann nicht von einer Arrow-Funktion gefolgt werden. 'async' anstattdessen verwenden.",
            u8"'AWAIT' CANNOT BE FOLLOWED BY AN ARROW FUNCTION; USE 'ASYNC' INSTEAD",
            u8"'await' ne peut \u00eatre suivi d'une fonction fl\u00e9ch\u00e9e ; utiliser plut\u00f4t 'async'",
            u8"'await' cannot be followed by an arrow function; use 'async' instead",
        },
    },
    {
        "'await' is only allowed in async functions"_translatable,
        {
            u8"'await' is only allowed in async functions",
            u8"'await' darf nur in mit 'asysc' markierten Funktionen verwendet werden",
            u8"'AWAIT' IS ONLY ALLOWED IN ASYNC FUNCTIONS",
            u8"'await' n'est autoris\u00e9 que dans des fonctions async",
            u8"'await' \u00e4r enbart till\u00e5tet i en 'async' funktion",
        },
    },
    {
        "'do-while' loop"_translatable,
        {
            u8"'do-while' loop",
            u8"do-while-Schleife",
            u8"'do-while' loop",
            u8"'do-while' loop",
            u8"'do-while' loop",
        },
    },
    {
        "'else' has no corresponding 'if'"_translatable,
        {
            u8"'else' has no corresponding 'if'",
            u8"'else' ohne zugeh\u00f6riges 'if'",
            u8"'ELSE' HAS NO CORRESPONDING 'IF'",
            u8"'else' n'a pas de 'if' correspondant",
            u8"'else' har ingen anh\u00f6rig 'if'",
        },
    },
    {
        "'for' loop"_translatable,
        {
            u8"'for' loop",
            u8"for-Schleife",
            u8"'for' loop",
            u8"'for' loop",
            u8"'for' loop",
        },
    },
    {
        "'if' statement"_translatable,
        {
            u8"'if' statement",
            u8"if-Anweisung",
            u8"'if' statement",
            u8"'if' statement",
            u8"'if' statement",
        },
    },
    {
        "'in' disallowed in C-style for loop initializer"_translatable,
        {
            u8"'in' disallowed in C-style for loop initializer",
            u8"Ung\u00fcltiges 'in' innerhalb Initialisierung der C-\u00e4hnlichen for-Schleife",
            u8"'IN' DISALLOWED IN C-STYLE FOR LOOP INITIALIZER",
            u8"'in' d\u00e9sactiv\u00e9 dans l'initialisation des boucles for de style C",
            u8"'in' \u00e4r otill\u00e5tet i C-stil for loop initierare",
        },
    },
    {
        "'while' loop"_translatable,
        {
            u8"'while' loop",
            u8"while-Schleife",
            u8"'while' loop",
            u8"'while' loop",
            u8"'while' loop",
        },
    },
    {
        "'with' statement"_translatable,
        {
            u8"'with' statement",
            u8"with-Anweisung",
            u8"'with' statement",
            u8"'with' statement",
            u8"'with' statement",
        },
    },
    {
        "'{0}' found here"_translatable,
        {
            u8"'{0}' found here",
            u8"'{0}' ist hier",
            u8"'{0}' found here",
            u8"'{0}' found here",
            u8"'{0}' found here",
        },
    },
    {
        "'{0}' is not allowed for strings; use {1} instead"_translatable,
        {
            u8"'{0}' is not allowed for strings; use {1} instead",
            u8"'{0}' ist f\u00fcr Strings nicht erlaubt. '{1}' anstattdessen verwenden.",
            u8"'{0}' is not allowed for strings; use {1} instead",
            u8"'{0}' is not allowed for strings; use {1} instead",
            u8"'{0}' is not allowed for strings; use {1} instead",
        },
    },
    {
        "'{0}' operator cannot be used before '**' without parentheses"_translatable,
        {
            u8"'{0}' operator cannot be used before '**' without parentheses",
            u8"Der Operator '{0}' kann nicht vor '**' ohne Klammern benutzt werden",
            u8"'{0}' operator cannot be used before '**' without parentheses",
            u8"'{0}' operator cannot be used before '**' without parentheses",
            u8"'{0}' operator cannot be used before '**' without parentheses",
        },
    },
    {
        "'}' is not allowed directly in JSX text; write {{'}'} instead"_translatable,
        {
            u8"'}' is not allowed directly in JSX text; write {{'}'} instead",
            u8"'}' darf nicht direkt in JSX-Text verwendet werden. Anstattdessen {{'}'} schreiben",
            u8"'}' is not allowed directly in JSX text; write {{'}'} instead",
            u8"'}' is not allowed directly in JSX text; write {{'}'} instead",
            u8"'}' is not allowed directly in JSX text; write {{'}'} instead",
        },
    },
    {
        "BigInt literal contains decimal point"_translatable,
        {
            u8"BigInt literal contains decimal point",
            u8"BigInt-Literal mit Dezimalpunkt",
            u8"BIGINT LITERAL CONTAINS DECIMAL POINT",
            u8"le lit\u00e9ral BigInt contient un s\u00e9parateur de d\u00e9cimales",
            u8"BigInt heltallitter\u00e4r inneh\u00e5ller decimaler",
        },
    },
    {
        "BigInt literal contains exponent"_translatable,
        {
            u8"BigInt literal contains exponent",
            u8"BigInt-Literal mit Exponenten",
            u8"BIGINT LITERAL CONTAINS EXPONENT",
            u8"le lit\u00e9ral BigInt contient un exposant",
            u8"BigInt heltallitter\u00e4r inneh\u00e5ller exponent",
        },
    },
    {
        "C-style for loop is missing its third component"_translatable,
        {
            u8"C-style for loop is missing its third component",
            u8"C-\u00e4hnliche for-Schleife fehlt drittes Argument",
            u8"C-STYLE FOR LOOP IS MISSING ITS THIRD COMPONENT",
            u8"troisi\u00e8me argument manquant pour la boucle de style C",
            u8"C-stil 'for' loop saknar tredje komponentent",
        },
    },
    {
        "C-style for loops have only three semicolon-separated components"_translatable,
        {
            u8"C-style for loops have only three semicolon-separated components",
            u8"C-\u00e4hnliche for-Schleifen haben nur drei durch Semikolon getrennte Komponenten",
            u8"C-STYLE FOR LOOPS HAVE ONLY THREE SEMICOLON-SEPARATED COMPONENTS",
            u8"les boucles for de style C ne poss\u00e8dent que trois composantes s\u00e9par\u00e9es par des points-virgules",
            u8"C-stil for loops har enbart tre semikolon separerande komponenter",
        },
    },
    {
        "JSON syntax error"_translatable,
        {
            u8"JSON syntax error",
            u8"Syntaxfehler in JSON",
            u8"JSON SYNTAX ERROR",
            u8"erreur de syntaxe JSON",
            u8"JSON syntax error",
        },
    },
    {
        "React/JSX is not yet implemented"_translatable,
        {
            u8"React/JSX is not yet implemented",
            u8"React/JSX ist noch nicht implementiert",
            u8"REACT/JSX IS NOT YET IMPLEMENTED",
            u8"React/JSX n'est pas encore impl\u00e9ment\u00e9",
            u8"React/JSX is not yet implemented",
        },
    },
    {
        "RegExp literal flags cannot contain Unicode escapes"_translatable,
        {
            u8"RegExp literal flags cannot contain Unicode escapes",
            u8"RegExp-Literale d\u00fcrfen keine Unicode Escapes enthalten",
            u8"REGEXP LITERAL FLAGS CANNOT CONTAIN UNICODE ESCAPES",
            u8"un litt\u00e9ral RegExp ne peut contenir des \u00e9chappements Unicode",
            u8"RegExp literal flags cannot contain Unicode escapes",
        },
    },
    {
        "TypeScript's 'enum' feature is not yet implemented by quick-lint-js"_translatable,
        {
            u8"TypeScript's 'enum' feature is not yet implemented by quick-lint-js",
            u8"Das 'enum' Feature aus TypeScript ist noch nicht in quick-lint-js implementiert",
            u8"TYPESCRIPT'S 'ENUM' FEATURE IS NOT YET IMPLEMENTED BY QUICK-LINT-JS",
            u8"la fonctionnalit\u00e9 'enum' de TypeScript n'est pas encore impl\u00e9ment\u00e9e dans quick-lint-js",
            u8"TypeScripts 'enum' \u00e4r inte \u00e4nnu implementerad av quick-lint-js",
        },
    },
    {
        "a 'do-while' loop"_translatable,
        {
            u8"a 'do-while' loop",
            u8"eine do-While-Schleife",
            u8"a 'do-while' loop",
            u8"a 'do-while' loop",
            u8"a 'do-while' loop",
        },
    },
    {
        "a 'for' loop"_translatable,
        {
            u8"a 'for' loop",
            u8"eine for-Schleife",
            u8"a 'for' loop",
            u8"a 'for' loop",
            u8"a 'for' loop",
        },
    },
    {
        "a 'while' loop"_translatable,
        {
            u8"a 'while' loop",
            u8"eine while-Schleife",
            u8"a 'while' loop",
            u8"a 'while' loop",
            u8"a 'while' loop",
        },
    },
    {
        "a 'with' statement"_translatable,
        {
            u8"a 'with' statement",
            u8"eine with-Anweisung",
            u8"a 'with' statement",
            u8"a 'with' statement",
            u8"a 'with' statement",
        },
    },
    {
        "a class statement is not allowed as the body of {1:singular}"_translatable,
        {
            u8"a class statement is not allowed as the body of {1:singular}",
            u8"Ein Klassen-Statement darf nicht der K\u00f6rper von {1:singular} sein",
            u8"A CLASS STATEMENT IS NOT ALLOWED AS THE BODY OF {1:singular}",
            u8"une d\u00e9claration de classe n'est pas autoris\u00e9e dans le corps de {1:singular}",
            u8"klass sats \u00e4r inte till\u00e5tet som ett stycke av {1:singular}",
        },
    },
    {
        "a function statement is not allowed as the body of {1:singular}"_translatable,
        {
            u8"a function statement is not allowed as the body of {1:singular}",
            u8"Ein Funktionsstatement ist ung\u00fcltig als K\u00f6rper von {1:singular}",
            u8"A FUNCTION STATEMENT IS NOT ALLOWED AS THE BODY OF {1:singular}",
            u8"une instruction de fonction ne peut faire partie du corps de {1:singular}",
            u8"en funktion sats \u00e4r inte till\u00e5tet som stycke till {1:singular}",
        },
    },
    {
        "a lexical declaration is not allowed as the body of {1:singular}"_translatable,
        {
            u8"a lexical declaration is not allowed as the body of {1:singular}",
            u8"Eine lexikalische Deklaration ist nicht als K\u00f6rper von {1:singular} erlaubt",
            u8"A LEXICAL DECLARATION IS NOT ALLOWED AS THE BODY OF {1:singular}",
            u8"une d\u00e9claration lexicale ne peut constituer le corps de {1:singular}",
            u8"lexikaliskt deklaration \u00e4r inte till\u00e5tet inuti ett stycke av {1:singular}",
        },
    },
    {
        "a {{0} b }} c"_translatable,
        {
            u8"a {{0} b }} c",
            u8"a {{0} b }} c",
            u8"a {{0} b }} c",
            u8"a {{0} b }} c",
            u8"a {{0} b }} c",
        },
    },
    {
        "an 'if' statement"_translatable,
        {
            u8"an 'if' statement",
            u8"eine if-Anweisung",
            u8"an 'if' statement",
            u8"an 'if' statement",
            u8"an 'if' statement",
        },
    },
    {
        "another invalid string, do not use outside benchmark"_translatable,
        {
            u8"another invalid string, do not use outside benchmark",
            u8"another invalid string, do not use outside benchmark",
            u8"another invalid string, do not use outside benchmark",
            u8"another invalid string, do not use outside benchmark",
            u8"another invalid string, do not use outside benchmark",
        },
    },
    {
        "array started here"_translatable,
        {
            u8"array started here",
            u8"Array beginnt hier",
            u8"array started here",
            u8"tableau d\u00e9but\u00e9 ici",
            u8"lista startar h\u00e4r",
        },
    },
    {
        "arrow is here"_translatable,
        {
            u8"arrow is here",
            u8"Arrow ist hier",
            u8"ARROW IS HERE",
            u8"la fl\u00e8che est ici",
            u8"pilen \u00e4r h\u00e4r",
        },
    },
    {
        "assigning to 'async' in a for-of loop requires parentheses"_translatable,
        {
            u8"assigning to 'async' in a for-of loop requires parentheses",
            u8"Zuweisung an 'async' in einer for-of-Schleife erfordert Verwendung von Klammern",
            u8"ASSIGNING TO 'ASYNC' IN A FOR-OF LOOP REQUIRES PARENTHESES",
            u8"une affectation de type 'async' dans une boucle for-of n\u00e9cessite l'usage de parenth\u00e8ses",
            u8"tilldelning av 'async' i for-of loop kr\u00e4ver paranteser",
        },
    },
    {
        "assignment to const global variable"_translatable,
        {
            u8"assignment to const global variable",
            u8"Zuweisung an globale konstante Variable",
            u8"ASSIGNMENT TO CONST GLOBAL VARIABLE",
            u8"affectation \u00e0 une variable globale constante",
            u8"tilldelar till global konstant variabel",
        },
    },
    {
        "assignment to const variable"_translatable,
        {
            u8"assignment to const variable",
            u8"Zuweisung an konstante Variable",
            u8"ASSIGNMENT TO CONST VARIABLE",
            u8"affectation \u00e0 une variable constante",
            u8"tilldelar till konstant variabel",
        },
    },
    {
        "assignment to const variable before its declaration"_translatable,
        {
            u8"assignment to const variable before its declaration",
            u8"Zuweisung an konstante Variable vor Deklaration",
            u8"VARIABLE USED BEFORE DECLARATION: {0}",
            u8"affectation \u00e0 une variable constante avant sa d\u00e9claration",
            u8"tilldelar konstant variable f\u00f6re dens deklaration",
        },
    },
    {
        "assignment to imported variable"_translatable,
        {
            u8"assignment to imported variable",
            u8"Zuweisung an importierte Variable",
            u8"assignment to imported variable",
            u8"assignment to imported variable",
            u8"assignment to imported variable",
        },
    },
    {
        "assignment to undeclared variable"_translatable,
        {
            u8"assignment to undeclared variable",
            u8"Zuweisung an nicht deklarierte Variable",
            u8"ASSIGNMENT TO UNDECLARED VARIABLE",
            u8"affectation \u00e0 une variable non d\u00e9clar\u00e9e",
            u8"tilldelar v\u00e4rde till variabel f\u00f6re deklaration",
        },
    },
    {
        "attribute has wrong capitalization; write '{1}' instead"_translatable,
        {
            u8"attribute has wrong capitalization; write '{1}' instead",
            u8"Attribut mit falscher Gro\u00df- und Kleinschreibung; '{1}' anstattdessen schreiben",
            u8"attribute has wrong capitalization; write '{1}' instead",
            u8"attribute has wrong capitalization; write '{1}' instead",
            u8"attribute has wrong capitalization; write '{1}' instead",
        },
    },
    {
        "binary number literal has no digits"_translatable,
        {
            u8"binary number literal has no digits",
            u8"Bin\u00e4res Zahlenliteral ohne Ziffern",
            u8"BINARY NUMBER LITERAL HAS NO DIGITS",
            u8"le litt\u00e9ral num\u00e9rique binaire n'a pas de chiffres",
            u8"bin\u00e4ra nummerlitteraler has inga siffror",
        },
    },
    {
        "break can only be used inside of a loop or switch"_translatable,
        {
            u8"break can only be used inside of a loop or switch",
            u8"'break' ist nur innerhalb von Schleifen und dem switch-Statement g\u00fcltig",
            u8"BREAK CAN ONLY BE USED INSIDE OF A LOOP OR SWITCH",
            u8"break ne peut pas \u00eatre utilis\u00e9 \u00e0 l'int\u00e9rieur d'une boucle ou d'une instruction switch",
            u8"break kan enbart vara inuti en loop eller switch",
        },
    },
    {
        "cannot access private identifier outside class"_translatable,
        {
            u8"cannot access private identifier outside class",
            u8"Zugriff auf privaten Bezeichner au\u00dferhalb der Klasse",
            u8"CANNOT ACCESS PRIVATE IDENTIFIER OUTSIDE CLASS",
            u8"impossible d'acc\u00e9der \u00e0 un identifiant priv\u00e9 en dehors d'une classe",
            u8"cannot access private identifier outside class",
        },
    },
    {
        "cannot assign to loop variable in for of/in loop"_translatable,
        {
            u8"cannot assign to loop variable in for of/in loop",
            u8"Ung\u00fcltige Zuweisung an Iterationsvariable der for-in/of-Schleife",
            u8"CANNOT ASSIGN TO LOOP VARIABLE IN FOR OF/IN LOOP",
            u8"impossible d'affecter la variable de boucle dans une boucle for of/in",
            u8"cannot assign to loop variable in for of/in loop",
        },
    },
    {
        "cannot declare 'await' inside async function"_translatable,
        {
            u8"cannot declare 'await' inside async function",
            u8"Kann 'await' nicht innerhalb einer async-Funktion deklarieren",
            u8"CANNOT DECLARE 'AWAIT' INSIDE ASYNC FUNCTION",
            u8"impossible de d\u00e9clarer 'await' \u00e0 l'int\u00e9rieur d'une fonction async",
            u8"kan inte deklarera 'await' inuti async funktion",
        },
    },
    {
        "cannot declare 'yield' inside generator function"_translatable,
        {
            u8"cannot declare 'yield' inside generator function",
            u8"Kann 'yield' nicht innerhalb einer Generatorfunktion deklarieren",
            u8"CANNOT DECLARE 'YIELD' INSIDE GENERATOR FUNCTION",
            u8"impossible de d\u00e9clarer 'yield' \u00e0 l'int\u00e9rieur d'une fonction g\u00e9n\u00e9ratrice",
            u8"kan inte deklarera 'yield' inuti en generatorfunktion",
        },
    },
    {
        "cannot declare and export variable with 'export default'"_translatable,
        {
            u8"cannot declare and export variable with 'export default'",
            u8"Kann keine Variable namens 'let' deklarieren und exportieren",
            u8"CANNOT DECLARE AND EXPORT VARIABLE WITH 'EXPORT DEFAULT'",
            u8"impossible de d\u00e9clarer et d'exporter une variable avec 'export default'",
            u8"kan inte deklarera och exportera variabel med 'export default'",
        },
    },
    {
        "cannot declare variable named keyword '{0}'"_translatable,
        {
            u8"cannot declare variable named keyword '{0}'",
            u8"Kann keine Variable namens 'let' exportieren",
            u8"CANNOT DECLARE VARIABLE NAMED KEYWORD '{0}'",
            u8"impossible de d\u00e9clarer une variable nomm\u00e9e comme le mot-cl\u00e9 '{0}'",
            u8"kan inte deklarera variabel med nyckelord '{0}'",
        },
    },
    {
        "cannot export variable named 'let'"_translatable,
        {
            u8"cannot export variable named 'let'",
            u8"Kann keine Variable namens 'let' exportieren",
            u8"CANNOT EXPORT VARIABLE NAMED 'let'",
            u8"impossible d'exporter une variable nomm\u00e9e 'let'",
            u8"kan inte exportera variabel vid namn 'let'",
        },
    },
    {
        "cannot export variable named keyword '{0}'"_translatable,
        {
            u8"cannot export variable named keyword '{0}'",
            u8"Ung\u00fcltiger Export einer Variablen mit dem Namen des Schl\u00fcsselwortes '{0}'",
            u8"CANNOT EXPORT VARIABLE NAMED KEYWORD '{0}'",
            u8"impossible d'exporter une variable nomm\u00e9e comme le mot-cl\u00e9 '{0}'",
            u8"kan inte exportera variabel med nyckelord '{0}'",
        },
    },
    {
        "cannot import 'let'"_translatable,
        {
            u8"cannot import 'let'",
            u8"'let' kann nicht importiert werden",
            u8"CANNOT IMPORT 'LET'",
            u8"impossible d'importer 'let'",
            u8"kan inte importera 'let'",
        },
    },
    {
        "cannot import variable named keyword '{0}'"_translatable,
        {
            u8"cannot import variable named keyword '{0}'",
            u8"Kann keine Variable namens 'let' exportieren",
            u8"CANNOT IMPORT VARIABLE NAMED KEYWORD '{0}'",
            u8"impossible d'importer une variable nomm\u00e9e comme un mot-cl\u00e9 '{0}'",
            u8"kan inte importera variabel med namngivet nyckelord '{0}'",
        },
    },
    {
        "cannot reference private variables without object; use 'this.'"_translatable,
        {
            u8"cannot reference private variables without object; use 'this.'",
            u8"Private Felder des Objekts k\u00f6nnen nicht ohne 'this.' referenziert werden",
            u8"CANNOT REFERENCE PRIVATE VARIABLES WITHOUT OBJECT; USE 'THIS.'",
            u8"impossible de r\u00e9f\u00e9rencer des variables priv\u00e9es sans objet ; utiliser 'this.'",
            u8"kan inte referera en privat variabel utan ett objekt; anv\u00e4nd 'this'",
        },
    },
    {
        "cannot update variable with '{0}' while declaring it"_translatable,
        {
            u8"cannot update variable with '{0}' while declaring it",
            u8"An Variable '{0}' kann nicht w\u00e4hrend der Deklaration zugewiesen werden",
            u8"CANNOT UPDATE VARIABLE WITH '{0}' WHILE DECLARING IT",
            u8"impossible d'actualiser la variable avec '{0}' pendant sa d\u00e9claration",
            u8"kan inte uppdatera variabel med '{0} under deklaration'",
        },
    },
    {
        "character is not allowed in identifiers"_translatable,
        {
            u8"character is not allowed in identifiers",
            u8"Ung\u00fcltiges Zeichen in Bezeichner",
            u8"CHARACTER IS NOT ALLOWED IN IDENTIFIERS",
            u8"caract\u00e8re non autoris\u00e9 dans les identifiants",
            u8"tecknet \u00e4r inte till\u00e5tet i indentifierare",
        },
    },
    {
        "children end here"_translatable,
        {
            u8"children end here",
            u8"Children enden hier",
            u8"children end here",
            u8"children end here",
            u8"children end here",
        },
    },
    {
        "classes cannot be named 'await' in async function"_translatable,
        {
            u8"classes cannot be named 'await' in async function",
            u8"classes cannot be named 'await' in async function",
            u8"classes cannot be named 'await' in async function",
            u8"classes cannot be named 'await' in async function",
            u8"classes cannot be named 'await' in async function",
        },
    },
    {
        "classes cannot be named 'let'"_translatable,
        {
            u8"classes cannot be named 'let'",
            u8"Klassen d\u00fcrfen nicht mit 'let' benannt werden",
            u8"CLASSES CANNOT BE NAMED 'LET'",
            u8"une classe ne peut \u00eatre nomm\u00e9e 'let'",
            u8"klass kan inte ben\u00e4mnas 'let'",
        },
    },
    {
        "code point in Unicode escape sequence must not be greater than U+10FFFF"_translatable,
        {
            u8"code point in Unicode escape sequence must not be greater than U+10FFFF",
            u8"Codepunkt innerhalb der Unicode-Escapesequenz darf nicht gr\u00f6\u00dfer als U+10FFFF sein",
            u8"CODE POINT IN UNICODE ESCAPE SEQUENCE MUST NOT BE GREATER THAN U+10FFFF",
            u8"un point de code dans une s\u00e9quence d'\u00e9chappement Unicode ne peut d\u00e9passer la valeur U+10FFFF",
            u8"code point in Unicode escape sequence must not be greater than U+10FFFF",
        },
    },
    {
        "code point out of range"_translatable,
        {
            u8"code point out of range",
            u8"Codepunkt au\u00dferhalb des zul\u00e4ssigen Bereichs",
            u8"CODE POINT OUT OF RANGE",
            u8"point de code hors limite",
            u8"kod punkt ur span",
        },
    },
    {
        "commas are not allowed after spread parameter"_translatable,
        {
            u8"commas are not allowed after spread parameter",
            u8"Ung\u00fcltiges Komma nach Spread-Parameter",
            u8"COMMAS ARE NOT ALLOWED AFTER SPREAD PARAMETER",
            u8"les virgules ne sont pas autoris\u00e9es apr\u00e8s un param\u00e8tre d'expansion",
            u8"kommatecken \u00e4r inte till\u00e5tet efter sprid parameter",
        },
    },
    {
        "commas are not allowed between class methods"_translatable,
        {
            u8"commas are not allowed between class methods",
            u8"Kommata sind nicht erlaubt zwischen Methoden einer Klasse",
            u8"COMMAS ARE NOT ALLOWED BETWEEN CLASS METHODS",
            u8"les virgules ne sont pas autoris\u00e9es entre les m\u00e9thodes de classe",
            u8"commas are not allowed between class methods",
        },
    },
    {
        "const fields within classes are only allowed in TypeScript, not JavaScript"_translatable,
        {
            u8"const fields within classes are only allowed in TypeScript, not JavaScript",
            u8"Innerhalb Klassen sind const-Felder nur in TypeScript g\u00fcltig, nicht jedoch in JavaScript",
            u8"CONST FIELDS WITHIN CLASSES ARE ONLY ALLOWED IN TYPESCRIPT, NOT JAVASCRIPT",
            u8"les champs const dans des classes ne sont autoris\u00e9s qu'avec TypeScript, et pas JavaScript",
            u8"const fields within classes are only allowed in TypeScript, not JavaScript",
        },
    },
    {
        "const variable declared here"_translatable,
        {
            u8"const variable declared here",
            u8"Konstante Variable wurde hier deklariert",
            u8"VARIABLE DECLARED HERE",
            u8"variable constante d\u00e9clar\u00e9e ici",
            u8"konstant variabel deklarerad h\u00e4r",
        },
    },
    {
        "continue can only be used inside of a loop"_translatable,
        {
            u8"continue can only be used inside of a loop",
            u8"continue ist nur innerhalb von Schleifen g\u00fcltig",
            u8"CONTINUE CAN ONLY BE USED INSIDE OF A LOOP",
            u8"continue ne peut \u00eatre utilis\u00e9 qu'\u00e0 l'int\u00e9rieur d'une boucle",
            u8"continue kan enbart vara inuti en loop",
        },
    },
    {
        "depth limit exceeded"_translatable,
        {
            u8"depth limit exceeded",
            u8"Maximale Verschachtelungstiefe \u00fcberschritten",
            u8"DEPTH LIMIT EXCEEDED",
            u8"limite de profondeur d\u00e9pass\u00e9e",
            u8"djup begr\u00e4nsning \u00f6verskriden",
        },
    },
    {
        "do-while loop is missing '{1}' around condition"_translatable,
        {
            u8"do-while loop is missing '{1}' around condition",
            u8"'{1}' fehlt um Bedingung der do-while-Schleife",
            u8"DO-WHILE LOOP IS MISSING '{1}' AROUND CONDITION",
            u8"une boucle do-while n\u00e9cesite '{1}' autour de la condition",
            u8"do-while loop saknar '{1}' runt vilkor",
        },
    },
    {
        "do-while loop needs parentheses around condition"_translatable,
        {
            u8"do-while loop needs parentheses around condition",
            u8"Klammern um Bedingung der do-while-Schleife fehlen",
            u8"DO-WHILE LOOP NEEDS PARENTHESES AROUND CONDITION",
            u8"une boucle do-while n\u00e9cessite des parenth\u00e8ses autour de la condition",
            u8"do-while loop beh\u00f6ver paranteser runt vilkor",
        },
    },
    {
        "do-while statement starts here"_translatable,
        {
            u8"do-while statement starts here",
            u8"do-while-Schleife beginnt hier",
            u8"DO-WHILE STATEMENT STARTS HERE",
            u8"instruction do-while d\u00e9but\u00e9e ici",
            u8"do-while p\u00e5st\u00e5ende startar h\u00e4r",
        },
    },
    {
        "escaped character is not allowed in identifiers"_translatable,
        {
            u8"escaped character is not allowed in identifiers",
            u8"Escape-Zeichen darf nicht nicht in Bezeichnern verwendet werden",
            u8"ESCAPED CHARACTER IS NOT ALLOWED IN IDENTIFIERS",
            u8"caract\u00e8re \u00e9chapp\u00e9 non permis dans les identifiants",
            u8"flykttecken \u00e4r inte till\u00e5tet i indentifierare",
        },
    },
    {
        "escaping '-' is not allowed in tag names; write '-' instead"_translatable,
        {
            u8"escaping '-' is not allowed in tag names; write '-' instead",
            u8"Escape von '-' ist in Tagnamen nicht erlaubt. '-' anstattdessen schreiben",
            u8"escaping '-' is not allowed in tag names; write '-' instead",
            u8"escaping '-' is not allowed in tag names; write '-' instead",
            u8"escaping '-' is not allowed in tag names; write '-' instead",
        },
    },
    {
        "event attributes must be camelCase: '{1}'"_translatable,
        {
            u8"event attributes must be camelCase: '{1}'",
            u8"Event-Attribute m\u00fcssen im camelCase geschrieben werden: '{1}'",
            u8"event attributes must be camelCase: '{1}'",
            u8"event attributes must be camelCase: '{1}'",
            u8"event attributes must be camelCase: '{1}'",
        },
    },
    {
        "existing variable declared here"_translatable,
        {
            u8"existing variable declared here",
            u8"Bereits existierende Variable wurde hier deklariert",
            u8"existing variable declared here",
            u8"existing variable declared here",
            u8"existing variable declared here",
        },
    },
    {
        "expected ')' to close function call"_translatable,
        {
            u8"expected ')' to close function call",
            u8"')' f\u00fcr Funktionsaufruf fehlt",
            u8"EXPECTED ')' TO CLOSE FUNCTION CALL",
            u8"')' attendu pour clore un appel de fonction",
            u8"f\u00f6rv\u00e4ntade ')' f\u00f6r att slutf\u00f6ra funktionkallelse",
        },
    },
    {
        "expected ',' between object literal entries"_translatable,
        {
            u8"expected ',' between object literal entries",
            u8"Komma fehlt zwischen Feldern des Objekt-Literals",
            u8"EXPECTED ',' BETWEEN OBJECT LITERAL ENTRIES",
            u8"',' attendu entre les entr\u00e9es litt\u00e9rales d'un objet",
            u8"f\u00f6rv\u00e4ntade ',' mellan f\u00f6rekommande objektlitteral",
        },
    },
    {
        "expected 'as' between '{1}' and '{2}'"_translatable,
        {
            u8"expected 'as' between '{1}' and '{2}'",
            u8"'as' zwischen '{1}' und '{2}' erwartet",
            u8"EXPECTED 'AS' BETWEEN '{1}' AND '{2}'",
            u8"'as' attendu entre '{1}' and '{2}'",
            u8"f\u00f6rv\u00e4ntade 'as' mellan '{1}' och '{2}'",
        },
    },
    {
        "expected 'from \"name_of_module.mjs\"'"_translatable,
        {
            u8"expected 'from \"name_of_module.mjs\"'",
            u8"'from \"name_of_module.mjs\"' erwartet",
            u8"EXPECTED 'FROM \"NAME_OF_MODULE.MJS\"'",
            u8"'from \"name_of_module.mjs\"' attendu",
            u8"f\u00f6rv\u00e4ntade 'from \"name_of_module.mjs\"'",
        },
    },
    {
        "expected 'from' before module specifier"_translatable,
        {
            u8"expected 'from' before module specifier",
            u8"'from' vor Modulspezifikation erwartet",
            u8"EXPECTED 'FROM' BEFORE MODULE SPECIFIER",
            u8"'from' attendu avant un sp\u00e9cificateur de module",
            u8"f\u00f6rv\u00e4ntade 'from' f\u00f6re modulspecifierare",
        },
    },
    {
        "expected '{{'"_translatable,
        {
            u8"expected '{{'",
            u8"'{{' erwartet",
            u8"expected '{{'",
            u8"'{{' attendu",
            u8"f\u00f6rv\u00e4ntade '{{'",
        },
    },
    {
        "expected expression after 'case'"_translatable,
        {
            u8"expected expression after 'case'",
            u8"Ausdruck nach 'case' erwartet",
            u8"EXPECTED EXPRESSION AFTER 'CASE'",
            u8"instruction attendue apr\u00e8s 'case'",
            u8"f\u00f6rv\u00e4ntade ett uttryck efter 'case'",
        },
    },
    {
        "expected expression before newline"_translatable,
        {
            u8"expected expression before newline",
            u8"Ausdruck vor Zeilenumbruch erwartet",
            u8"EXPECTED EXPRESSION BEFORE NEWLINE",
            u8"instruction attendue avant le saut de ligne",
            u8"f\u00f6rv\u00e4ntade ett uttryck f\u00f6re nyrad",
        },
    },
    {
        "expected expression before semicolon"_translatable,
        {
            u8"expected expression before semicolon",
            u8"Ausdruck vor Semikolon erwartet",
            u8"EXPECTED EXPRESSION BEFORE SEMICOLON",
            u8"instruction attendue avant un point-virgule",
            u8"f\u00f6rv\u00e4ntade ett uttryck f\u00f6re semikolon",
        },
    },
    {
        "expected hexadecimal digits in Unicode escape sequence"_translatable,
        {
            u8"expected hexadecimal digits in Unicode escape sequence",
            u8"Hexadezimale Ziffern in Unicode-Escapesequenz erwartet",
            u8"EXPECTED HEXADECIMAL DIGITS IN UNICODE ESCAPE SEQUENCE",
            u8"nombres hexadecimaux attendus dans une s\u00e9quence d'\u00e9chappement Unicode",
            u8"f\u00f6rv\u00e4ntade hexadecimala siffror i Unicode flyktsekvens",
        },
    },
    {
        "expected parameter for arrow function, but got a literal instead"_translatable,
        {
            u8"expected parameter for arrow function, but got a literal instead",
            u8"Unerwartetes Literal anstelle des Parameters der Arrow-Funktion",
            u8"EXPECTED PARAMETER FOR ARROW FUNCTION, BUT GOT A LITERAL INSTEAD",
            u8"param\u00e8tre attendu au lieu d'un litt\u00e9ral pour la fonction fl\u00e9ch\u00e9e",
            u8"f\u00f6rv\u00e4ntade parameter f\u00f6r pilfunktion, men fick in litter\u00e4r ist\u00e4llet",
        },
    },
    {
        "expected parameter for arrow function, but got an expression instead"_translatable,
        {
            u8"expected parameter for arrow function, but got an expression instead",
            u8"Unerwarter Ausdruck anstelle des Parameters der Arrow-Funktion",
            u8"EXPECTED PARAMETER FOR ARROW FUNCTION, BUT GOT AN EXPRESSION INSTEAD",
            u8"param\u00e8tre attendu au lieu d'une expression pour la fonction fl\u00e9ch\u00e9e",
            u8"f\u00f6rv\u00e4ntade paramter till pilfunktion, men fick ett uttryck ist\u00e4llet",
        },
    },
    {
        "expected variable name for 'catch'"_translatable,
        {
            u8"expected variable name for 'catch'",
            u8"Variablenname f\u00fcr 'catch' fehlt",
            u8"EXPECTED VARIABLE NAME FOR 'CATCH'",
            u8"nom de variable requis pour 'catch'",
            u8"f\u00f6rv\u00e4ntade variabel namn f\u00f6r 'catch'",
        },
    },
    {
        "expected variable name for 'import'-'as'"_translatable,
        {
            u8"expected variable name for 'import'-'as'",
            u8"Variablenname f\u00fcr 'import'-'as' fehlt",
            u8"EXPECTED VARIABLE NAME FOR 'IMPORT'-'AS'",
            u8"nom de variable requis pour 'import'-'as'",
            u8"expected variable name for 'import'-'as'",
        },
    },
    {
        "expected {1:headlinese}"_translatable,
        {
            u8"expected {1:headlinese}",
            u8"{1:headlinese} erwartet",
            u8"expected {1:headlinese}",
            u8"expected {1:headlinese}",
            u8"expected {1:headlinese}",
        },
    },
    {
        "expected {1:singular}"_translatable,
        {
            u8"expected {1:singular}",
            u8"{1:singular} erwartet",
            u8"expected {1:singular}",
            u8"expected {1:singular}",
            u8"expected {1:singular}",
        },
    },
    {
        "exporting requires 'default'"_translatable,
        {
            u8"exporting requires 'default'",
            u8"Exportieren erfordert 'default'",
            u8"EXPORTING REQUIRES 'DEFAULT'",
            u8"l'exportation n\u00e9cessite 'default'",
            u8"exportering kr\u00e4ver 'default'",
        },
    },
    {
        "exporting requires '{{' and '}'"_translatable,
        {
            u8"exporting requires '{{' and '}'",
            u8"Exportieren erfordert '{{' und '}'",
            u8"EXPORTING REQUIRES '{{' AND '}'",
            u8"l'exportation n\u00e9cessite '{{' and '}'",
            u8"exportering kr\u00e4ver '{{' och '}'",
        },
    },
    {
        "extra ',' is not allowed between function call arguments"_translatable,
        {
            u8"extra ',' is not allowed between function call arguments",
            u8"Mehrere Kommata zwischen den Argumenten eines Funktionsaufruf sind ung\u00fcltig",
            u8"EXTRA ',' IS NOT ALLOWED BETWEEN FUNCTION CALL ARGUMENTS",
            u8"',' suppl\u00e9mentaire non autoris\u00e9 entre les arguments d'appel d'une fonction",
            u8"extra ',' \u00e4r inte till\u00e5tet mellan funktionkallelses argument",
        },
    },
    {
        "for loop needs an iterable, or condition and update clauses"_translatable,
        {
            u8"for loop needs an iterable, or condition and update clauses",
            u8"for-Schleife erfordert entweder ein Iterable oder eine Bedingung und Updateanweisung",
            u8"FOR LOOP NEEDS AN ITERABLE, OR CONDITION AND UPDATE CLAUSES",
            u8"une boucle for n\u00e9cessite un it\u00e9rable ou des clauses de condition et de mise \u00e0 jour",
            u8"for loop beh\u00f6ver en itererande eller vilkorligen uppdaterande moment",
        },
    },
    {
        "for-in loop expression cannot have semicolons"_translatable,
        {
            u8"for-in loop expression cannot have semicolons",
            u8"Ausdruck der for-in-Schleife darf keine Semikolon beinhalten",
            u8"FOR-IN LOOP EXPRESSION CANNOT HAVE SEMICOLONS",
            u8"une instruction de boucle for-in ne peut contenir de points-virgules",
            u8"for-in loop satser kan inte ha semikolon",
        },
    },
    {
        "for-of loop expression cannot have semicolons"_translatable,
        {
            u8"for-of loop expression cannot have semicolons",
            u8"Ausdruck der for-in-Schleife darf keine Semikolon beinhalten",
            u8"FOR-OF LOOP EXPRESSION CANNOT HAVE SEMICOLONS",
            u8"une instruction de boucle for-of ne peut contenir de points-virgules",
            u8"for-of loop satser kan inte ha semikolon",
        },
    },
    {
        "forwarding exports are only allowed in export-from"_translatable,
        {
            u8"forwarding exports are only allowed in export-from",
            u8"Forwarding-Exporte sind nur in export-from zul\u00e4ssig",
            u8"FORWARDING EXPORTS ARE ONLY ALLOWED IN EXPORT-FROM",
            u8"les exports transf\u00e9r\u00e9s ne sont autoris\u00e9s qu'avec export-from",
            u8"framf\u00f6randeexportering \u00e4r enbart till\u00e5tet i export-from",
        },
    },
    {
        "free {1} and {0} {1} {2}"_translatable,
        {
            u8"free {1} and {0} {1} {2}",
            u8"freies {1} und {0} {1} {2}",
            u8"free {1} and {0} {1} {2}",
            u8"free {1} and {0} {1} {2}",
            u8"free {1} and {0} {1} {2}",
        },
    },
    {
        "function call started here"_translatable,
        {
            u8"function call started here",
            u8"Funktionsaufruf beginnt hier",
            u8"FUNCTION CALL STARTED HERE",
            u8"appel de fonction d\u00e9but\u00e9 ici",
            u8"funktionkallelse startar h\u00e4r",
        },
    },
    {
        "function called before declaration in block scope: {0}"_translatable,
        {
            u8"function called before declaration in block scope: {0}",
            u8"Funktion '{0}' wird im Block-Scope vor ihrer Deklaration aufgerufen",
            u8"function called before declaration in block scope: {0}",
            u8"fonction appel\u00e9e avant sa d\u00e9claration dans la port\u00e9e de bloc : {0}",
            u8"function called before declaration in block scope: {0}",
        },
    },
    {
        "function declared here"_translatable,
        {
            u8"function declared here",
            u8"Funktion wird hier deklariert",
            u8"function declared here",
            u8"fonction d\u00e9clar\u00e9e ici",
            u8"funktion deklarerad h\u00e4r",
        },
    },
    {
        "function starts here"_translatable,
        {
            u8"function starts here",
            u8"Funktionsaufruf beginnt hier",
            u8"function starts here",
            u8"function starts here",
            u8"function starts here",
        },
    },
    {
        "functions/methods should not have '=>'"_translatable,
        {
            u8"functions/methods should not have '=>'",
            u8"Funktionen/Methoden sollten kein '=>' haben",
            u8"FUNCTIONS/METHODS SHOULD NOT HAVE '=>'",
            u8"les fonctions/m\u00e9thodes ne peuvent contenir '=>'",
            u8"functions/methods should not have '=>'",
        },
    },
    {
        "generator function '*' belongs after keyword function"_translatable,
        {
            u8"generator function '*' belongs after keyword function",
            u8"'*'-Zeichen f\u00fcr Generatorfunktionen wird nach dem function-Schl\u00fcsselwort erwartet",
            u8"GENERATOR FUNCTION '*' BELONGS AFTER KEYWORD FUNCTION",
            u8"une fonction g\u00e9n\u00e9ratrice '*' figure apr\u00e8s la fonction mot-cl\u00e9",
            u8"generator function '*' belongs after keyword function",
        },
    },
    {
        "generator function '*' belongs before function name"_translatable,
        {
            u8"generator function '*' belongs before function name",
            u8"'*'-Zeichen f\u00fcr Generatorfunktionen wird vor Funktionsnamen erwartet",
            u8"GENERATOR FUNCTION '*' BELONGS BEFORE FUNCTION NAME",
            u8"une fonction g\u00e9n\u00e9ratrice '*' figure avant le nom de la fonction",
            u8"generatorfunktion '*' tillh\u00f6r f\u00f6re funktionens namn",
        },
    },
    {
        "here"_translatable,
        {
            u8"here",
            u8"hier",
            u8"here",
            u8"here",
            u8"here",
        },
    },
    {
        "hex number literal has no digits"_translatable,
        {
            u8"hex number literal has no digits",
            u8"Hexadezimales Zahlenliteral ohne Ziffern",
            u8"HEX NUMBER LITERAL HAS NO DIGITS",
            u8"le litt\u00e9ral num\u00e9rique hex n'a pas de chiffres",
            u8"hex nummerlitteral har inga siffror",
        },
    },
    {
        "if statement is missing '{1}' around condition"_translatable,
        {
            u8"if statement is missing '{1}' around condition",
            u8"'{1}' fehlt um Bedingung der if-Anweisung",
            u8"IF STATEMENT IS MISSING '{1}' AROUND CONDITION",
            u8"une instruction if n\u00e9cessite '{1}' autour de la condition",
            u8"if sats saknar '{1}' runt vilkor",
        },
    },
    {
        "if statement needs parentheses around condition"_translatable,
        {
            u8"if statement needs parentheses around condition",
            u8"Klammern fehlen um Bedingung der if-Anweisung",
            u8"IF STATEMENT NEEDS PARENTHESES AROUND CONDITION",
            u8"une instruction if n\u00e9cessite des parenth\u00e8ses autour de la condition",
            u8"if sats beh\u00f6ver paranteser runt vilkor",
        },
    },
    {
        "imported variable declared here"_translatable,
        {
            u8"imported variable declared here",
            u8"Importierte Variable wurde hier deklariert",
            u8"imported variable declared here",
            u8"imported variable declared here",
            u8"imported variable declared here",
        },
    },
    {
        "incomplete export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'"_translatable,
        {
            u8"incomplete export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'",
            u8"Unvollst\u00e4ndiger Export. M\u00f6glichkeiten sind: 'export default ...' oder 'export {{name}' oder 'export * from ...' oder 'export class' oder 'export function' oder 'export let'",
            u8"INCOMPLETE EXPORT; EXPECTED 'EXPORT DEFAULT ...' OR 'EXPORT {{NAME}' OR 'EXPORT * FROM ...' OR 'EXPORT CLASS' OR 'EXPORT FUNCTION' OR 'EXPORT LET'",
            u8"export incompl ; 'export default ...' ou 'export {{name}' ou 'export * from ...' ou 'export class' ou 'export function' ou 'export let' attendu",
            u8"ofullst\u00e4ndig exportering; f\u00f6rv\u00e4ntades 'export default ...' eller 'export {{name}' eller 'export * from ...' eller 'export class' eller 'export function' eller 'export let'",
        },
    },
    {
        "indexing requires an expression"_translatable,
        {
            u8"indexing requires an expression",
            u8"Indizierung erfordert einen Ausdruck",
            u8"INDEXING REQUIRES AN EXPRESSION",
            u8"l'indexation n\u00e9cessite une expression",
            u8"indexering kr\u00e4ver ett uttryck",
        },
    },
    {
        "integer cannot be represented and will be rounded to '{1}'"_translatable,
        {
            u8"integer cannot be represented and will be rounded to '{1}'",
            u8"integer cannot be represented and will be rounded to '{1}'",
            u8"integer cannot be represented and will be rounded to '{1}'",
            u8"integer cannot be represented and will be rounded to '{1}'",
            u8"integer cannot be represented and will be rounded to '{1}'",
        },
    },
    {
        "invalid UTF-8 sequence"_translatable,
        {
            u8"invalid UTF-8 sequence",
            u8"Ung\u00fcltige UTF-8 Sequenz",
            u8"INVALID UTF-8 SEQUENCE",
            u8"s\u00e9quence UTF-8 invalide",
            u8"ogiltig UTF-8 sekvens",
        },
    },
    {
        "invalid expression left of assignment"_translatable,
        {
            u8"invalid expression left of assignment",
            u8"Ung\u00fcltiger Ausdruck linkerseits der Zuweisung",
            u8"INVALID EXPRESSION LEFT OF ASSIGNMENT",
            u8"expression invalide \u00e0 gauche de l'assignation",
            u8"ogiltigt uttryck f\u00f6re tilldelning",
        },
    },
    {
        "invalid function parameter"_translatable,
        {
            u8"invalid function parameter",
            u8"Ung\u00fcltiger Funktionsparameter",
            u8"INVALID FUNCTION PARAMETER",
            u8"param\u00e8tre de fonction invalide",
            u8"ogiltig funktions parameter",
        },
    },
    {
        "invalid hex escape sequence: {0}"_translatable,
        {
            u8"invalid hex escape sequence: {0}",
            u8"Ung\u00fcltige Hex-Escapesequenz: {0}",
            u8"INVALID HEX ESCAPE SEQUENCE: {0}",
            u8"s\u00e9quence d'\u00e9chappement hex invalide: {0}",
            u8"ogiltig kring\u00e5ende hex sekvens: {0}",
        },
    },
    {
        "invalid lone literal in object literal"_translatable,
        {
            u8"invalid lone literal in object literal",
            u8"Vereinzeltes Literal in Objekt-Literal",
            u8"INVALID LONE LITERAL IN OBJECT LITERAL",
            u8"litt\u00e9ral isol\u00e9 invalide dans un litt\u00e9ral objet",
            u8"ogiltigt l\u00e5neuttryck i objektliteral",
        },
    },
    {
        "keywords cannot contain escape sequences"_translatable,
        {
            u8"keywords cannot contain escape sequences",
            u8"Schl\u00fcsselworte d\u00fcrfen keine Escapesequenzen beinhalten",
            u8"KEYWORDS CANNOT CONTAIN ESCAPE SEQUENCES",
            u8"les mots-cl\u00e9s ne peuvent pas contenir de s\u00e9quence d'\u00e9chappement",
            u8"nyckelord kan inte inneh\u00e5lla en flyktsekvens",
        },
    },
    {
        "label named 'await' not allowed in async function"_translatable,
        {
            u8"label named 'await' not allowed in async function",
            u8"Labels namens 'await' sind innerhalb async-Functionen verboten",
            u8"LABEL NAMED 'AWAIT' NOT ALLOWED IN ASYNC FUNCTION",
            u8"\u00e9tiquette nomm\u00e9e 'await' non autoris\u00e9e dans les fonctions async",
            u8"label named 'await' not allowed in async function",
        },
    },
    {
        "legacy octal literal may not be BigInt"_translatable,
        {
            u8"legacy octal literal may not be BigInt",
            u8"Veraltete Oktalliterale sind in BigInts nicht erlaubt",
            u8"LEGACY OCTAL LITERAL MAY NOT BE BIGINT",
            u8"un litt\u00e9ral octal classique ne peut pas \u00eatre de type BigInt",
            u8"\u00e4rftligt octal nummerlitteral kan inte vara BigInt",
        },
    },
    {
        "legacy octal literals may not contain underscores"_translatable,
        {
            u8"legacy octal literals may not contain underscores",
            u8"Veraltete Oktalliterale d\u00fcrfen keine Unterstriche enthalten",
            u8"LEGACY OCTAL LITERALS MAY NOT CONTAIN UNDERSCORES",
            u8"un litt\u00e9ral octal classique ne peut pas contenir de tiret de soulignement",
            u8"\u00e4rftligt octal nummerlitteral kan inte inneh\u00e5lla understr\u00e4ck",
        },
    },
    {
        "let statement cannot declare variables named 'let'"_translatable,
        {
            u8"let statement cannot declare variables named 'let'",
            u8"let darf keine Variablen namens 'let' deklarieren",
            u8"LET STATEMENT CANNOT DECLARE VARIABLES NAMED 'LET'",
            u8"une instruction let ne peut d\u00e9clarer de variables nomm\u00e9es 'let'",
            u8"let p\u00e5st\u00e5ende kan inte deklareras med namnet 'let'",
        },
    },
    {
        "methods should not use the 'function' keyword"_translatable,
        {
            u8"methods should not use the 'function' keyword",
            u8"Methoden sollten nicht mehr das 'function'-Schl\u00fcsselwort verwenden",
            u8"METHODS SHOULD NOT USE THE 'FUNCTION' KEYWORD",
            u8"les m\u00e9thodes ne doivent pas utiliser le mot-cl\u00e9 'function'",
            u8"metoder b\u00f6r inte anv\u00e4nda nyckelordet 'function'",
        },
    },
    {
        "mismatched JSX tags; expected '</{1}>'"_translatable,
        {
            u8"mismatched JSX tags; expected '</{1}>'",
            u8"Unpassender JSX-Tag; '</{1}>' wurde anstattdessen erwartet",
            u8"mismatched JSX tags; expected '</{1}>'",
            u8"mismatched JSX tags; expected '</{1}>'",
            u8"mismatched JSX tags; expected '</{1}>'",
        },
    },
    {
        "missing ',' between variable declarations"_translatable,
        {
            u8"missing ',' between variable declarations",
            u8"Komma fehlt zwischen Variablendeklarationen",
            u8"missing ',' between variable declarations",
            u8"',' manquant entre les d\u00e9clarations de variable",
            u8"saknar ',' mellan variabel deklaration",
        },
    },
    {
        "missing '...' in JSX attribute spread"_translatable,
        {
            u8"missing '...' in JSX attribute spread",
            u8"'...' fehlt in JSX-Attribut-Spread",
            u8"missing '...' in JSX attribute spread",
            u8"missing '...' in JSX attribute spread",
            u8"missing '...' in JSX attribute spread",
        },
    },
    {
        "missing ':' in conditional expression"_translatable,
        {
            u8"missing ':' in conditional expression",
            u8"':' des tern\u00e4ren Operators fehlt",
            u8"MISSING ':' IN CONDITIONAL EXPRESSION",
            u8"':' manquant dans l'expression conditionnelle",
            u8"saknar ':' i vilkorsuttryck",
        },
    },
    {
        "missing '<>' and '</>' to enclose multiple children"_translatable,
        {
            u8"missing '<>' and '</>' to enclose multiple children",
            u8"'<>' und '</>' fehlt, um mehrere Children einzuschlie\u00dfen",
            u8"missing '<>' and '</>' to enclose multiple children",
            u8"missing '<>' and '</>' to enclose multiple children",
            u8"missing '<>' and '</>' to enclose multiple children",
        },
    },
    {
        "missing '=' after variable"_translatable,
        {
            u8"missing '=' after variable",
            u8"'=' fehlt nach Variablenname",
            u8"MISSING '=' AFTER VARIABLE",
            u8"'=' manquant apr\u00e8s une variable",
            u8"saknar '=' efter variabel",
        },
    },
    {
        "missing 'if' after 'else'"_translatable,
        {
            u8"missing 'if' after 'else'",
            u8"missing 'if' after 'else'",
            u8"missing 'if' after 'else'",
            u8"missing 'if' after 'else'",
            u8"missing 'if' after 'else'",
        },
    },
    {
        "missing 'while (condition)' for do-while statement"_translatable,
        {
            u8"missing 'while (condition)' for do-while statement",
            u8"'while (condition)' der do-while-Schleife fehlt",
            u8"MISSING 'WHILE (CONDITION)' FOR DO-WHILE STATEMENT",
            u8"'while (condition)' manquant pour une instruction for do-while",
            u8"saknar 'while (vilkor)' till do-while p\u00e5st\u00e5ende",
        },
    },
    {
        "missing arrow operator for arrow function"_translatable,
        {
            u8"missing arrow operator for arrow function",
            u8"Arrow-Operator der Arrow-Funktion fehlt",
            u8"MISSING ARROW OPERATOR FOR ARROW FUNCTION",
            u8"op\u00e9rateur de fl\u00e8che manquant pour la fonction fl\u00e9ch\u00e9e",
            u8"missing arrow operator for arrow function",
        },
    },
    {
        "missing body for 'for' loop"_translatable,
        {
            u8"missing body for 'for' loop",
            u8"K\u00f6rper der for-Schleife fehlt",
            u8"MISSING BODY FOR 'FOR' LOOP",
            u8"corps manquant pour la boucle 'for'",
            u8"saknar stycke f\u00f6r 'for' loop",
        },
    },
    {
        "missing body for 'if' statement"_translatable,
        {
            u8"missing body for 'if' statement",
            u8"K\u00f6rper der if-Anweisung fehlt",
            u8"MISSING BODY FOR 'IF' STATEMENT",
            u8"corps manquant pour l'instruction 'if'",
            u8"saknar stycke f\u00f6r 'if' p\u00e5st\u00e5ende",
        },
    },
    {
        "missing body for 'switch' statement"_translatable,
        {
            u8"missing body for 'switch' statement",
            u8"K\u00f6rper des switch-Statements fehlt",
            u8"MISSING BODY FOR 'SWITCH' STATEMENT",
            u8"corps manquant pour l'instruction 'switch'",
            u8"saknar stycke f\u00f6r 'switch' p\u00e5st\u00e5ende",
        },
    },
    {
        "missing body for catch clause"_translatable,
        {
            u8"missing body for catch clause",
            u8"K\u00f6rper des 'catch'-Blocks fehlt",
            u8"MISSING BODY FOR CATCH CLAUSE",
            u8"corps manquant pour la clause catch",
            u8"saknar stycke f\u00f6re catch sats",
        },
    },
    {
        "missing body for class"_translatable,
        {
            u8"missing body for class",
            u8"Klassenk\u00f6rper fehlt",
            u8"MISSING BODY FOR CLASS",
            u8"corps manquant pour la classe",
            u8"saknar stycke f\u00f6r klass",
        },
    },
    {
        "missing body for do-while loop"_translatable,
        {
            u8"missing body for do-while loop",
            u8"K\u00f6rper der do-while-Schleife fehlt",
            u8"MISSING BODY FOR DO-WHILE LOOP",
            u8"corps manquant pour la boucle do-while",
            u8"saknar stycke f\u00f6r do-while loop",
        },
    },
    {
        "missing body for finally clause"_translatable,
        {
            u8"missing body for finally clause",
            u8"K\u00f6rper des 'finally'-Blocks fehlt",
            u8"MISSING BODY FOR FINALLY CLAUSE",
            u8"corps manquant pour la clause finally",
            u8"saknar stycke f\u00f6r finally sats",
        },
    },
    {
        "missing body for function"_translatable,
        {
            u8"missing body for function",
            u8"K\u00f6rper der Funktion fehlt",
            u8"MISSING BODY FOR FUNCTION",
            u8"corps manquant pour la fonction",
            u8"missing body for function",
        },
    },
    {
        "missing body for try statement"_translatable,
        {
            u8"missing body for try statement",
            u8"K\u00f6rper des try-Statements fehlt",
            u8"MISSING BODY FOR TRY STATEMENT",
            u8"corps manquant pour l'instruction try",
            u8"saknar stycke f\u00f6r 'try' p\u00e5st\u00e5ende",
        },
    },
    {
        "missing body for while loop"_translatable,
        {
            u8"missing body for while loop",
            u8"K\u00f6rper der while-Schleife fehlt",
            u8"MISSING BODY FOR WHILE LOOP",
            u8"corps manquant pour la boucle while",
            u8"saknar stycke f\u00f6r while loop",
        },
    },
    {
        "missing body for {1:headlinese}"_translatable,
        {
            u8"missing body for {1:headlinese}",
            u8"K\u00f6rper f\u00fcr {1:headlinese} fehlt",
            u8"MISSING BODY FOR {1:headlinese}",
            u8"corps manquant pour {1:headlinese}",
            u8"saknas stycke f\u00f6r {1:headlinese}",
        },
    },
    {
        "missing catch or finally clause for try statement"_translatable,
        {
            u8"missing catch or finally clause for try statement",
            u8"catch- oder finally-Klausel des try-Statements fehlt",
            u8"MISSING CATCH OR FINALLY CLAUSE FOR TRY STATEMENT",
            u8"clause catch ou finally manquante pour l'instruction try",
            u8"saknar catch eller finally sats f\u00f6r try sats",
        },
    },
    {
        "missing catch variable name between parentheses"_translatable,
        {
            u8"missing catch variable name between parentheses",
            u8"catch-Variablenname fehlt zwischen Klammern",
            u8"MISSING CATCH VARIABLE NAME BETWEEN PARENTHESES",
            u8"nom de variable de capture manquante entre les parenth\u00e8ses",
            u8"saknar catch variabelnamn mellan paranteser",
        },
    },
    {
        "missing comma between object literal entries"_translatable,
        {
            u8"missing comma between object literal entries",
            u8"Komma fehlt zwischen Feldern des Objekt-Literals",
            u8"MISSING COMMA BETWEEN OBJECT LITERAL ENTRIES",
            u8"virgule manquante entre les entr\u00e9es du litt\u00e9ral objet",
            u8"saknar comma mellan f\u00f6rekommande objektliteral",
        },
    },
    {
        "missing comparison; '{1}' does not extend to the right side of '{0}'"_translatable,
        {
            u8"missing comparison; '{1}' does not extend to the right side of '{0}'",
            u8"Vergleich fehlt. '{1}' wird nicht auf der rechten Seite von '{0}' angewandt",
            u8"missing comparison; '{1}' does not extend to the right side of '{0}'",
            u8"missing comparison; '{1}' does not extend to the right side of '{0}'",
            u8"missing comparison; '{1}' does not extend to the right side of '{0}'",
        },
    },
    {
        "missing condition for if statement"_translatable,
        {
            u8"missing condition for if statement",
            u8"Bedingung der if-Anweisung fehlt",
            u8"MISSING CONDITION FOR IF STATEMENT",
            u8"condition manquante pour l'instruction if",
            u8"saknar vilkor i if p\u00e5st\u00e5ende",
        },
    },
    {
        "missing condition for switch statement"_translatable,
        {
            u8"missing condition for switch statement",
            u8"Bedingung des switch-Statements fehlt",
            u8"MISSING CONDITION FOR SWITCH STATEMENT",
            u8"condition manquante pour l'instruction switch",
            u8"saknar vilkor f\u00f6r switch p\u00e5st\u00e5ende",
        },
    },
    {
        "missing condition for while statement"_translatable,
        {
            u8"missing condition for while statement",
            u8"Bedingung der while-Schleife fehlt",
            u8"MISSING CONDITION FOR WHILE STATEMENT",
            u8"condition manquante pour l'instruction while",
            u8"saknar vilkor f\u00f6r while p\u00e5st\u00e5ende",
        },
    },
    {
        "missing end of array; expected ']'"_translatable,
        {
            u8"missing end of array; expected ']'",
            u8"Ende des Arrays fehlt. ']' erwartet",
            u8"MISSING END OF ARRAY; EXPECTED ']'",
            u8"fin de tableau manquante ; ']' attendu",
            u8"saknar slut av lista; f\u00f6rv\u00e4ntades ']'",
        },
    },
    {
        "missing expression between parentheses"_translatable,
        {
            u8"missing expression between parentheses",
            u8"Ausdruck fehlt zwischen Klammern",
            u8"MISSING EXPRESSION BETWEEN PARENTHESES",
            u8"expression manquante entre parenth\u00e8ses",
            u8"saknar uttryck mellan paranteser",
        },
    },
    {
        "missing for loop header"_translatable,
        {
            u8"missing for loop header",
            u8"Kopf der Schleife fehlt",
            u8"MISSING FOR LOOP HEADER",
            u8"en-t\u00eate de boucle for manquante",
            u8"saknar for loop rubrik",
        },
    },
    {
        "missing function parameter list"_translatable,
        {
            u8"missing function parameter list",
            u8"Parameterliste der Funktion fehlt",
            u8"MISSING FUNCTION PARAMETER LIST",
            u8"liste de param\u00e8tres de fonction manquante",
            u8"saknar funktions parameter lista",
        },
    },
    {
        "missing header and body for 'for' loop"_translatable,
        {
            u8"missing header and body for 'for' loop",
            u8"Kopf und K\u00f6rper der for-Schleife fehlen",
            u8"MISSING HEADER AND BODY FOR 'FOR' LOOP",
            u8"en-t\u00eate et corps manquants pour la boucle 'for'",
            u8"saknar rubrik och stycke f\u00f6r 'for' loop",
        },
    },
    {
        "missing initializer in const declaration"_translatable,
        {
            u8"missing initializer in const declaration",
            u8"Initialisierung der const-Deklaration fehlt",
            u8"MISSING INITIALIZER IN CONST DECLARATION",
            u8"initialisateur manquant dans la d\u00e9claration const",
            u8"missing initializer in const declaration",
        },
    },
    {
        "missing name in function statement"_translatable,
        {
            u8"missing name in function statement",
            u8"Name fehlt innerhalb function-Statement",
            u8"MISSING NAME IN FUNCTION STATEMENT",
            u8"nom manquant pour l'instruction de fonction",
            u8"saknar namn f\u00f6r funktions p\u00e5st\u00e5ende",
        },
    },
    {
        "missing name of class"_translatable,
        {
            u8"missing name of class",
            u8"Klassenname fehlt",
            u8"MISSING NAME OF CLASS",
            u8"nom de classe manquant",
            u8"saknar namn f\u00f6r klass",
        },
    },
    {
        "missing name of exported class"_translatable,
        {
            u8"missing name of exported class",
            u8"Name der exportierten Klasse fehlt",
            u8"MISSING NAME OF EXPORTED CLASS",
            u8"nom de classe export\u00e9e manquant",
            u8"saknar namn f\u00f6r exporterad klass",
        },
    },
    {
        "missing name of exported function"_translatable,
        {
            u8"missing name of exported function",
            u8"Name der exportierten Funktion fehlt",
            u8"MISSING NAME OF EXPORTED FUNCTION",
            u8"nom de fonction export\u00e9e manquant",
            u8"saknar namn f\u00f6r exporterad funktion",
        },
    },
    {
        "missing name or parentheses for function"_translatable,
        {
            u8"missing name or parentheses for function",
            u8"Name oder Klammern fehlen f\u00fcr die Funktion",
            u8"MISSING NAME OR PARENTHESES FOR FUNCTION",
            u8"nom ou parenth\u00e8ses manquants pour la fonction",
            u8"saknar namn eller paranteser f\u00f6r funktion",
        },
    },
    {
        "missing operand for operator"_translatable,
        {
            u8"missing operand for operator",
            u8"Operand des Operators fehlt",
            u8"MISSING OPERAND FOR OPERATOR",
            u8"op\u00e9rande manquante pour l'op\u00e9rateur",
            u8"saknar operand f\u00f6r operat\u00f6r",
        },
    },
    {
        "missing operator between expression and arrow function"_translatable,
        {
            u8"missing operator between expression and arrow function",
            u8"Operator fehlt zwischen Ausdruck und Arrow-Funktion",
            u8"MISSING OPERATOR BETWEEN EXPRESSION AND ARROW FUNCTION",
            u8"op\u00e9rateur manquant entre l'expression et la fonction fl\u00e9ch\u00e9e",
            u8"saknar operat\u00f6r mellan uttryck och pilfunktion",
        },
    },
    {
        "missing parameters for arrow function"_translatable,
        {
            u8"missing parameters for arrow function",
            u8"Parameter fehlen f\u00fcr Arrow-Funktion",
            u8"MISSING PARAMETERS FOR ARROW FUNCTION",
            u8"param\u00e8tres manquants pour la fonction fl\u00e9ch\u00e9e",
            u8"saknar parametrar f\u00f6re pilfunktion",
        },
    },
    {
        "missing parentheses around left-hand side of '**'"_translatable,
        {
            u8"missing parentheses around left-hand side of '**'",
            u8"Klammern um linke Seite von '**' fehlen",
            u8"missing parentheses around left-hand side of '**'",
            u8"missing parentheses around left-hand side of '**'",
            u8"missing parentheses around left-hand side of '**'",
        },
    },
    {
        "missing parentheses around operand of '{0}'"_translatable,
        {
            u8"missing parentheses around operand of '{0}'",
            u8"Klammern fehlen um den Operanden von '{0}'",
            u8"missing parentheses around operand of '{0}'",
            u8"missing parentheses around operand of '{0}'",
            u8"missing parentheses around operand of '{0}'",
        },
    },
    {
        "missing parentheses around self-invoked function"_translatable,
        {
            u8"missing parentheses around self-invoked function",
            u8"Klammern um selbstaufgerufene Funktion fehlen",
            u8"missing parentheses around self-invoked function",
            u8"missing parentheses around self-invoked function",
            u8"missing parentheses around self-invoked function",
        },
    },
    {
        "missing property name after '.' operator"_translatable,
        {
            u8"missing property name after '.' operator",
            u8"Eigenschaftsname fehlt nach dem '.'-Operator",
            u8"MISSING PROPERTY NAME AFTER '.' OPERATOR",
            u8"nom de propri\u00e9t\u00e9 manquant apr\u00e8s l'op\u00e9rateur '.'",
            u8"saknar egenskaps namn efter '.' operat\u00f6r",
        },
    },
    {
        "missing property name between '.' and '.'"_translatable,
        {
            u8"missing property name between '.' and '.'",
            u8"Eigenschaftsname fehlt zwischen '.' und '.'",
            u8"missing property name between '.' and '.'",
            u8"nom de propri\u00e9t\u00e9 manquante entre '.' et '.'",
            u8"missing property name between '.' and '.'",
        },
    },
    {
        "missing semicolon after statement"_translatable,
        {
            u8"missing semicolon after statement",
            u8"Semikolon fehlt nach Anweisung",
            u8"MISSING SEMICOLON AFTER STATEMENT",
            u8"point-virgule manquant apr\u00e8s l'instruction",
            u8"saknar semikolon efter p\u00e5st\u00e5ende",
        },
    },
    {
        "missing semicolon between condition and update parts of for loop"_translatable,
        {
            u8"missing semicolon between condition and update parts of for loop",
            u8"Semikolon fehlt zwischen Bedingung und Update-Anweisung der for-Schleife",
            u8"MISSING SEMICOLON BETWEEN CONDITION AND UPDATE PARTS OF FOR LOOP",
            u8"point-virgule manquant entre la condition et l'actualisation de la boucle for",
            u8"saknar semikolon mellan vilkor och updaterings delen i for loopen",
        },
    },
    {
        "missing semicolon between init and condition parts of for loop"_translatable,
        {
            u8"missing semicolon between init and condition parts of for loop",
            u8"Semikolon fehlt zwischen Initialisierung und Bedingung der for-Schleife",
            u8"MISSING SEMICOLON BETWEEN INIT AND CONDITION PARTS OF FOR LOOP",
            u8"point-virgule manquant entre l'initialisation et la condition de la boucle for",
            u8"saknar semikolon mellan start och vilkors delen i for loopen",
        },
    },
    {
        "missing value for object property"_translatable,
        {
            u8"missing value for object property",
            u8"Wert der Objekteigenschaft fehlt",
            u8"MISSING VALUE FOR OBJECT PROPERTY",
            u8"valeur manquante pour la propri\u00e9t\u00e9 d'objet",
            u8"saknar v\u00e4rde f\u00f6r objektegenskap",
        },
    },
    {
        "missing variable name"_translatable,
        {
            u8"missing variable name",
            u8"Variablenname fehlt",
            u8"MISSING VARIABLE NAME",
            u8"nom de variable manquant",
            u8"saknar variabel namn",
        },
    },
    {
        "misspelled React attribute; write '{1}' instead"_translatable,
        {
            u8"misspelled React attribute; write '{1}' instead",
            u8"React-Attribut ist falsch geschrieben; '{1}' anstattdessen schreiben",
            u8"misspelled React attribute; write '{1}' instead",
            u8"misspelled React attribute; write '{1}' instead",
            u8"misspelled React attribute; write '{1}' instead",
        },
    },
    {
        "new variable shadows existing variable"_translatable,
        {
            u8"new variable shadows existing variable",
            u8"Neue Variable macht bereits existierende unsichtbar",
            u8"new variable shadows existing variable",
            u8"new variable shadows existing variable",
            u8"new variable shadows existing variable",
        },
    },
    {
        "newline is not allowed between 'async' and arrow function parameter list"_translatable,
        {
            u8"newline is not allowed between 'async' and arrow function parameter list",
            u8"Zeilenumbruch ist zwischen 'async' und Arrow-Funktion verboten",
            u8"NEWLINE IS NOT ALLOWED BETWEEN 'ASYNC' AND ARROW FUNCTION PARAMETER LIST",
            u8"un saut de ligne n'est pas autoris\u00e9 entre 'async' et la liste de param\u00e8tres d'une fonction fl\u00e9ch\u00e9e",
            u8"nyrad \u00e4r inte till\u00e5ten mellan 'async' och pilfunktions parameter lista",
        },
    },
    {
        "number literal contains consecutive underscores"_translatable,
        {
            u8"number literal contains consecutive underscores",
            u8"Zahlenliteral darf keine aufeinanderfolgenden Unterstriche enthalten",
            u8"NUMBER LITERAL CONTAINS CONSECUTIVE UNDERSCORES",
            u8"le litt\u00e9ral num\u00e9rique contient plusieurs tirets de soulignement cons\u00e9cutifs",
            u8"numerlitter\u00e4r inneh\u00e5ller upprepande understr\u00e4ck",
        },
    },
    {
        "number literal contains trailing underscore(s)"_translatable,
        {
            u8"number literal contains trailing underscore(s)",
            u8"Zahlenliteral endet mit Unterstrich(en)",
            u8"NUMBER LITERAL CONTAINS TRAILING UNDERSCORE(S)",
            u8"le litt\u00e9ral num\u00e9rique est suivi d'un tiret de soulignement",
            u8"nummerlitter\u00e4r inneh\u00e5ller efterf\u00f6ljande understr\u00e4ck",
        },
    },
    {
        "object literal started here"_translatable,
        {
            u8"object literal started here",
            u8"Objektliteral beginnt hier",
            u8"OBJECT LITERAL STARTED HERE",
            u8"litt\u00e9ral objet d\u00e9but\u00e9 ici",
            u8"objektlitteral startades h\u00e4r",
        },
    },
    {
        "octal literal may not have decimal"_translatable,
        {
            u8"octal literal may not have decimal",
            u8"Oktalliterale mit Dezimalpunkt sind nicht erlaubt",
            u8"OCTAL LITERAL MAY NOT HAVE DECIMAL",
            u8"un litt\u00e9ral octal ne peut avoir de partie d\u00e9cimale",
            u8"oktal nummerlitter\u00e4l kan inte ha decimaler",
        },
    },
    {
        "octal literal may not have exponent"_translatable,
        {
            u8"octal literal may not have exponent",
            u8"Oktalliterale mit Exponenten sind nicht erlaubt",
            u8"OCTAL LITERAL MAY NOT HAVE EXPONENT",
            u8"un litt\u00e9ral octal ne peut avoir d'exposant",
            u8"oktal nummerlitter\u00e4l kan inte ha exponent",
        },
    },
    {
        "octal number literal has no digits"_translatable,
        {
            u8"octal number literal has no digits",
            u8"Oktales Zahlenliteral ohne Ziffern",
            u8"OCTAL NUMBER LITERAL HAS NO DIGITS",
            u8"le litt\u00e9ral num\u00e9rique octal n'a pas de chiffres",
            u8"oktal nummerlitteral har inga siffror",
        },
    },
    {
        "opening '<{1}>' tag here"_translatable,
        {
            u8"opening '<{1}>' tag here",
            u8"\u00d6ffnender '<{1}>' Tag ist hier",
            u8"opening '<{1}>' tag here",
            u8"opening '<{1}>' tag here",
            u8"opening '<{1}>' tag here",
        },
    },
    {
        "private properties are not allowed in object literals"_translatable,
        {
            u8"private properties are not allowed in object literals",
            u8"Innerhalb von Objektliteralen sind private Eigenschaften verboten",
            u8"PRIVATE PROPERTIES ARE NOT ALLOWED IN OBJECT LITERALS",
            u8"les propri\u00e9t\u00e9s priv\u00e9es ne sont pas autoris\u00e9es dans les litt\u00e9raux objet",
            u8"privata egenskaper \u00e4r inte till\u00e5tna i objektlitter\u00e4ler",
        },
    },
    {
        "redeclaration of global variable"_translatable,
        {
            u8"redeclaration of global variable",
            u8"Globale Variable wird erneut deklariert",
            u8"REDECLARATION OF GLOBAL VARIABLE",
            u8"red\u00e9claration de variable globale",
            u8"omdeklaration av global variabel",
        },
    },
    {
        "redeclaration of variable: {0}"_translatable,
        {
            u8"redeclaration of variable: {0}",
            u8"Variable '{0}' wird erneut deklariert",
            u8"REDECLARATION OF VARIABLE: {0}",
            u8"red\u00e9claration de variable: {0}",
            u8"omdekleration av variabel: {0}",
        },
    },
    {
        "redundant delete statement on variable"_translatable,
        {
            u8"redundant delete statement on variable",
            u8"Unn\u00f6tiges delete-Statement f\u00fcr Variable",
            u8"REDUNDANT DELETE STATEMENT ON VARIABLE",
            u8"instruction delete redondante pour la variable",
            u8"redundant delete p\u00e5st\u00e5ende f\u00f6r variabel",
        },
    },
    {
        "remove '{0}' to update an existing variable"_translatable,
        {
            u8"remove '{0}' to update an existing variable",
            u8"Entferne '{0}' um die vorhandene Variable zu updaten",
            u8"REMOVE '{0}' TO UPDATE AN EXISTING VARIABLE",
            u8"supprimer '{0}' pour actualiser une variable existante",
            u8"ta bort '{0}' f\u00f6r att uppdatera en existerande variabel",
        },
    },
    {
        "return statement returns nothing (undefined)"_translatable,
        {
            u8"return statement returns nothing (undefined)",
            u8"Return-Statement gibt nichts (undefined) zur\u00fcck",
            u8"return statement returns nothing (undefined)",
            u8"l'instruction de retour ne retourne rien (undefined)",
            u8"return statement returns nothing (undefined)",
        },
    },
    {
        "see here"_translatable,
        {
            u8"see here",
            u8"siehe hier",
            u8"see here",
            u8"see here",
            u8"see here",
        },
    },
    {
        "something happened"_translatable,
        {
            u8"something happened",
            u8"etwas geschah",
            u8"something happened",
            u8"something happened",
            u8"something happened",
        },
    },
    {
        "stray comma in function parameter"_translatable,
        {
            u8"stray comma in function parameter",
            u8"Vereinzeltes Komme in Funktionsparameter",
            u8"stray comma in function parameter",
            u8"virgule isol\u00e9e dans un param\u00e8tre de fonction",
            u8"stray comma in function parameter",
        },
    },
    {
        "stray comma in let statement"_translatable,
        {
            u8"stray comma in let statement",
            u8"Vereinzeltes Komma in let-Statement",
            u8"STRAY COMMA IN LET STATEMENT",
            u8"virgule isol\u00e9e dans une instruction let",
            u8"vilset komma i let p\u00e5st\u00e5ende",
        },
    },
    {
        "switch statement is missing '{1}' around condition"_translatable,
        {
            u8"switch statement is missing '{1}' around condition",
            u8"'{1}' fehlt um Bedingung der switch-Anweisung",
            u8"SWITCH STATEMENT IS MISSING '{1}' AROUND CONDITION",
            u8"une instruction switch n\u00e9cessite '{1}' autour de la condition",
            u8"switch sats saknas '{1}' runt vilkoren",
        },
    },
    {
        "switch statement needs parentheses around condition"_translatable,
        {
            u8"switch statement needs parentheses around condition",
            u8"Klammern fehlen um Bedingung der switch-Anweisung",
            u8"SWITCH STATEMENT NEEDS PARENTHESES AROUND CONDITION",
            u8"une instruction switch n\u00e9cessite des parenth\u00e8ses autour de la condition",
            u8"switch sats beh\u00f6ver paranteser runt vilkor",
        },
    },
    {
        "this {0} looks fishy"_translatable,
        {
            u8"this {0} looks fishy",
            u8"dieses {0} sieht merkw\u00fcrdig aus",
            u8"this {0} looks fishy",
            u8"this {0} looks fishy",
            u8"this {0} looks fishy",
        },
    },
    {
        "this {1} looks fishy"_translatable,
        {
            u8"this {1} looks fishy",
            u8"dieses {1} sieht merkw\u00fcrdig aus",
            u8"this {1} looks fishy",
            u8"this {1} looks fishy",
            u8"this {1} looks fishy",
        },
    },
    {
        "try statement starts here"_translatable,
        {
            u8"try statement starts here",
            u8"try-Statement beginnt hier",
            u8"TRY STATEMENT STARTS HERE",
            u8"l'instruction try d\u00e9bute ici",
            u8"try sats startar h\u00e4r",
        },
    },
    {
        "unclosed block comment"_translatable,
        {
            u8"unclosed block comment",
            u8"Blockkommentar ohne Ende",
            u8"UNCLOSED BLOCK COMMENT",
            u8"commentaire de bloc non ferm\u00e9",
            u8"oavslutad kommentationsstycke",
        },
    },
    {
        "unclosed class; expected '}' by end of file"_translatable,
        {
            u8"unclosed class; expected '}' by end of file",
            u8"Klasse ohne Ende. '}' bis sp\u00e4testens zum Ende der Datei erwartet",
            u8"unclosed class; expected '}' by end of file",
            u8"unclosed class; expected '}' by end of file",
            u8"unclosed class; expected '}' by end of file",
        },
    },
    {
        "unclosed code block; expected '}' by end of file"_translatable,
        {
            u8"unclosed code block; expected '}' by end of file",
            u8"Code-Block ohne Ende. '}' bis sp\u00e4testens zum Ende der Datei erwartet",
            u8"UNCLOSED CODE BLOCK; EXPECTED '}' BY END OF FILE",
            u8"bloc de code non ferm\u00e9 ; '}' attendu avant la fin du fichier",
            u8"oavslutad kod stycke; f\u00f6rv\u00e4ntade '}' innan slutet av filen",
        },
    },
    {
        "unclosed identifier escape sequence"_translatable,
        {
            u8"unclosed identifier escape sequence",
            u8"Unbeendete Bezeichner-Escapesequenz",
            u8"UNCLOSED IDENTIFIER ESCAPE SEQUENCE",
            u8"s\u00e9quence d'\u00e9chappement d'identifiant non ferm\u00e9e",
            u8"oavslutad identifierare flyktsekvens",
        },
    },
    {
        "unclosed object literal; expected '}'"_translatable,
        {
            u8"unclosed object literal; expected '}'",
            u8"Unbeendetes Objekt-Literal. '}' erwartet",
            u8"UNCLOSED OBJECT LITERAL; EXPECTED '}'",
            u8"litt\u00e9ral objet non ferm\u00e9 ; '}' attendu",
            u8"oavslutad objektlitter\u00e4r; f\u00f6rv\u00e4ntade '}'",
        },
    },
    {
        "unclosed regexp literal"_translatable,
        {
            u8"unclosed regexp literal",
            u8"Unbeendetes RegExp-Literal",
            u8"UNCLOSED REGEXP LITERAL",
            u8"litt\u00e9ral regexp non ferm\u00e9",
            u8"oavslutad regexplitteral",
        },
    },
    {
        "unclosed string literal"_translatable,
        {
            u8"unclosed string literal",
            u8"Zeichenkette ohne Ende",
            u8"UNCLOSED STRING LITERAL",
            u8"litt\u00e9ral string non ferm\u00e9",
            u8"oavslutad str\u00e4nglitteral",
        },
    },
    {
        "unclosed template"_translatable,
        {
            u8"unclosed template",
            u8"Template ohne Ende",
            u8"UNCLOSED TEMPLATE",
            u8"template non ferm\u00e9",
            u8"oavslutad mall",
        },
    },
    {
        "unexpected '#'"_translatable,
        {
            u8"unexpected '#'",
            u8"Unerwartete '#'",
            u8"UNEXPECTED '#'",
            u8"'#' inattendu",
            u8"of\u00f6rv\u00e4ntad '#'",
        },
    },
    {
        "unexpected '@'"_translatable,
        {
            u8"unexpected '@'",
            u8"Unerwartetes '@'",
            u8"UNEXPECTED '@'",
            u8"'@' inattendu",
            u8"of\u00f6rv\u00e4ntad '@'",
        },
    },
    {
        "unexpected '\\' in identifier"_translatable,
        {
            u8"unexpected '\\' in identifier",
            u8"Unerwartetes '\\' in Bezeichner",
            u8"UNEXPECTED '\\' IN IDENTIFIER",
            u8"'\\' inattendu dans un identifiant",
            u8"of\u00f6rv\u00e4ntad '\\' i identifierare",
        },
    },
    {
        "unexpected 'case' outside switch statement"_translatable,
        {
            u8"unexpected 'case' outside switch statement",
            u8"Unerwartetes 'case' au\u00dferhalb des switch-Statements",
            u8"UNEXPECTED 'CASE' OUTSIDE SWITCH STATEMENT",
            u8"'case' inattendu en dehors d'une instruction switch",
            u8"of\u00f6rv\u00e4ntad 'case' utanf\u00f6r switch sats",
        },
    },
    {
        "unexpected 'catch' without 'try'"_translatable,
        {
            u8"unexpected 'catch' without 'try'",
            u8"Unerwartetes 'catch' ohne zugeh\u00f6riges 'try'",
            u8"UNEXPECTED 'CATCH' WITHOUT 'TRY'",
            u8"instruction 'catch' inattendue en l'absence de 'try'",
            u8"of\u00f6rv\u00e4ntad 'catch' utan n\u00e5gon 'try'",
        },
    },
    {
        "unexpected 'default' outside switch statement"_translatable,
        {
            u8"unexpected 'default' outside switch statement",
            u8"Unerwartetes 'default' au\u00dferhalb des switch-Statements",
            u8"UNEXPECTED 'DEFAULT' OUTSIDE SWITCH STATEMENT",
            u8"'default' inattendu en dehors d'une instruction switch",
            u8"of\u00f6rv\u00e4ntad 'default' utanf\u00f6r switch sats",
        },
    },
    {
        "unexpected 'finally' without 'try'"_translatable,
        {
            u8"unexpected 'finally' without 'try'",
            u8"'finally' ohne zugeh\u00f6riges 'try'",
            u8"UNEXPECTED 'FINALLY' WITHOUT 'TRY'",
            u8"'finally' inappropri\u00e9 sans 'try'",
            u8"of\u00f6rv\u00e4ntad 'finally' utan 'try'",
        },
    },
    {
        "unexpected '{0}'"_translatable,
        {
            u8"unexpected '{0}'",
            u8"Unerwartetes '{0}'",
            u8"unexpected '{0}'",
            u8"'{0}' inattendu",
            u8"of\u00f6rv\u00e4ntad '{0}'",
        },
    },
    {
        "unexpected characters in binary literal"_translatable,
        {
            u8"unexpected characters in binary literal",
            u8"Unerwartete Zeichen in bin\u00e4rem Zahlenliteral",
            u8"UNEXPECTED CHARACTERS IN BINARY LITERAL",
            u8"caract\u00e8res inattendus dans un litt\u00e9ral binaire",
            u8"of\u00f6rv\u00e4ntat tecken i bin\u00e4rlitteral",
        },
    },
    {
        "unexpected characters in hex literal"_translatable,
        {
            u8"unexpected characters in hex literal",
            u8"Unerwartete Zeichen in hexadezimalem Zahlenliteral",
            u8"UNEXPECTED CHARACTERS IN HEX LITERAL",
            u8"caract\u00e8res inattendus dans un litt\u00e9ral hex",
            u8"of\u00f6rv\u00e4ntat tecken i hexlitteral",
        },
    },
    {
        "unexpected characters in number literal"_translatable,
        {
            u8"unexpected characters in number literal",
            u8"Unerwartete Zeichen in Zahlenliteral",
            u8"UNEXPECTED CHARACTERS IN NUMBER LITERAL",
            u8"caract\u00e8res inattendus dans un litt\u00e9ral num\u00e9rique",
            u8"of\u00f6rv\u00e4ntat tecken i nummerlitteral",
        },
    },
    {
        "unexpected characters in octal literal"_translatable,
        {
            u8"unexpected characters in octal literal",
            u8"Unerwartete Zeichen in oktalem Zahlenliteral",
            u8"UNEXPECTED CHARACTERS IN OCTAL LITERAL",
            u8"caract\u00e8res inattendus dans un litt\u00e9ral octal",
            u8"of\u00f6rv\u00e4ntat tecken i oktallitteral",
        },
    },
    {
        "unexpected control character"_translatable,
        {
            u8"unexpected control character",
            u8"Unerwartetes Steuerzeichen",
            u8"UNEXPECTED CONTROL CHARACTER",
            u8"caract\u00e8re de contr\u00f4le inattendu",
            u8"of\u00f6rv\u00e4ntat kontrolltecken",
        },
    },
    {
        "unexpected expression; missing key for object entry"_translatable,
        {
            u8"unexpected expression; missing key for object entry",
            u8"Ausdruck vor Zeilenumbruch erwartet",
            u8"UNEXPECTED EXPRESSION; MISSING KEY FOR OBJECT ENTRY",
            u8"expression inattendue ; cl\u00e9 manquante pour l'entr\u00e9e d'objet",
            u8"of\u00f6rv\u00e4ntat uttryck; saknar nyckel f\u00f6r tilltr\u00e4dande objekt",
        },
    },
    {
        "unexpected identifier in expression; missing operator before"_translatable,
        {
            u8"unexpected identifier in expression; missing operator before",
            u8"Unerwarteter Bezeichner in Ausdruck. Operator fehlt davor.",
            u8"UNEXPECTED IDENTIFIER IN EXPRESSION; MISSING OPERATOR BEFORE",
            u8"identifiant inattendu dans une expression ; op\u00e9rateur initial manquant",
            u8"of\u00f6rv\u00e4ntad identifierare i uttryck; saknar operat\u00f6r f\u00f6re",
        },
    },
    {
        "unexpected literal in parameter list; expected parameter name"_translatable,
        {
            u8"unexpected literal in parameter list; expected parameter name",
            u8"Unerwartetes Literal in Parameterliste. Parametername erwartet",
            u8"UNEXPECTED LITERAL IN PARAMETER LIST; EXPECTED PARAMETER NAME",
            u8"litt\u00e9ral inattendu dans une liste de param\u00e8tres ; nom de param\u00e8tre attendu",
            u8"of\u00f6rv\u00e4ntad litteral i parameter lista; f\u00f6rv\u00e4ntade parameter namn",
        },
    },
    {
        "unexpected statement before first switch case, expected 'case' or 'default'"_translatable,
        {
            u8"unexpected statement before first switch case, expected 'case' or 'default'",
            u8"Unerwartetes Statment vor dem ersten Switch-Case. Entweder 'case' oder 'default' erwartet.",
            u8"unexpected statement before first switch case, expected 'case' or 'default'",
            u8"unexpected statement before first switch case, expected 'case' or 'default'",
            u8"unexpected statement before first switch case, expected 'case' or 'default'",
        },
    },
    {
        "unexpected token"_translatable,
        {
            u8"unexpected token",
            u8"Unerwartetes Token",
            u8"unexpected token",
            u8"symbole manquant",
            u8"of\u00f6rv\u00e4ntad token",
        },
    },
    {
        "unexpected token in export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'"_translatable,
        {
            u8"unexpected token in export; expected 'export default ...' or 'export {{name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'",
            u8"Unerwartetes Token in Export. M\u00f6glichkeiten sind: 'export default ...' oder 'export {{name}' oder 'export * from ...' oder 'export class' oder 'export function' oder 'export let'",
            u8"UNEXPECTED TOKEN IN EXPORT; EXPECTED 'EXPORT DEFAULT ...' OR 'EXPORT {{NAME}' OR 'EXPORT * FROM ...' OR 'EXPORT CLASS' OR 'EXPORT FUNCTION' OR 'EXPORT LET'",
            u8"symbole inattendu dans export ; 'export default ...' ou 'export {{name}' ou 'export * from ...' ou 'export class' ou 'export function' ou 'export let' attendu",
            u8"of\u00f6rv\u00e4ntad token i export; f\u00f6rv\u00e4ntade 'export default ...' eller 'export {{name}' eller 'export * from ...' eller 'export class' eller 'export function'  'export let'",
        },
    },
    {
        "unexpected token in variable declaration; expected variable name"_translatable,
        {
            u8"unexpected token in variable declaration; expected variable name",
            u8"Unerwartetes Token innerhalb der Variablendeklaration. Variablenname anstattdessen erwartet",
            u8"UNEXPECTED TOKEN IN VARIABLE DECLARATION; EXPECTED VARIABLE NAME",
            u8"symbole inattendu dans une d\u00e9claration de variable ; nom de variable attendu",
            u8"of\u00f6rv\u00e4ntad token i variabel deklaration; f\u00f6rv\u00e4ntade variabel namn",
        },
    },
    {
        "unicode byte order mark (BOM) cannot appear before #! at beginning of script"_translatable,
        {
            u8"unicode byte order mark (BOM) cannot appear before #! at beginning of script",
            u8"Die Unicode Bytereihenfolge-Markierung (BOM) darf nicht vor #! zu Beginn eines Skripts erscheinen",
            u8"UNICODE BYTE ORDER MARK (BOM) CANNOT APPEAR BEFORE #! AT BEGINNING OF SCRIPT",
            u8"un indicateur d'ordre des octets (BOM) ne peut figurer avant #! au d\u00e9but d'un script",
            u8"unicode byte ordningsm\u00e4rke (BOM) kan inte f\u00f6rekomma f\u00f6re #! i b\u00f6rjan av skript",
        },
    },
    {
        "unmatched '}'"_translatable,
        {
            u8"unmatched '}'",
            u8"Zugeh\u00f6rige geschweifte Klammer fehlt",
            u8"UNMATCHED '}'",
            u8"'}' non appari\u00e9",
            u8"omatchad '}'",
        },
    },
    {
        "unmatched indexing bracket"_translatable,
        {
            u8"unmatched indexing bracket",
            u8"Zugeh\u00f6rige Indizierungsklammer fehlt",
            u8"UNMATCHED INDEXING BRACKET",
            u8"crochet d'indexation non appari\u00e9",
            u8"omatchad indexerande hakparantes",
        },
    },
    {
        "unmatched parenthesis"_translatable,
        {
            u8"unmatched parenthesis",
            u8"Zugeh\u00f6rige Klammer fehlt",
            u8"UNMATCHED PARENTHESIS",
            u8"parenth\u00e8se non appari\u00e9e",
            u8"omatchad parantes",
        },
    },
    {
        "unopened block comment"_translatable,
        {
            u8"unopened block comment",
            u8"Blockkommentar ohne Beginn",
            u8"UNOPENED BLOCK COMMENT",
            u8"commentaire de bloc non ouvert",
            u8"unopened block comment",
        },
    },
    {
        "use 'while' instead to loop until a condition is false"_translatable,
        {
            u8"use 'while' instead to loop until a condition is false",
            u8"'while' benutzen, um zu iterieren, bis die Bedingung false wird",
            u8"USE 'WHILE' INSTEAD TO LOOP UNTIL A CONDITION IS FALSE",
            u8"utiliser plut\u00f4t 'while' pour boucler jusqu'\u00e0 ce qu'une condition soit fausse",
            u8"anv\u00e4nd 'while' ist\u00e4llet f\u00f6r att iterera till vilkoren \u00e4r falskt",
        },
    },
    {
        "use of undeclared variable: {0}"_translatable,
        {
            u8"use of undeclared variable: {0}",
            u8"Variable {0} wird verwendet, ist jedoch nicht deklariert",
            u8"USE OF UNDECLARED VARIABLE: {0}",
            u8"utilisation d'une variable non d\u00e9clar\u00e9e : {0}",
            u8"anv\u00e4ndning av odeklarerad variabel: {0}",
        },
    },
    {
        "variable already declared here"_translatable,
        {
            u8"variable already declared here",
            u8"Variable wurde zuvor hier deklariert",
            u8"VARIABLE DECLARED HERE",
            u8"variable d\u00e9j\u00e0 d\u00e9clar\u00e9e ici",
            u8"variabel \u00e4r redan deklarerad h\u00e4r",
        },
    },
    {
        "variable assigned before its declaration"_translatable,
        {
            u8"variable assigned before its declaration",
            u8"Zuweisung an Variable vor Deklaration",
            u8"VARIABLE USED BEFORE DECLARATION: {0}",
            u8"variable affect\u00e9e avant sa d\u00e9claration",
            u8"tilldelar variabel f\u00f6re deklaration",
        },
    },
    {
        "variable declared here"_translatable,
        {
            u8"variable declared here",
            u8"Variablendeklaration ist hier",
            u8"VARIABLE DECLARED HERE",
            u8"variable d\u00e9clar\u00e9e ici",
            u8"variabel deklarerades h\u00e4r",
        },
    },
    {
        "variable used before declaration: {0}"_translatable,
        {
            u8"variable used before declaration: {0}",
            u8"Variable '{0}' wird ihrer Deklaration verwendet",
            u8"VARIABLE USED BEFORE DECLARATION: {0}",
            u8"variable utilis\u00e9e avant sa d\u00e9claration : {0}",
            u8"variabel anv\u00e4nd f\u00f6re: {0}",
        },
    },
    {
        "what is this '{1}' nonsense?"_translatable,
        {
            u8"what is this '{1}' nonsense?",
            u8"Was soll dieser '{1}' Humbug?",
            u8"what is this '{1}' nonsense?",
            u8"what is this '{1}' nonsense?",
            u8"what is this '{1}' nonsense?",
        },
    },
    {
        "while loop is missing '{1}' around condition"_translatable,
        {
            u8"while loop is missing '{1}' around condition",
            u8"'{1}' fehlt um Bedingung der while-Schleife",
            u8"WHILE LOOP IS MISSING '{1}' AROUND CONDITION",
            u8"une boucle while n\u00e9cessite '{1}' autour de la condition",
            u8"while loop saknar '{1}' runt vilkoren",
        },
    },
    {
        "while loop needs parentheses around condition"_translatable,
        {
            u8"while loop needs parentheses around condition",
            u8"Klammern fehlen um Bedingung der while-Schleife",
            u8"WHILE LOOP NEEDS PARENTHESES AROUND CONDITION",
            u8"une boucle while n\u00e9cessite des parenth\u00e8ses autour de la condition",
            u8"while loop beh\u00f6ver paranteser runt vilkoren",
        },
    },
    {
        "with statement is missing '{1}' around expression"_translatable,
        {
            u8"with statement is missing '{1}' around expression",
            u8"'{1}' fehlt um Ausdruck der with-Anweisung",
            u8"WITH STATEMENT IS MISSING '{1}' AROUND EXPRESSION",
            u8"une instruction with n\u00e9cessite '{1}' autour de l'expression",
            u8"with p\u00e5st\u00e5ende saknar '{1}' runt uttryck",
        },
    },
    {
        "with statement needs parentheses around expression"_translatable,
        {
            u8"with statement needs parentheses around expression",
            u8"Klammern fehlen um Ausdruck der with-Anweisung",
            u8"WITH STATEMENT NEEDS PARENTHESES AROUND EXPRESSION",
            u8"une instruction with n\u00e9cessite des parenth\u00e8ses autour de l'expression",
            u8"with p\u00e5st\u00e5ende saknar paranteser runt uttryck",
        },
    },
    {
        "{0} with no bindings"_translatable,
        {
            u8"{0} with no bindings",
            u8"{0} ohne Binding",
            u8"{0} WITH NO BINDINGS",
            u8"{0} sans liaisons",
            u8"{0} utan bindningar",
        },
    },
    {
        "~~~ invalid string, do not use outside benchmark ~~~"_translatable,
        {
            u8"~~~ invalid string, do not use outside benchmark ~~~",
            u8"~~~ invalid string, do not use outside benchmark ~~~",
            u8"~~~ invalid string, do not use outside benchmark ~~~",
            u8"~~~ invalid string, do not use outside benchmark ~~~",
            u8"~~~ invalid string, do not use outside benchmark ~~~",
        },
    },
};
// clang-format on
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
