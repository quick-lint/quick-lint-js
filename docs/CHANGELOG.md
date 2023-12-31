# Release notes

This document chronicles quick-lint-js' releases. This document's format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

quick-lint-js' version numbers are arbitrary. quick-lint-js does *not* adhere to
Semantic Versioning.

## Unreleased

### Fixed

* TypeScript support (still experimental):
  * Assigning to a variable with the same name as an `import`ed type no longer
    falsely reports [E0185][] ("assignment to imported variable").
  * Interface index signature variables can now be named contextual keywords
    such as `type`.
  * Writing `++x` inside `? :` no longer falsely reports [E0254][] ("unexpected
    ':' in expression; did you mean 'as'?").

## 2.19.0 (2023-12-30)

[Downloads](https://c.quick-lint-js.com/releases/2.19.0/)

### Added

* Unicode 15.1 is now supported, including new CJK code points in identifiers.
* Missing `break;` (or `return;` or `throw ...;` or `// fallthrough`) after a
  clause in `switch` statement now reports [E0427][] ("missing 'break;' or '//
  fallthrough' comment between statement and 'case'"). (Implemented by [Yash
  Masani][].)
* Detection of multiple `export default` statements ([E0715][]) now also applies
  to `export {... as default};` statements.
* JSX elements and fragments are now allowed in JSX attributes without
  surrounding them in `{` and `}` (e.g.
  `<List header=<ListHeader />>{items}</List>`).
* TypeScript support (still experimental):
  * `export as namespace` statements are now parsed.
  * Const generic parameters (`<const T>`) are now parsed.
  * Assertion signatures (`function f(param): asserts param`) are now parsed.
  * If a type predicate appears outside a return type, quick-lint-js now reports
    [E0426][] ("type predicates are only allowed as function return types").
  * `export default` now supports separately-declared TypeScript interfaces and
    types.
  * `cond ? (param): ReturnType => body : f` is now correctly parsed as a
    conditional expression with a function in the truthy branch.
    (`cond ? (t) : param => body` continues to be parsed as a conditional
    expression with a function in the falsy branch.)
  * Using `<<` in an interface's `extends` clause, in a class's `implements`
    clause, or in a `typeof` type now reports [E0429][] instead of misleading
    diagnostics. (Implemented by [strager][] and [Ariel Don][].)
  * Properties in object literal types can now be named with number and string
    literals.
  * Repeating `in` or `out` generic parameter modifiers now reports [E0432][]
    ("'in' or 'out' variance specifier cannot be listed twice").
  * Writing a newline after an `in`, `out`, or `const` generic parameter
    modifier now reports [E0440][] ("newline is not allowed after 'in' modifier
    in generic parameter"). (Implemented by [Ariel Don][].)
  * Negative number literals are now parsed in types.
  * Parameter decorators are now parsed.
  * Overload signatures are now allowed on exported functions.
  * Assigning to a class now reports [E0003][] ("cannot assign to class").
  * Definite assignment assertions (`!` after a variable name in `let` or `var`)
    is now supported.
  * `override` is now supported in classes.
  * Abstract constructor types (such as `abstract new () => C`) are now
    supported.
  * `export class { m(); }` in a .d.ts file no longer falsely reports [E0172][]
    ("missing body for function").
  * `import()` type assertions are now allowed in TypeScript types.
  * U+0085 (Next Line) is now interpreted as whitespace.

### Fixed

* Class decorators may now reference the class by name.
* Decorators are now parsed correctly when using a semicolon-free coding style.
* Properties named 'private', 'declare', or similar names are now parsed
  correctly when using a semicolon-free coding style.
* A newline between an arrow function and a parenthesized expression (e.g.
  `let f = () => {} /*newline*/ (console.log('x'));` no longer falsely reports
  [E0211][] ("missing parentheses around self-invoked function").
* `for await (async of []);` no longer falsely reports [E0082][] ("assigning to
  'async' in a for-of loop requires parentheses"). (`for (async of []);` still
  reports the diagnostic.)
* A `return` statement inside a nested `switch` no longer falsely repots
  [E0427][] ("missing 'break;' or '// fallthrough' comment between " "statement
  and 'case'").
* TypeScript support (still experimental):
  * Types named `await`, `implements`, `interface`, `let`, `package`, `private`,
    `protected`, `public`, `static`, and `yield` are now recognized in type
    signatures.
  * `export default` with a class and an interface (triggering declaration
    merging) no longer fasely reports [E0715][] ("cannot use multiple `export
    default` statements in one module").
  * `class C<T extends U, U> {}` no longer falsely reports [E0058][] ("variable
    used before declaration").
  * `case await x:` no longer treats `:` as if it was a type annotation colon in
    an arrow function parameter list.
  * Fixed a crash if certain diagnostics are reported after a TypeScript
    interface. (Implemented by [Rui Serra][].)
  * `import("modulename").Class<<T>(params) => ReturnType>` in a type is now
    parsed correctly.
  * `import("modulename")` in a type is now allowed and no longer falsely
    reports a diagnostic.
  * `import type * from 'othermodule';` no longer crashes quick-lint-js with an
    assertion failure.
  * Generic call signatures are now parsed correctly when using a semicolon-free
    coding style.
  * Interface index signatures and computed property names in interfaces are now
    parsed correctly when using a semicolon-free coding style.
  * Properties named 'extends' or 'is' are now parsed correctly when using a
    semicolon-free coding style.
  * Types named 'asserts' are parsed correctly when using a semicolon-free
    coding style.
  * `declare const x = 42;` no longer falsely reports [E0351][] or [E0385][].
  * `type T = T.thing;` no longer falsely reports [E0384][] ("cannot use type
    directly in its own definition").
  * Interface methods and index signatures can now end in `,` (in addition to
    `;`).
  * Parsing of `;` and `,` between and after properties in object types is much
    less buggy.
  * quick-lint-js no longer incorrectly reports [E0384][] ("cannot use type
    directly in its own definition") when using `extends ? :`.
  * Making a type alias and a function with the same name no longer falsely
    reports [E0034][] ("redeclaration of variable").
  * Importing a type then declaring a function or variable with the same name no
    longer falsely reports [E0034][] ("redeclaration of variable").
  * Importing a function or variable then declaring a type with the same name no
    longer falsely reports [E0034][] ("redeclaration of variable").
  * `T extends () => RT ? A : B` no longer falsely reports [E0348][]
    ("unexpected '?' in type; use '| void' to make an optional type").
  * `<T extends />` is now correctly parsed as a JSX element.
  * `interface I { get: any; }` (field named `get` with a type annotation) no
    longer reports [E0054][] ("unexpected token"). (Implemented by [Rui
    Serra][].)
  * In type assertions, certain types such as in `<string[]>expr` and
    `< <T>() => RT>expr`, are no longer incorrectly interpreted as JSX.
  * Assigning to an enum or namespace no longer crashes quick-lint-js with an
    assertion failure.
  * Subnamespaces can now be named contextual keywords such as `string`.
  * Import aliases can now be named contextual keywords such as `implements`.
  * Optional function parameters can now be named contextual keywords such as
    `readonly`.
  * Import aliases can now be declared with `import type`.
  * Namespace aliases can now reference variables named contextual keywords such as
    `yield` inside namespaces.
  * Type annotations can now reference types inside namespaces named contextual
    keywords such as `string`.
  * `import A = ns; class A {}` no longer crashes or falsely reports that `A`
    was redeclared.
  * `import {type A from 'mod'}; let A;` no longer crashes or falsely reports
    that `A` was redeclared.
  * `extends` checks with multiple `infer`s no longer crash or falsely report
    that the inferred variable was redeclared.
  * `class implements I {}` is now parsed as a class with no name rather than a
    class with the name `implements`.
  * `class C extends Base<T> implements I {}` now correctly parses `Base<T>` as
    a type with generic arguments rather than logical comparisons.
  * Variables declared inside a `declare global` block are now correctly
    declared as global variables instead of module variables. This means that
    variables inside a `declare global` block can be shadowed by module
    variables without diagnostics.
  * Fixed false negatives for [E0196][] if a type has the same name as a
    variable.
  * Using a variable in a type or interface (e.g. with `typeof`) no longer
    falsely reports [E0058][] ("variable used before declaration").
  * Type predicates in function types no longer falsely report [E0315][]
    ("'param' is not the name of a parameter").
  * Arrow functions with return type annotations such as `(42)` or `(string[])`
    are now parsed correctly.
  * `f<T>?.()` (optional chaining function call with generic function arguments)
    is now parsed correctly.
  * In a .d.ts file, `declare module` without a body no longer falsely reports
    errors.
  * `export default` inside a `declare module` no longer falsely reports
    [E0715][] ("cannot use multiple `export default` statements in one module").
  * `typeof` in types now supports variables named `boolean`, `string`, and some
    other names.
  * An optional parameter with `?` followed by an optional parameter with `=` no
    longer falsely reports [E0379][] ("optional parameter cannot be followed by
    a required parameter").
  * `extends` in some cases is no longer incorrectly interpreted as an `infer`
    constraint, such as in
    `MyType extends (infer T extends U ? T1 : F1) ? T2 : F2;`.
  * Nested `extends`, such as in `A extends () => B extends C ? D : E ? F : G`,
    no longer falsely reports [E0348][] ("unexpected '?' in type; use '| void'
    to make an optional type").
  * `class T<T> {}` no longer falsely reports [E0034][] ("redeclaration of
    variable").
  * `await <T>() => {}` no longer does confusing things.

## 2.18.0 (2023-11-03)

[Downloads](https://c.quick-lint-js.com/releases/2.18.0/)

### Added

* Mixing `&` and `<<` such as in `a & 0x1 << 3` now reports [E0716][]
  ("unintuitive operator precedence when using & and << or >>"). (Implemented by
  [toastin0][].)

### Fixed

* A missing operator in an `if` condition (such as in `if (x y)`) no longer
  causes [E0065][] ("'else' has no corresponding 'if'") to be reported.
  (Implemented by [Ariel Don][].)
* `cmake --install` with `--component build-tools` now installs the build
  tools. (This is a regression introduced in quick-lint-js version 2.16.0.)
* Windows: The installer and executables are now signed with a non-expired
  certificate chain.

## 2.17.0 (2023-10-25)

[Downloads](https://c.quick-lint-js.com/releases/2.17.0/)

### Known issues

* `cmake --install` with `--component build-tools` does not install the build
  tools. (This is a regression introduced in quick-lint-js version 2.16.0.)
  Fix: [Git commit
  3923f0df76d24b73d57f15eec61ab190ea048093][cmake-install-component-build-tools-patch]
* Windows: Code signing does not validate on some machines. [Workaround: follow
  SSL.com's instructions for removing the expired "Certum Trusted Network CA"
  certificate.](https://www.ssl.com/blogs/ssl-com-legacy-cross-signed-root-certificate-expiring-on-september-11-2023/#ftoc-heading-7)

### Added

* quick-lint-js now understands decorators.
* quick-lint-js now understands `accessor` fields.
* `class C { myField, }` now reports better diagnostics.
* quick-lint-js now compiles correctly on big-endian architectures such as
  S/390 (Linux s390x).
* Missing commas in array literals now reports [E0712][]. (Implemented by
  [koopiehoop][].)
* `get *prop()` and `set *prop()` in classes now report [E0713][] ("getters and
  setters cannot be generators"). (Implemented by [koopiehoop][].)
* `async get` and `async set` in classes now report [E0714][] ("'async' keyword
  is not allowed on getters or setters"). (Implemented by [koopiehoop][].)
* Multiple `export default` statements now report [E0715][] ("cannot use
  multiple `export default` statements in one module"). (Implemented by
  [Ariel Don][].)
* Emacs: The Debian/Ubuntu package now installs the Emacs plugin. Manual
  installation of the .el files is no longer required.
* CLI: The new `--stdin-path` CLI option allows users of the `--stdin` option
  (primarily text editors) to have quick-lint-js detect the language
  automatically via `--language=default` or `--language=experimental-default`.
* TypeScript support (still experimental):
  * CLI: The new `--language=experimental-default` option auto-detects the
    language based on the file's extension (`.ts`, `.tsx`, `.d.ts`, or `.js`).
  * Emacs: Flymake plugin users can now opt into TypeScript support. See the
    [Flymake configuration instructions][emacs-configure-flymake].
  * Class method overload signatures are now parsed.
  * [E0398][] is now reported when using both `abstract` and `static` on a
    single class property.
  * `,` is now allowed after interface fields. (Previously only `;` or a newline
    was allowed.)
  * `if (cond);` now reports [E0064][] ("missing body for 'if' statement").
    (Implemented by [Samir Hamud][].)
  * Type predicates are now supported in function types (e.g.
    `(param) => param is Type`).
  * `declare` fields are now parsed inside classes.
  * `declare global` blocks are now parsed.

### Fixed

* [E0072][] is no longer falsely reported if `function` has a newline after it.
* [E0196][] is no longer falsely reported if the shadowing variable is declared
  in the head of a `for` loop. For example, quick-lint-js no longer warns about
  `let x; for (let x = 0;;);`.
* Emacs: .el files are now installed in the correct place on Arch Linux, btw.
* Emacs: The Flymake plugin now reliably clears out diagnostics after issues are
  fixed. Sticky diagnostics are no more.
* TypeScript support (still experimental):
  * A newline after `public`, `protected`, `private`, or `readonly` inside a
    class is now interpreted correctly.
  * `<T>(T: T) => {}` (a generic arrow function with the same name for a
    run-time parameter and a generic parameter) no longer falsely reports
    [E0034][] ("redeclaration of variable").
  * A namespace with the same name as an interface or type alias no longer
    falsely reports [E0034][] ("redeclaration of variable").
  * `declare class` and `function` with the same name no longer falsely reports
    [E0034][] ("redeclaration of variable").
  * `(a?, ...b) => ReturnType` in a TypeScript type no longer falsely reports
    [E0379][] ("optional parameter cannot be followed by a required parameter").
  * LSP: A file named `a.tsxbanana.ts` is no longer recognized as a
    TypeScript JSX file. It is now recognized as a non-JSX TypeScript
    file.
  * Nested `module` declarations no longer falsely report [E0361][]. E0361's
    message has been changed:
    * Before: "module with string name is only allowed at the top level"
    * After: "TypeScript 'declare module' with string name is not allowed in
      namespaces"

## 2.16.0 (2023-09-06)

[Downloads](https://c.quick-lint-js.com/releases/2.16.0/)

### Known issues

* `cmake --install` with `--component build-tools` does not install the build
  tools. Fix: [Git commit
  3923f0df76d24b73d57f15eec61ab190ea048093][cmake-install-component-build-tools-patch]

### Added

* `2 ^ 8` now reports [E0710][] ("'^' is the XOR operator; to exponentiate, use
  '\*\*' instead"). (Implemented by [pedrobl1718][].)
* A missing `}` in a function parameter list now reports [E0161][] ("unclosed
  object literal; expected '}'"). (Implemented by [Yash Masani][].)
* quick-lint-js now builds on Alpine Linux and other musl-based distributions.
* TypeScript support (still experimental):
  * Invalid recursive type definitions such as `type T = T;` now report
    [E0384][] ("cannot use type directly in its own definition").
  * quick-lint-js now recognizes `import` types.
  * quick-lint-js now recognizes `.d.ts` files:
    * CLI: `--language=experimental-typescript-definition`
    * LSP server: `typescriptdefinition`; `typescript` will detect from the URI,
      and `typescriptsource` prevents detection from the URI
    * Visual Studio Code extension: detected from the file name

### Fixed

* LSP: On macOS, quick-lint-js no longer hangs if a file has too many
  diagnostics.
* `#property in object` expressions are now parsed correctly and do not report
  [E0155][].
* `({k = defaultValue} = o);` no longer incorrectly reports [E0253][] ("use ':'
  instead of '=' in object literals").
* Class property initializers no longer incorrectly report [E0058][] ("variable
  used before declaration"). Example:
  `class C { myProperty = f(); }  const f = () => {};`
* TypeScript support (still experimental):
  * quick-lint-js no longer falsely reports [E0058][] ("variable used before
    declaration") for code such as `function f<T extends T[]>() {}`.
  * Declaring a type alias or an interface inside a `declare namespace` no
    longer reports [E0357][] ("'declare namespace' cannot contain statements,
    only declarations").
  * `const enum E {}` no longer causes [E0150][].
  * Using a `declare`-ed variable prior to its declaration is now allowed.
    Example: `new C(); declare class C { }`
  * Using a variable in a `declare class`'s `extends` clause prior to the
    variable's declaration is now allowed.
    Example: `declare class Derived extends Base { } class Base { }`
  * VS Code: The extension now loads when only opening a TypeScript file.
    (Previously, the extension would only load when you opened a JavaScript or
    JSON file.) The `quick-lint-js.experimental-typescript` setting is still
    required.
  * Vim ALE: JSX syntax is now recognized inside `.tsx` files.

### Changed

* quick-lint-js no longer depends on the Boost third-party library. The
  `QUICK_LINT_JS_USE_BUNDLED_BOOST` CMake variable now has no effect.

## 2.15.0 (2023-07-18)

[Downloads](https://c.quick-lint-js.com/releases/2.15.0/)

### Added

* LSP server: quick-lint-js can now receive configuration options via the
  `initialize` request. See [LSP configuration
  documentation](https://quick-lint-js.com/docs/lsp/#_configuration) for
  details.
* `x = x;` now reports [E0383][]. (Implemented by [Austin Garcia][].)
* `...` without something following it now reports either [E0708][] ("unexpected
  '...'; expected expression") or [E0709][] ("expected variable name after
  '...'"). (Implemented by [Isaac Nonato][].)
* TypeScript support (still experimental):
  * `namespace A.B {}` syntax (with `.`) is now supported.
  * `static public myMethod() {}` now reports [E0380][] ("'public' access
    specifier must precede 'static'"). (Implemented by [Leszek Nowicki][].)
  * `class C { \u{63}onstructor() {} }` now reports [E0381][] ("Typescript does
    not allow keywords to contain escape sequence"). (Implemented by [Jait
    Jacob][].)
  * `export declare` is now supported.

### Fixed

* Emacs (Flycheck): Using `C-x C-;` to comment a line now runs quick-lint-js
  (instead of leaving old diagnostics on the screen). This requires that you
  update your Emacs file: add `new-line` to the
  `flycheck-check-syntax-automatically` variable.
* `export default abstract;` is now parsed correctly as the export of a variable
  named `abstract`.
* `export default async (newline) function f() {}` is now parsed correctly as
  the export of a variable named `async` followed by the declaration of a
  non-async function named `f`.
* TypeScript support (still experimental):
  * [E0034][] ("redeclaration of variable") is no longer incorrectly reported
    in various cases, including the following:
      * `class A {} namespace A {}`
      * `function f<T>(T) {}`
      * `type X = null; var X;`
      * `namespace ns{} namespace ns{}`
  * `export = a b` now reports [E0027][] ("missing semicolon after statement").
  * `module 'name' { import ... }` no longer falsely reports [E0362][] ("cannot
    import a module from inside a 'declare namespace'").
  * `module 'name';` no longer falsely reports [E0356][] ("missing body for
    TypeScript namespace").

## 2.14.0 (2023-05-22)

[Downloads](https://c.quick-lint-js.com/releases/2.14.0/)

### Added

* Missing props in JSX tags now report [E0376][] ("JSX prop is missing an
  expression"). (Implemented by [James Moles][].)
* `! ==` (with a space) now reports [E0373][] or [E0374][]. (Implemented by
  [daethtech][].)
* `x == y ?? true` now reports [E0369][]. (Implemented by [Kate Conkright][].)
* `apt install quick-lint-js` now works on ARM64 (AArch64) Debian and Ubuntu
  installations.
* TypeScript support (still experimental):
    * Certain invalid parameter properties (such as in `constructor(public {id,
      name})`) now report diagnostics.
    * `function f(a?, b) {}` now reports [E0379][]. (Implemented by [Alek
      Lefebvre][].)

### Fixed

* Arrow functions with TypeScript parameter type annotations no longer report
  confusing diagnostics in JavaScript mode. (Implemented by [Leszek Nowicki][].)
* TypeScript support (still experimental):
  * '\`hello${world}\` as const' no longer falsely reports [E0291][].
    (Fixed by [Leszek Nowicki][].)
* Windows: Fixed a build failure with MinGW GCC 13.1.0.
* Shell completions are now up to date with the CLI. (Fixed by [Jake
  Castelli][].)
* FreeBSD: quick-lint-js no longer fails to build.

### Changed

* Ubuntu 18.04 LTS Bionic is no longer supported. It might happen to work, but it
  is not extensively tested. Canonical is itself ending support for Bionic on
  May 31, 2023, so please upgrade to Ubuntu 20.04 LTS or newer.

## 2.13.0 (2023-04-13)

[Downloads](https://c.quick-lint-js.com/releases/2.13.0/)

### Known issues

* Building quick-lint-js on [FreeBSD is
  broken](https://github.com/quick-lint/quick-lint-js/issues/999).

### Added

* quick-lint-js now recognizes [Deno][]'s global variables, including `Deno`.
  This suppresses undesired [E0057][] ("use of undeclared variable") warnings.
  This is controlled by the [`deno` global group][config-global-groups].
* `if () {}` (without a condition) now reports [E0452][] ("empty parenthesis
  after control statement"). (Implemented by [Yunus][].)
* `return x,` now reports [E0026][] ("missing operand for operator").
  (Implemented by [Tom Binford][].)
* TypeScript support (still experimental):
    * The `satisfies` operator is now recognized.
    * The `infer` operator is now recognized.
    * The `in` and `out` keywords for generic parameter variance are now
      recognized.
    * Parameter properties (`constructor(public readonly name: string, public
      age: number)`) are now recognized.
    * `C<T>=y;` now reports [E0365][] ("TypeScript requires whitespace between
      '>' and '=' here").
    * `extends` in `interface` now allows generic arguments, such as in
      `interface I extends T<U> {}`.
    * `extends` in `interface` now allows nested namespaces, such as in
      `interface I extends ns1.ns2.ns3.I {}`.
* FreeBSD: `quick-lint-js --debug-apps` now works. (Implemented by [Nico
  Sonack][].)

### Fixed

* Fixed several false errors:
  * `do while (x); while (y);` no longer falsely reports [E0101][] ("missing
    body for do-while loop").
  * `{ var async; async }` no longer falsely reports [E0054][] ("unexpected
    token").
  * `async[x]` no longer falsely reports [E0054][] ("unexpected token").
  * `await x ? y : z` no longer falsely reports [E0311][] ("missing parentheses
    around parameter").
  * `if (c) async () => {}; else {}` no longer falsely reports [E0065][]
    ("'else' has no corresponding 'if'").
  * `class A extends await {}` no longer falsely reports [E0111][] ("missing
    body for class").
  * `class A extends await() {}` no longer falsely reports [E0176][] ("missing
    arrow operator for arrow function").
  * `do if (c) {} else (b); while (d);` no longer falsely reports [E0103][]
    ("missing 'while (condition)' for do-while statement").
* `if (c) {} else (b) d;` now correctly reports [E0027][] ("missing semicolon
  after statement").
* The CLI's `--output-format=gnu-like` output (default) no longer prints
  terminal escape sequences for "dumb" terminals (where `TERM=dumb`). (Fixed by
  [wagner riffel][].)
* FreeBSD: The quick-lint-js-licenses.txt file is now populated correctly.
* TypeScript support (still experimental):
    * `let x: C<T>=y;` no longer falsely reports an error.
    * `class A extends B<C> {}` no longer falsely reports an error.
    * `T extends keyof O ? A : B` no longer falsely reports an error.

### Changed

* Cross compiling now requires you to build with
  `QUICK_LINT_JS_ENABLE_BUILD_TOOLS` for the build machine then with
  `QUICK_LINT_JS_USE_BUILD_TOOLS` for the target machine. See the
  [cross-compiling documentation][cross-compiling-quick-lint-js] for details.
* Code signing certificate has been refreshed. The old certificate expires on
  April 18, 2023 (PDT) and the new certificate expires on April 17, 2024 (PDT).
  The public key is the same between the two certificates:
  `7ea531a42cd3e7161b6951f93d83449546e90722` (SHA1).

## 2.12.0 (2023-03-08)

[Downloads](https://c.quick-lint-js.com/releases/2.12.0/)

### Added

* `array[i, j]` now reports [E0450][] ("misleading use of ',' operator in
  index") (implemented by [Yunus][]).
* `while (x > 0, y > 0)` now reports [E0451][] ("misleading use of ',' operator
  in conditional statement") (implemented by [Yunus][]).
* Improvements to experimental TypeScript support:
  * Type variables such as `Readonly<T>` and `IArguments` are now recognized by
    the new `typescript` global group which is enabled by default.
  * `declare` is now supported.
  * The old-style `module` syntax for namespaces is now supported.

### Fixed

* Fixed [E0062][] being reported when [E0061][] should be reported instead
  (fixed by [Yunus][]).
* TypeScript `namespace` without a body now reports [E0356][] instead of
  crashing with an assertion failure.

### Changed

* quick-lint-js' build system now optionally creates executes and runs them
  during the build. This behavior is controlled by the
  `QUICK_LINT_JS_ENABLE_BUILD_TOOLS` and `QUICK_LINT_JS_USE_BUILD_TOOLS` CMake
  variables.
  * For most people, `QUICK_LINT_JS_ENABLE_BUILD_TOOLS` is enabled by default
    and should work without extra configuration. No action is needed.
  * When cross-compiling, `QUICK_LINT_JS_ENABLE_BUILD_TOOLS` is disabled.
    Currently, `QUICK_LINT_JS_ENABLE_BUILD_TOOLS` is optional, so this should
    behave as before. However, in the future, either
    `QUICK_LINT_JS_ENABLE_BUILD_TOOLS` or `QUICK_LINT_JS_USE_BUILD_TOOLS` will
    be required.
  * If you need to configure the build tools specially, or if you want to build
    the build tools when cross-compiling, you must use the
    `QUICK_LINT_JS_USE_BUILD_TOOLS` CMake variable. See the [cross-compiling
    documentation][cross-compiling-quick-lint-js] for instructions on using
    `QUICK_LINT_JS_USE_BUILD_TOOLS` correctly.

## 2.11.0 (2023-01-31)

[Downloads](https://c.quick-lint-js.com/releases/2.11.0/)

### Changed

* Debian: The apt repository signing key expired on (2023-01-21). This causes
  `apt-get update` to fail. To fix this issue, add the latest signing key by
  running the following command:

      $ curl https://c.quick-lint-js.com/quick-lint-js-release.key | sudo apt-key add -

* LSP: The quick-lint-js LSP server no longer accepts JSON-RPC 2.0 batch
  messages. In practice, LSP clients don't use this feature, so this breaking
  change should do no harm. The LSP specification recently started
  [prohibiting batch
  messages](https://github.com/microsoft/language-server-protocol/pull/1651) to
  match the status quo.

### Added

* LSP: The LSP server now recognizes and lints TypeScript and TypeScript JSX
  code. However, the editor plugins currently do **not** use this feature by
  default.
* Vim: You can opt into TypeScript support. Search for `EXPERIMENTAL` in
  `:help quick-lint-js`. (Disabled by default.)
* VS Code: You can now opt into experimental TypeScript support. Set the
  `quick-lint-js.experimental-typescript` setting to `true`. (Disabled by
  default.)
* `switch (c) { case A: break; case A: break; }` now reports [E0347][]
  ("duplicated case clause in switch statement") (implemented by [Rebraws][]).
* Translations: Brazilian Portuguese (implemented by [Guilherme Vasconcelos][]).
* Mixing up the order of the `export`, `async`, and `function` keywords now
  reports [E0326][] or [E0327][] (implemented by [Alek Lefebvre][]).
* TypeScript types like `string?` now report [E0348][] (implemented by
  [Alek Lefebvre][]).
* Extra parentheses in function parameters now report [E0349][] (function
  parameter cannot be parenthesized) (implemented by [Harshit Aghera][]).
* QuickJS's global variables are now recognized by default via the new `quickjs`
  global group (implemented by [wagner riffel]).
* [E0190][] is now reported if the literal is `undefined`, not only `null`,
  strings, and numbers (implemented by [Harshit Aghera][]).

### Fixed

* Variables can be named according to Unicode 15 (previously Unicode 14).
* In tracing files, the thread IDs in the file headers are now correct if
  tracing was enabled after startup.
* [E0325][] is now an error rather than a warning.
* `0o123.toString()`, `0x123.toString()`, `0b101.toString()`, and
  `0123.toString()` no longer incorrectly report [E0046][], [E0047][], or
  [E0048][].
* Using a variable called `async`, then exporting something, no longer reports
  an unexpected token error (implemented by [Alek Lefebvre][]).
* `class C { x = 0, y; }` now reports [E0330][] ("',' should be ';' instead")
  instead of [E0057][] ("use of undeclared variable: y") (implemented by
  [clegoz][]).

## 2.10.0 (2022-10-14)

[Downloads](https://c.quick-lint-js.com/releases/2.10.0/)

### Added

* [E0286][] and [E0287][] are now reported if the string or the value have extra
  parentheses (such as in `(tag.toUpperCase()) === 'img'`) (implemented by
  [Rebraws][]).
* `myArray === []` now reports [E0344][] (implemented by [Rebraws][]).
* `myArray === [a, b, c]` now reports [E0341][] (implemented by [Rebraws][]).
* CLI: The new [`--language` option][cli-language] lets you opt out of JSX
  support. With `--language=javascript`, quick-lint-js will report [E0177][] for
  any uses of JSX.
* You can opt into TypeScript support in the CLI using the
  [`--language=experimental-typescript` option][cli-language]. TypeScript
  support is experimental, so there will be bugs (including crashes) in
  quick-lint-js. This option does not affect the LSP server.
* [E0149][] and [E0150][] are now reported for labelled statements too
  (implemented by [Guilherme Vasconcelos][]).

### Fixed

* `static` code blocks in classes no longer report E0242 ("TypeScript static
  blocks are not supported in JavaScript").
* When writing `implements` in JavaScript code, quick-lint-js will no longer
  possibly report [E0246][] in addition to [E0247][]. quick-lint-js will now
  only report [E0247][].
* `{ x: y = z }` in an expression no longer incorrectly reports [E0253][].
* `{ x: y = z }` now reports errors such as [E0057][] for `z`. (`z` used to be
  completely ignored during variable analysis.)
* `{ x = z }` now only reports [E0253][], instead of also reporting [E0059][]
  for `x`.
* `class C { field!; }` now only reports [E0239][] (TypeScript assignment-asserted
  fields are not supported in JavaScript) instead of both E0239 and
  [E0236][] (assignment-asserted field must have a type annotation).

### Changed

* [E0707][] has been merged into [E0069][].
* [E0271][] has been merged into [E0270][].
* [E0279][] has been merged into [E0278][].

## 2.9.0 (2022-09-05)

[Downloads](https://c.quick-lint-js.com/releases/2.9.0/)

### Added

* `tag.toUpperCase() === 'img'` now reports [E0286][], and
  `tag.toLowerCase() === 'IMG'` now reports [E0287][] (implemented by [Roland
  Strasser][]).

### Fixed

* `(param) { body(); }` now reports [E0176][] ('missing arrow operator for arrow
  function') (implemented by [Roland Strasser][]).
* quick-lint-js no longer crashes if a TypeScript `type` alias appears in a
  JavaScript file.
* quick-lint-js now recognizes the `jest` global variable from the Jest test
  framework. This suppresses undesired [E0057][] ("use of undeclared variable")
  warnings.
* `await () => x` no longer reports `x` as undeclared multiple times. ([E0178][]
  is still reported, as intended.)
* Defining a global variable in `quick-lint-js.config` which is already declared
  by default now always changes the `shadowable` and `writable` settings.
* `await () => await () => ...` no longer slows down linting and consumes a lot
  of memory.

## 2.8.0 (2022-07-25)

[Downloads](https://c.quick-lint-js.com/releases/2.8.0/)

### Added

* `await await f()` now reports [E0266][] ("redundant await") (implemented by
  [clegoz][]).
* CLI: Misuse of `--vim-file-bufnr` now reports warnings (implemented by [Roland
  Strasser][]).

### Fixed

* LSP, VS Code: quick-lint-js now correctly handles merging lines containing
  non-ASCII with lines containing ASCII. This fixes spurious diagnostics. Thanks
  to T0x3139 and [mirabellier][] for reporting and helping fix this bug.
* quick-lint-js now recognizes [Bun][]'s global variables, including `Bun` and
  `bunJSX`. This suppresses undesired [E0057][] ("use of undeclared variable")
  warnings.

### Removed

* The `en@loud` translation has been removed. It was only used for testing.

### Changed

* Debian/Ubuntu: Our Debian/Ubuntu package now might not work on Ubuntu 16.04 or
  Debian 9. The following Debian-based Linux distributions are tested:
  * Debian 10 Bullseye and 11 Buster
  * PureOS Amber
  * Ubuntu 18.04 Bionic and 20.04 Focal
* Manual: Our Linux x86_64/amd64 pre-built binaries now might not work on Ubuntu
  16.04 or similarly old distributions. The following Linux distributions are
  tested:
  * Arch Linux
  * Debian 10 Bullseye and 11 Buster
  * Fedora 35 and 36
  * Ubuntu 18.04 Bionic and 20.04 Focal

## 2.7.0 (2022-07-13)

[Downloads](https://c.quick-lint-js.com/releases/2.7.0/)

### Added

* `const o = {k = v};` now reports [E0253][] ("use ':' instead of '=' in object
  literals").
* `--snarky` is now even snarkier.
* LSP server: The new `quick-lint-js.tracing-directory` LSP configuration item
  can be set to a path where quick-lint-js will log LSP protocol traffic. This
  keylogger option can be used by quick-lint-js contributors to investigate
  bugs. This new option is disabled by default.

### Fixed

* quick-lint-js no longer fails to build with simdjson version 2.2.0.
* Declaring a TypeScript interface with the same name as a generic parameter no
  longer crashes quick-lint-js.

## 2.6.0 (2022-06-15)

[Downloads](https://c.quick-lint-js.com/releases/2.6.0/)

### Added

* `DOMError`, `MutationEvent`, and some other browser global variables are now
  recognized.
* New global group: `web-worker` (enabled by default)
* The `importScripts` global function is now recognized by default.
* Forgetting a semicolon between class fields now reports "missing semicolon
  after field" ([E0223][]) instead of "missing semicolon after statement"
  ([E0027][]).
* Using some TypeScript features, such as class generics and interfaces, in
  JavaScript code is now reported with a friendly error message.
* [E0199][] ("unclosed class") is now reported in more scenarios.
* `static` code blocks in classes are now supported.

### Fixed

* [E0707][] now only complains about classes named `await`, not any class inside
  an async function.
* [E0054][] is no longer incorrectly reported for class properties named
  `implements`, `interface`, `package`, `private`, `protected`, or `public`.
* In statement contexts, `async` followed by a newline followed by `function` is
  no longer falsely interpreted as an async function. It is instead interpreted
  as the use of a variable called `async` followed by a non-async function, per
  the language standard.
* `((x)) => {}` no longer crashes the parser with an assertion failure.
* Tests now pass if the user's locale is Italian (`it_IT.utf8` on Linux).
* The FreeBSD build now succeeds and tests pass.

## 2.5.0 (2022-05-23)

[Downloads](https://c.quick-lint-js.com/releases/2.5.0/)

### Added

* [E0144][] and [E0145][] are now reported for `implements`, `interface`,
  `package`, `private`, `protected`, and `public` (in addition to other
  keywords).
* [E0086][] is now reported in more cases, such as when the deleted variable was
  declared outside the function containing the `delete` expression.
* Classes named `await` in `async` functions now report [E0707][] (implemented
  by [ooblegork][]).
* Integer literals which are silently rounded at run-time now report [E0212][]
  (implemented by [Rob Miner][]).
* VS Code: The new `quick-lint-js.tracing` setting can be set to `"verbose"` to
  log changes to documents. This keylogger option can be used by quick-lint-js
  contributors to investigate bugs. This new option is disabled by default.
* CLI: The `--snarky` option makes error messages more interesting. Implemented
  by [david doroz][].

### Fixed

* [E0150][] no longer crashes quick-lint-js when using a German (`de`) locale.
  (Fixed by [Nico Sonack][].)
* The npm package now includes `copyright` files listing software licenses and
  copyright notices.
* [E0086][] is no longer falsely reported in some cases.
* VS Code (Windows x64): When the extension is first loaded, and a file has
  errors, quick-lint-js no longer reports the first error always on the first
  line. (The issue was a [compiler
  bug](https://lists.gnu.org/archive/html/bug-binutils/2022-05/msg00099.html)
  which we worked around.)
* quick-lint-js no longer creates and writes messages to `/tmp/qljs.log`.

### Changed

* This release is signed with a new GPG key.
  * Old fingerprint: 0327DE8F9CEF499851D19F6ED20BA9DCCF0E9D20
  * New fingerprint: A6851D57A65803E98C05DA01C08A7BC89CA2F557

## 2.4.2 (2022-04-22)

[Downloads](https://c.quick-lint-js.com/releases/2.4.2/)

### Changed

* The name of package installed by the MSIX installer has changed. It used to be
  "fa6b940b-8f96-4b46-8e21-703a63133e06" but now it is
  "quick-lint.quick-lint-js". If you installed quick-lint-js using a prior
  version of the MSIX installer, Windows won't let you upgrade. Uninstall the
  old version of quick-lint-js before installing a new version.

## 2.4.1 (2022-04-19)

[Downloads](https://c.quick-lint-js.com/releases/2.4.1/)

### Fixed

* When using the `de`, `fr_FR`, or `sv_SV` locale, some error messages are no
  longer blank.

### Changed

* The Windows MSIX installer is now signed with a Microsoft-trusted certificate.
  If you installed quick-lint-js using a prior version of the MSIX installer,
  Windows won't let you upgrade. Uninstall the old version of quick-lint-js
  before installing a new version.
* The Windows executable is now signed with a Microsoft-trusted certificate.

## 2.4.0 (2022-04-15)

[Downloads](https://c.quick-lint-js.com/releases/2.4.0/)

### Added

* Missing parentheses around a self-invoked arrow function are now reported as
  [E0211][]. (Implemented by [Sarah Schulte][].)
* [E0179][] is now reported if a JSX element such as `<div />` follows `return`
  (implemented by [Rob Miner][]).

### Fixed

* `<div> <> </> </div>` (a JSX fragment inside a JSX element) no longer reports
  a syntax error.

### Changed

* Windows builds (x86, x64, ARM, and ARM64) are now built with MinGW's C and C++
  runtime libraries instead of Microsoft's. This fixes a compliance issue with
  GPLv3.

## 2.3.1 (2022-03-24)

[Downloads](https://c.quick-lint-js.com/releases/2.3.1/)

### Added

* Windows: The 32-bit x86 builds are now code-signed.
* Windows: Tab-completion now works in PowerShell. (Completion needs to be
  [installed manually][install-powershell-completions].) (Implemented by [Tony
  Sathre][].)

### Fixed

* The executable files shared between the npm packages and the static builds are
  now identical. (Previously, the files sometimes differed because they were
  code-signed independently. Code signing is non-deterministic.)

## 2.3.0 (2022-02-24)

[Downloads](https://c.quick-lint-js.com/releases/2.3.0/)

### Added

* An incomplete `class` now reports [E0199][] instead of failing with the
  catch-all [E0054][]. (Thanks to [Dave Churchill][] for reporting.)
* Statements before the first `case` in a `switch` now report [E0198][]
  (implemented by [Himanshu][]).
* When building from source, you can tell quick-lint-js to use your copy of
  third-party dependencies instead of quick-lint-js' bundled
  dependencies:
  * `-DQUICK_LINT_JS_USE_BUNDLED_BOOST=OFF`: Use your own copy of Boost.
  * `-DQUICK_LINT_JS_USE_BUNDLED_GOOGLE_BENCHMARK=OFF`: Use your own copy of Google Benchmark (benchmarks only).
  * `-DQUICK_LINT_JS_USE_BUNDLED_GOOGLE_TEST=OFF`: Use your own copy of Google Test (tests only).
  * `-DQUICK_LINT_JS_USE_BUNDLED_SIMDJSON=OFF`: Use your own copy of simdjson.

## 2.2.0 (2022-02-17)

[Downloads](https://c.quick-lint-js.com/releases/2.2.0/)

### Fixed

* `x && <Element/>` no longer falsely reports [E0026][] (missing operand for
  operator). (Thanks to [Piotr Dąbrowski][] for reporting.)
* In top-level code, `await <x/>` is now parsed as the `await` operator followed
  by a JSX element (rather than `await` less-than-compared to `x`, followed by
  jibberish).

### Changed

* Homebrew: Emacs Lisp files are now installed in
  `<brew>/share/emacs/site-lisp/quick-lint-js` (per `brew audit`'s
  recommendation) instead of in `share/emacs/site-lisp`.

## 2.1.0 (2022-02-09)

[Downloads](https://c.quick-lint-js.com/releases/2.1.0/)

### Fixed

* Curried arrow functions like `a => b => { a; b; }` no longer falsely reports
  [E0057][]. (Thanks to [Christian Mund][] for reporting.)

## 2.0.0 (2022-02-08)

[Downloads](https://c.quick-lint-js.com/releases/2.0.0/)

### Added

* quick-lint-js now supports JSX syntax in both .js and .jsx files.
* New diagnostics for JSX: [E0019][], [E0181][], [E0182][], [E0183][],
  [E0186][], [E0187][], [E0189][], [E0191][], [E0192][], [E0193][]
* `if (...) {...} else (...) {...}` now reports [E0184][] ('missing `if` after
  `else`') (implemented by [Himanshu][]).
* `if (x = "")` now reports [E0188][] ('`=` changes variables; to compare, use
  `===` instead').
* `if (a == "X" || "Y")` now reports [E0190][] ('missing comparison; `===` does
  not extend to the right side of `||`').
* `async (param1, param2) {}` now reports [E0176][] ('missing arrow operator for
  arrow function'). ([E0176][] was previously reported only for non-`async`
  arrow functions.)
* `let x = 'nah'; if (y) { let x = 'yah'; }` now reports [E0196][] ('new
   variable shadows existing variable'). This warning is reported only when an
   assignment (instead of a new variable declaration) was intended.
* `console.log(“hello world”);` now reports [E0197][] (''“' is not allowed for
  strings; use " instead').
* `-1 ** 2` now reports [E0194][] (missing parentheses around left-hand side of
  `**`).
* `typeof 10 ** 7` now reports [E0195][] (missing parentheses around operand of
  `typeof`).

### Fixed

* LSP: When responding to unsupported methods with error code -32601,
  quick-lint-js now includes the request ID. (Previously, the `"id"` field was
  always `null`.)
* CLI: If a crash occurs due to [E0054][] or [E0203][], the CLI no longer
  crashes (e.g. with an illegal instruction error).
* quick-lint-js no longer ignores elements of assigned arrays. For example,
  `[fisrt, second] = s.split(' ');` will now report [E0057][] for `fisrt` (if
  `fisrt` is not declared).
* quick-lint-js no longer incorrectly reports [E0176][] (missing arrow operator
  for arrow function) if the `extends` clause for a class is parenthesized
  and contains commas (e.g. `class A extends (B, C) {}`).
* quick-lint-js no longer incorrectly reports [E0016][], [E0038][], [E0060][],
  or [E0207][] in tagged template literals. (These errors are still reported for
  untagged template literals and for string literals.)

### Changed

* Assigning to an imported variable now reports [E0185][] ('assignment to
  imported variable') instead of [E0003][] ('assignment to const variable')
  (implemented by [Matheus de Sousa][]).

## 1.0.0 (2021-12-13)

[Downloads](https://c.quick-lint-js.com/releases/1.0.0/)

### Added

* Windows: Clang-cl is now able to compile quick-lint-js for Windows.
* [E0176][] is now reported if the desired arrow function has more than zero
  parameters.
* [E0178][] is now reported if the arrow function appears in top-level code or
  inside a non-async function.

### Fixed

* Arch Linux: The quick-lint-js package now installs correctly if you have
  another Vim plugin package installed. The installation error was: "error: failed
  to commit transaction (conflicting files)"
* `(typeof x)=>{}` and similar code now reports [E0151][] (invalid function
  parameter) instead of [E0019][] (invalid binding in let statement).
* `([(x,)] => {})` now declares `x` as a parameter instead of ignoring `x`
  entirely. (It still produces [E0180][]).
* `\u0` at the end of a file now reports [E0016][] (expected hexadecimal digits
  in Unicode escape sequence) instead of [E0038][] (unclosed identifier escape
  sequence)
* `"\u{00a0:"` now reports only one error ([E0038][] (unclosed identifier escape
  sequence)) instead of two ([E0038][] and [E0040][] (unclosed string literal)).
* After reporting [E0178][], the arrow function is parsed as if it was async,
  allowing the body to use `await` as an operator without error.

### Changed

* [E0013][] is no longer reported for identifiers such as `bird\u{360000}`.
  [E0207][] is reported instead.

### Optimized

* The Windows .exe is 51% smaller (2.30 MiB -> 1.14 MiB) by making a hash table
  used for translations compile-time-only. [See the patch
  here.](https://github.com/quick-lint/quick-lint-js/commit/408e7db762736081cfff05c82c997c5aea7f4ab5)

## 0.7.1 (2021-12-06)

Beta release.

[Downloads](https://c.quick-lint-js.com/releases/0.7.1/)

### Added

* npm: The npm package now installs on Windows when using the x86 (32-bit)
  version of Node.js.
* [E0036][] is now reported in more situations.

### Fixed

* npm: The npm package now installs on macOS Apple silicon when using the
  AArch64 (native) version of Node.js. (The package previously worked only on
  macOS Intel, or when using x86_64 Node.js with Rosetta.)

## 0.7.0 (2021-12-05)

Beta release.

[Downloads](https://c.quick-lint-js.com/releases/0.7.0/)

### Security

* The source archive (`quick-lint-js-*.tar.gz`) is now signed with our GPG key.
* Arch Linux: The release package on AUR now verifies that the source archive is
  signed with our GPG key.

### Added

* New diagnostics: [E0053][], [E0179][]
* Improve error reporting of `console.("hello")` and similar code.
* Reported errors now include a link to the website for a detailed explanation
  (implemented by [Amir][]).
* CLI: New `--diagnostic-hyperlinks` option (implemented by [Amir][]).
* CLI: Fish and Zsh completions for the `--output-format` option now include
  documentation for each value (implemented by [Amir][]).

### Fixed

* Reporting [E0144][] no longer also reports [E0057][].
* Variables can be named according to Unicode 14 (previously Unicode 13).
* `void 0?a:b=c` no longer reports [E0020][].
* Vim: The Debian package, the Arch Linux package, and the Homebrew package now
  install the coc.nvim plugin and the plugin documentation.
* Neovim: Fixed with newer versions of nvim-lspconfig
  (since commit
  [97da7ed12e](https://github.com/neovim/nvim-lspconfig/commit/97da7ed12e7e0d86e735e38a8170e941d4ed3e9a)
  published November 25, 2021).

### Optimized

* Identifier parsing is now SIMD-optimized for ARM systems, including Apple
  silicon. [See the patch
  here.](https://github.com/quick-lint/quick-lint-js/commit/79cf6e71f42722a8eca28ab20f288abdc41ec162)
* Diagnostic message translations consume less space in executables and also
  take less time to process. [See the patch
  here.](https://github.com/quick-lint/quick-lint-js/commit/1dcedfe985a3a4ddf956d629907265e46b2c6aed)
* Expression ASTs are now garbage-collected during parsing, reducing peak memory
  usage (and as a side effect making parsing faster). [See the patch
  here.](https://github.com/quick-lint/quick-lint-js/commit/9d96b4c54c81fc95f1094f129bf1fdc5db8d02e3)

## 0.6.0 (2021-11-20)

Beta release.

[Downloads](https://c.quick-lint-js.com/releases/0.6.0/)

### Added

* Various new diagnostics (implemented by [Himanshu][] and [Matheus de
  Sousa][]).
* VS Code: The new `quick-lint-js.logging` setting allows you to show
  quick-lint-js' internal log messages in an Output window.
* VS Code: Apple silicon (e.g. M1) (ARM64) is now supported.
* VS Code: Linux ARM (32-bit) is now supported.
* Vim: Improved performance for ALE versions v2.5.0 and newer (and v3.0.0 and
  newer). You should configure `g:ale_lint_on_text_changed` to `'always'` for
  the best experience.
* Vim: Improved performance of the ALE plugin by using the LSP server by
  default.
* The macOS and Windows binaries are now code-signed.
* Emacs (Eglot): You no longer need to type `M-x eglot` in order to start
  linting. Apply this fix by [adding `(eglot-ensure)` to your init
  file](https://quick-lint-js.com/install/emacs/configure/#eglot).
* quick-lint-js now understands the `AggregateError`, `FinalizationRegistry`,
  and `WeakRef` ECMAScript global variables by default.
* The macOS and Windows executables now include embedded code signatures. These
  signatures are made with a self-signed certificate.
* The Linux executables are now signed with a GPG signature (`.asc` files).
* Vim: [coc.nvim][] is now supported.
* quick-lint-js has a new mascot, Dusty. Say hello! (Artwork by [Jenny
  "Jennipuff" Wheat][]).
* Translations: German (implemented by [Nico Sonack][])

### Fixed

* macOS: quick-lint-js no longer hangs if a file is changed in your home
  directory or project directory.
* Various crashes on invalid code have been fixed.
* quick-lint-js consumes less memory for pathological code patterns.
* VS Code: The extension no longer tries to load an ARM64 DLL on Windows x64.
* The npm package and the manual builds are now much smaller and faster. (They
  were previously compiled in debug, unoptimized mode.)
* `delete x` no longer reports a warning if `x` is a global variable.
* JSX: Instead of reporting a bunch of errors, quick-lint-js now tells you that
  JSX syntax is not yet supported.
* FreeBSD: Fixed build.
* [E0073][], [E0094][], [E0104][], [E0106][], [E0111][], and [E0119][] now point
  to a more helpful place (implemented by strager and [Amir][]).
* `for (let x = a in b; c; d) {}` now reports [E0108][] instead of reporting
  [E0173][], [E0110][], and [E0110][] again.

### Changed

* Error codes now have four decimal digits instead of three. For example, E001
  is now called E0001.
* LSP benchmarks have been rewritten. The new benchmarks should produce more
  stable numbers and be fairer to linters with a high start-up time such as
  Flow.

### Removed

* quick-lint-js no longer looks for files named `.quick-lint-js.config`. To
  configure quick-lint-js, name your file `quick-lint-js.config` instead.

## 0.5.0 (2021-10-12)

Beta release.

[Downloads](https://c.quick-lint-js.com/releases/0.5.0/)

### Added

* Various new errors (implemented by [Himanshu][]).
* `quick-lint-js.config` now supports the [**literally-anything** global
  group][config-global-groups]. Use this option to disable all
  use-of-undeclared-variable warnings.
* If quick-lint-js crashes, it will link to a page to [report
  crashes](https://quick-lint-js.com/crash-report/).
* VS Code: The plugin now works on Windows on ARM (64-bit).
* VS Code: The plugin now works on Linux on ARM (32-bit and 64-bit).
* The CLI and LSP server now compile on FreeBSD (amd64). However, we don't
  provide FreeBSD pre-built executables.
* Neovim: nvim-lspconfig is now supported by quick-lint-js' plugin (implemented
  by [tiagovla][]).
* VS Code: Contributors can now enable [performance
  tracing](https://github.com/quick-lint/quick-lint-js/blob/1e7947ba71711479e04fe9100e2d09d202015926/plugin/vscode/PERFORMANCE-TRACING.md)
  (implemented by [Jimmy Qiu][]).

### Fixed

* LSP, VS Code: Filesystem change watching failures no longer crash. These
  failures can happen on Windows for directories on network shares, for example.
* LSP: Unknown messages no longer crash the LSP server. This makes the LSP
  server compatible with more clients, such as Neovim's built-in client.
* `break await` and `break yield` no longer incorrectly reports errors (fixed by
  [Himanshu][]).
* `for (var x = ++y in []) {}` now parses correctly as valid JavaScript.
* Vim: The plugin no longer crashes if a filename contains a newline character
  (or certain other control characters).
* LSP, VS Code: Config file changes are now recognized properly if any path
  component contains a symbolic link (Linux and macOS).
* quick-lint-js no longer warns about variables named `__dirname`,
  `__filename`, `exports`, `module`, or `require` by default.
* Parsing certain code patterns containing `await/` no longer takes excessive
  memory and time.
* Various crashes on invalid code have been fixed.
* LSP, VS Code: Some memory leaks have been fixed.
* Arch Linux: building should no longer fail with "The install of the
  quick-lint-js target requires changing an RPATH from the build tree".

## 0.4.0 (2021-09-09)

Beta release.

[Downloads](https://c.quick-lint-js.com/releases/0.4.0/)

### Changed

* CLI: The `--stdin` option (and `-`) no longer search for configuration files
  in the current working directory. Use the `--path-for-config-search` option to
  approximate the old behavior.
* Emacs: For scratch buffers, the Flycheck and Flymake plugins no longer
  search for configuration files.
* Vim: For unnamed buffers, the ALE plugin no longer searches for configuration
  files in the current working directory's parent and ancestors. Name the buffer
  using the `:file` command if you want a configuration file to take effect.
* VS Code: The plugin has been rewritten. Performance should be better, but
  stability and compatibility might suffer. For example, VS Code on Apple
  silicon (AArch64 CPUs) has not been tested and will likely no longer work.
  Please report compatibility issues.

### Added

* Various new errors (implemented by [Himanshu][])
* CLI: `--path-for-config-search` allows customizing where configuration files
  are found for `--stdin`
* Global variables for several popular libraries are now recognized by default,
  including jQuery, Jasmine, and Jest
* Syntax and schema errors in `quick-lint-js.config` no longer crash
  quick-lint-js. Instead, these errors are highlighted similar to errors in
  JavaScript files.
* VS Code: `quick-lint-js.config` is now used when linting opened JavaScript
  files.

### Fixed

* Building quick-lint-js with AddressSanitizer on Fedora no longer fails in
  `collect-copyright`
* Building quick-lint-js no longer fails if a directory called `brew` exists
  anywhere in `$PATH`
* Various crashes given invalid JavaScript no longer happen (implemented by
  [wagner riffel][] and [David Vasileff][])
* `for (const x of xs)` no longer incorrectly reports [E205][E0205] (missing
   initializer in const declaration) (fixed by [Himanshu][])
* Windows: `quick-lint-js.config` files are now recognized if the containing
  directory contains non-ASCII characters
* Fix SSE2 corruption on 32-bit Windows builds with MSVC.

## 0.3.0 (2021-07-02)

Beta release.

[Downloads](https://c.quick-lint-js.com/releases/0.3.0/)

### Security

* Vim plugin: Version 0.2.0 executed `node_modules/.bin/quick-lint-js` without
  validating that the program is really quick-lint-js. The plugin now
  conservatively ignores `quick-lint-js` in `node_modules` by default.

### Added

* New JS syntax: `.?`, class fields, `import.meta`, top-level `await`,
  `#privateProperties`
* Translations: Swedish (implemented by [Kim "Linden"][])
* Emacs plugin (implemented by [wagner riffel][])
* Neovim plugin
* Errors and warnings are now documented on the website
* Various new errors and error message improvements (implemented by
  [AidenThing][], [Daniel La Rocque][], [Erlliam Mejia][], [Himanshu][], [Lee
  Wannacott][], [Matheus de Sousa][])
* Shell completion for the CLI (Bash, Fish, Zsh) (implemented by [Shivam
  Mehta][])
* `--stdin` CLI option allows reading JavaScript from standard input instead of
  a file (implemented by [wagner riffel][])
* Partially implemented: Global variables can be configured using a
  `quick-lint-js.config` file

### Fixed

* Using browser variables such as `document` and `window` no longer reports
  undesired undeclared variable warnings
* `with` statements and `eval` no longer cause spurious undeclared variable
  warnings (implemented by [Himanshu][])
* `++a[0];` no longer reports an assignment-to-const-variable error if `a` was
  declared with `const` (implemented by [Himanshu][])
* `'undefined' === typeof foo ? 3 : 4` no longer reports a warning that `foo` is
  undeclared (implemented by [Himanshu][])
* Building tests with GCC 11.1.1 no longer fails due to `-Werror`
* Various crashes given invalid JavaScript no longer happen
* Building the VS Code plugin now works on Windows (implemented by [Jimmy
  Qiu][])

## 0.2.0 (2021-04-05)

Beta release.

[Downloads](https://c.quick-lint-js.com/releases/0.2.0/)

[Bun]: https://bun.sh/
[Deno]: https://deno.land/
[cli-language]: ../cli/#language
[cmake-install-component-build-tools-patch]: https://github.com/quick-lint/quick-lint-js/commit/3923f0df76d24b73d57f15eec61ab190ea048093.patch
[coc.nvim]: https://github.com/neoclide/coc.nvim
[config-global-groups]: https://quick-lint-js.com/config/#global-groups
[cross-compiling-quick-lint-js]: https://quick-lint-js.com/contribute/build-from-source/cross-compiling/
[emacs-configure-flymake]: https://quick-lint-js.com/install/emacs/configure/#flymake
[install-powershell-completions]: https://github.com/quick-lint/quick-lint-js/blob/master/completions/README.md#powershell

[AidenThing]: https://github.com/AidenThing
[Alek Lefebvre]: https://github.com/AlekLefebvre
[Amir]: https://github.com/ahmafi
[Ariel Don]: https://github.com/arieldon
[Austin Garcia]: https://github.com/holychowders
[Christian Mund]: https://github.com/kkkrist
[Daniel La Rocque]: https://github.com/dlarocque
[Dave Churchill]: https://www.cs.mun.ca/~dchurchill/
[David Vasileff]: https://github.com/dav000
[Erlliam Mejia]: https://github.com/erlliam
[Guilherme Vasconcelos]: https://github.com/Guilherme-Vasconcelos
[Harshit Aghera]: https://github.com/HarshitAghera
[Himanshu]: https://github.com/singalhimanshu
[Isaac Nonato]: https://github.com/isaacnonato
[Jait Jacob]: https://github.com/jaitjacob
[Jake Castelli]: https://github.com/jakecastelli
[James Moles]: https://github.com/JPMoles
[Jenny "Jennipuff" Wheat]: https://twitter.com/jennipaff
[Jimmy Qiu]: https://github.com/lifeinData
[Kate Conkright]: https://github.com/applepie23
[Kim "Linden"]: https://github.com/Lindenbyte
[Lee Wannacott]: https://github.com/LeeWannacott
[Leszek Nowicki]: https://github.com/leszek888
[Matheus de Sousa]: https://github.com/keyehzy
[Nico Sonack]: https://github.com/herrhotzenplotz
[Piotr Dąbrowski]: https://github.com/yhnavein
[Rebraws]: https://github.com/Rebraws
[Rob Miner]: https://github.com/robminer6
[Roland Strasser]: https://github.com/rol1510
[Rui Serra]: https://github.com/ruipserra
[Samir Hamud]: https://github.com/samir-hamud
[Sarah Schulte]: https://github.com/cgsdev0
[Shivam Mehta]: https://github.com/maniac-en
[Tom Binford]: https://github.com/TomBinford
[Tony Sathre]: https://github.com/tonysathre
[Yash Masani]: https://github.com/yashmasani
[Yunus]: https://github.com/yunusey
[clegoz]: https://github.com/clegoz
[daethtech]: https://github.com/daethtech
[david doroz]: https://github.com/DaviddHub
[koopiehoop]: https://github.com/koopiehoop
[mirabellier]: https://github.com/mirabellierr
[ooblegork]: https://github.com/ooblegork
[pedrobl1718]: https://github.com/pedrobl85
[strager]: https://github.com/strager
[tiagovla]: https://github.com/tiagovla
[toastin0]: https://github.com/toastin0
[wagner riffel]: https://github.com/wgrr

[E0001]: https://quick-lint-js.com/errors/E0001/
[E0003]: https://quick-lint-js.com/errors/E0003/
[E0013]: https://quick-lint-js.com/errors/E0013/
[E0016]: https://quick-lint-js.com/errors/E0016/
[E0019]: https://quick-lint-js.com/errors/E0019/
[E0020]: https://quick-lint-js.com/errors/E0020/
[E0026]: https://quick-lint-js.com/errors/E0026/
[E0027]: https://quick-lint-js.com/errors/E0027/
[E0034]: https://quick-lint-js.com/errors/E0034/
[E0036]: https://quick-lint-js.com/errors/E0036/
[E0038]: https://quick-lint-js.com/errors/E0038/
[E0040]: https://quick-lint-js.com/errors/E0040/
[E0046]: https://quick-lint-js.com/errors/E0046/
[E0047]: https://quick-lint-js.com/errors/E0047/
[E0048]: https://quick-lint-js.com/errors/E0048/
[E0053]: https://quick-lint-js.com/errors/E0053/
[E0054]: https://quick-lint-js.com/errors/E0054/
[E0057]: https://quick-lint-js.com/errors/E0057/
[E0058]: https://quick-lint-js.com/errors/E0058/
[E0059]: https://quick-lint-js.com/errors/E0059/
[E0060]: https://quick-lint-js.com/errors/E0060/
[E0061]: https://quick-lint-js.com/errors/E0061/
[E0062]: https://quick-lint-js.com/errors/E0062/
[E0064]: https://quick-lint-js.com/errors/E0064/
[E0065]: https://quick-lint-js.com/errors/E0065/
[E0069]: https://quick-lint-js.com/errors/E0069/
[E0072]: https://quick-lint-js.com/errors/E0072/
[E0073]: https://quick-lint-js.com/errors/E0073/
[E0086]: https://quick-lint-js.com/errors/E0086/
[E0094]: https://quick-lint-js.com/errors/E0094/
[E0101]: https://quick-lint-js.com/errors/E0101/
[E0103]: https://quick-lint-js.com/errors/E0103/
[E0104]: https://quick-lint-js.com/errors/E0104/
[E0106]: https://quick-lint-js.com/errors/E0106/
[E0108]: https://quick-lint-js.com/errors/E0108/
[E0110]: https://quick-lint-js.com/errors/E0110/
[E0111]: https://quick-lint-js.com/errors/E0111/
[E0119]: https://quick-lint-js.com/errors/E0119/
[E0144]: https://quick-lint-js.com/errors/E0144/
[E0145]: https://quick-lint-js.com/errors/E0145/
[E0149]: https://quick-lint-js.com/errors/E0149/
[E0150]: https://quick-lint-js.com/errors/E0150/
[E0151]: https://quick-lint-js.com/errors/E0151/
[E0155]: https://quick-lint-js.com/errors/E0155/
[E0161]: https://quick-lint-js.com/errors/E0161/
[E0173]: https://quick-lint-js.com/errors/E0173/
[E0176]: https://quick-lint-js.com/errors/E0176/
[E0177]: https://quick-lint-js.com/errors/E0177/
[E0178]: https://quick-lint-js.com/errors/E0178/
[E0179]: https://quick-lint-js.com/errors/E0179/
[E0180]: https://quick-lint-js.com/errors/E0180/
[E0181]: https://quick-lint-js.com/errors/E0181/
[E0182]: https://quick-lint-js.com/errors/E0182/
[E0183]: https://quick-lint-js.com/errors/E0183/
[E0184]: https://quick-lint-js.com/errors/E0184/
[E0185]: https://quick-lint-js.com/errors/E0185/
[E0186]: https://quick-lint-js.com/errors/E0186/
[E0187]: https://quick-lint-js.com/errors/E0187/
[E0188]: https://quick-lint-js.com/errors/E0188/
[E0189]: https://quick-lint-js.com/errors/E0189/
[E0190]: https://quick-lint-js.com/errors/E0190/
[E0191]: https://quick-lint-js.com/errors/E0191/
[E0192]: https://quick-lint-js.com/errors/E0192/
[E0193]: https://quick-lint-js.com/errors/E0193/
[E0194]: https://quick-lint-js.com/errors/E0194/
[E0195]: https://quick-lint-js.com/errors/E0195/
[E0196]: https://quick-lint-js.com/errors/E0196/
[E0197]: https://quick-lint-js.com/errors/E0197/
[E0198]: https://quick-lint-js.com/errors/E0198/
[E0199]: https://quick-lint-js.com/errors/E0199/
[E0203]: https://quick-lint-js.com/errors/E0203/
[E0205]: https://quick-lint-js.com/errors/E0205/
[E0207]: https://quick-lint-js.com/errors/E0207/
[E0211]: https://quick-lint-js.com/errors/E0211/
[E0212]: https://quick-lint-js.com/errors/E0212/
[E0223]: https://quick-lint-js.com/errors/E0223/
[E0236]: https://quick-lint-js.com/errors/E0236/
[E0239]: https://quick-lint-js.com/errors/E0239/
[E0246]: https://quick-lint-js.com/errors/E0246/
[E0247]: https://quick-lint-js.com/errors/E0247/
[E0253]: https://quick-lint-js.com/errors/E0253/
[E0266]: https://quick-lint-js.com/errors/E0266/
[E0270]: https://quick-lint-js.com/errors/E0270/
[E0271]: https://quick-lint-js.com/errors/E0271/
[E0278]: https://quick-lint-js.com/errors/E0278/
[E0279]: https://quick-lint-js.com/errors/E0279/
[E0286]: https://quick-lint-js.com/errors/E0286/
[E0287]: https://quick-lint-js.com/errors/E0287/
[E0291]: https://quick-lint-js.com/errors/E0291/
[E0311]: https://quick-lint-js.com/errors/E0311/
[E0325]: https://quick-lint-js.com/errors/E0325/
[E0326]: https://quick-lint-js.com/errors/E0326/
[E0327]: https://quick-lint-js.com/errors/E0327/
[E0330]: https://quick-lint-js.com/errors/E0330/
[E0341]: https://quick-lint-js.com/errors/E0341/
[E0344]: https://quick-lint-js.com/errors/E0344/
[E0345]: https://quick-lint-js.com/errors/E0345/
[E0347]: https://quick-lint-js.com/errors/E0347/
[E0348]: https://quick-lint-js.com/errors/E0348/
[E0349]: https://quick-lint-js.com/errors/E0349/
[E0356]: https://quick-lint-js.com/errors/E0356/
[E0357]: https://quick-lint-js.com/errors/E0357/
[E0361]: https://quick-lint-js.com/errors/E0361/
[E0362]: https://quick-lint-js.com/errors/E0362/
[E0365]: https://quick-lint-js.com/errors/E0365/
[E0369]: https://quick-lint-js.com/errors/E0369/
[E0373]: https://quick-lint-js.com/errors/E0373/
[E0374]: https://quick-lint-js.com/errors/E0374/
[E0376]: https://quick-lint-js.com/errors/E0376/
[E0379]: https://quick-lint-js.com/errors/E0379/
[E0380]: https://quick-lint-js.com/errors/E0380/
[E0381]: https://quick-lint-js.com/errors/E0381/
[E0383]: https://quick-lint-js.com/errors/E0383/
[E0384]: https://quick-lint-js.com/errors/E0384/
[E0398]: https://quick-lint-js.com/errors/E0398/
[E0426]: https://quick-lint-js.com/errors/E0426/
[E0427]: https://quick-lint-js.com/errors/E0427/
[E0429]: https://quick-lint-js.com/errors/E0429/
[E0450]: https://quick-lint-js.com/errors/E0450/
[E0451]: https://quick-lint-js.com/errors/E0451/
[E0452]: https://quick-lint-js.com/errors/E0452/
[E0707]: https://quick-lint-js.com/errors/E0707/
[E0708]: https://quick-lint-js.com/errors/E0708/
[E0709]: https://quick-lint-js.com/errors/E0709/
[E0710]: https://quick-lint-js.com/errors/E0710/
[E0712]: https://quick-lint-js.com/errors/E0712/
[E0713]: https://quick-lint-js.com/errors/E0713/
[E0714]: https://quick-lint-js.com/errors/E0714/
[E0715]: https://quick-lint-js.com/errors/E0715/
[E0716]: https://quick-lint-js.com/errors/E0716/
