# Release notes

This document chronicles quick-lint-js' releases. This document's format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

quick-lint-js' version numbers are arbitrary. quick-lint-js does *not* adhere to
Semantic Versioning.

## Unreleased

### Added

* Windows: The 32-bit x86 builds are now code-signed.

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
  Wannacott][], [Matheus Sousa][])
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

## Contributors

- [AidenThing](https://github.com/AidenThing)<br>
- [Amir](https://github.com/ahmafi)<br>
- [Christian Mund](https://github.com/kkkrist)<br>
- [Daniel La Rocque](https://github.com/dlarocque)<br>
- [Dave Churchill](https://www.cs.mun.ca/~dchurchill/)<br>
- [David Vasileff](https://github.com/dav000)<br>
- [Erlliam Mejia](https://github.com/erlliam)<br>
- [Himanshu](https://github.com/singalhimanshu)<br>
- [Jenny "Jennipuff" Wheat](https://twitter.com/jennipaff)<br>
- [Jimmy Qiu](https://github.com/lifeinData)<br>
- [Kim "Linden"](https://github.com/Lindenbyte)<br>
- [Lee Wannacott](https://github.com/LeeWannacott)<br>
- [Matheus de Sousa](https://github.com/keyehzy)<br>
- [Nico Sonack](https://github.com/herrhotzenplotz)<br>
- [Piotr Dąbrowski](https://github.com/yhnavein)<br>
- [Shivam Mehta](https://github.com/maniac-en)<br>
- [coc.nvim](https://github.com/neoclide/coc.nvim)<br>
- [config-global-groups](https://quick-lint-js.com/config/#global-groups)<br>
- [tiagovla](https://github.com/tiagovla)<br>
- [wagner riffel](https://github.com/wgrr)<br>

## Error codes

- [E0001](https://quick-lint-js.com/errors/E0001/): variable assigned before its declaration<br>
- [E0002](https://quick-lint-js.com/errors/E0002/): assignment to const global variable<br>
- [E0003](https://quick-lint-js.com/errors/E0003/): assignment to const
variable<br>
- [E0004](https://quick-lint-js.com/errors/E0004/): assignment to const variable before its declaration<br>
- [E0005](https://quick-lint-js.com/errors/E0005/): BigInt literal contains decimal point<br>
- [E0006](https://quick-lint-js.com/errors/E0006/): BigInt literal contains exponent<br>
- [E0007](https://quick-lint-js.com/errors/E0007/): classes cannot be named 'let'<br>
- [E0008](https://quick-lint-js.com/errors/E0008/): let statement cannot declare variables named 'let'<br>
- [E0009](https://quick-lint-js.com/errors/E0009/): cannot export variable named 'let'<br>
- [E0010](https://quick-lint-js.com/errors/E0010/): cannot import 'let'<br>
- [E0011](https://quick-lint-js.com/errors/E0011/): character is not allowed in identifiers<br>
- [E0012](https://quick-lint-js.com/errors/E0012/): escaped character is not allowed in identifiers<br>
- [E0013](https://quick-lint-js.com/errors/E0013/): code point out of range<br>
- [E0014](https://quick-lint-js.com/errors/E0014/): expected expression before newline<br>
- [E0015](https://quick-lint-js.com/errors/E0015/): expected expression before semicolon<br>
- [E0016](https://quick-lint-js.com/errors/E0016/): expected hexadecimal digits in Unicode escape sequence<br>
- [E0017](https://quick-lint-js.com/errors/E0017/): if statement needs parentheses around condition<br>
- [E0018](https://quick-lint-js.com/errors/E0018/): if statement is missing '(' or ')' around condition<br>
- [E0019](https://quick-lint-js.com/errors/E0019/): escaping '-' is not allowed in tag names; write '-' instead<br>
- [E0020](https://quick-lint-js.com/errors/E0020/): invalid expression left of assignment<br>
- [E0021](https://quick-lint-js.com/errors/E0021/): invalid lone literal in object literal<br>
- [E0022](https://quick-lint-js.com/errors/E0022/): invalid UTF-8 sequence<br>
- [E0023](https://quick-lint-js.com/errors/E0023/): keywords cannot contain escape sequences<br>
- [E0024](https://quick-lint-js.com/errors/E0024/): const/let/var with no bindings<br>
- [E0025](https://quick-lint-js.com/errors/E0025/): missing comma between object literal entries<br>
- [E0026](https://quick-lint-js.com/errors/E0026/): missing operand for
operator<br>
- [E0027](https://quick-lint-js.com/errors/E0027/): missing semicolon after statement<br>
- [E0028](https://quick-lint-js.com/errors/E0028/): number literal contains consecutive underscores<br>
- [E0029](https://quick-lint-js.com/errors/E0029/): number literal contains trailing underscores<br>
- [E0030](https://quick-lint-js.com/errors/E0030/): octal literal may not have exponent<br>
- [E0031](https://quick-lint-js.com/errors/E0031/): octal literal may not have decimal<br>
- [E0032](https://quick-lint-js.com/errors/E0032/): legacy octal literal may not be BigInt<br>
- [E0033](https://quick-lint-js.com/errors/E0033/): redeclaration of global variable<br>
- [E0034](https://quick-lint-js.com/errors/E0034/): redeclaration of
variable<br>
- [E0035](https://quick-lint-js.com/errors/E0035/): RegExp literal flags cannot contain Unicode escapes<br>
- [E0036](https://quick-lint-js.com/errors/E0036/): stray comma in let
statement<br>
- [E0037](https://quick-lint-js.com/errors/E0037/): unclosed block comment<br>
- [E0038](https://quick-lint-js.com/errors/E0038/): unclosed identifier escape sequence<br>
- [E0039](https://quick-lint-js.com/errors/E0039/): unclosed regexp literal<br>
- [E0040](https://quick-lint-js.com/errors/E0040/): unclosed string literal<br>
- [E0041](https://quick-lint-js.com/errors/E0041/): unclosed template<br>
- [E0042](https://quick-lint-js.com/errors/E0042/): unexpected '@'<br>
- [E0043](https://quick-lint-js.com/errors/E0043/): unexpected '\\' in identifier<br>
- [E0044](https://quick-lint-js.com/errors/E0044/): unexpected characters in number literal<br>
- [E0045](https://quick-lint-js.com/errors/E0045/): unexpected control
character<br>
- [E0046](https://quick-lint-js.com/errors/E0046/): unexpected characters in binary literal<br>
- [E0047](https://quick-lint-js.com/errors/E0047/): unexpected characters in octal literal<br>
- [E0048](https://quick-lint-js.com/errors/E0048/): unexpected characters in hex literal<br>
- [E0049](https://quick-lint-js.com/errors/E0049/): binary number literal has no digits<br>
- [E0050](https://quick-lint-js.com/errors/E0050/): hex number literal has no digits<br>
- [E0051](https://quick-lint-js.com/errors/E0051/): octal number literal has no digits<br>
- [E0052](https://quick-lint-js.com/errors/E0052/): unexpected '#'<br>
- [E0053](https://quick-lint-js.com/errors/E0053/): missing property name between '.' and '.'<br>
- [E0054](https://quick-lint-js.com/errors/E0054/): unexpected token<br>
- [E0055](https://quick-lint-js.com/errors/E0055/): unmatched indexing
bracket<br>
- [E0056](https://quick-lint-js.com/errors/E0056/): unmatched parenthesis<br>
- [E0057](https://quick-lint-js.com/errors/E0057/): use of undeclared
variable<br>
- [E0058](https://quick-lint-js.com/errors/E0058/): variable used before declaration<br>
- [E0059](https://quick-lint-js.com/errors/E0059/): assignment to undeclared variable<br>
- [E0060](https://quick-lint-js.com/errors/E0060/): invalid hex escape
sequence<br>
- [E0061](https://quick-lint-js.com/errors/E0061/): missing name in function statement<br>
- [E0062](https://quick-lint-js.com/errors/E0062/): missing name or parentheses for function<br>
- [E0063](https://quick-lint-js.com/errors/E0063/): missing operator between expression and arrow function<br>
- [E0064](https://quick-lint-js.com/errors/E0064/): missing body for 'if' statement<br>
- [E0065](https://quick-lint-js.com/errors/E0065/): 'else' has no corresponding 'if'<br>
- [E0066](https://quick-lint-js.com/errors/E0066/): exporting requires '{' and '}'<br>
- [E0067](https://quick-lint-js.com/errors/E0067/): exporting requires
'default'<br>
- [E0068](https://quick-lint-js.com/errors/E0068/): extra ',' is not allowed between function call arguments<br>
- [E0069](https://quick-lint-js.com/errors/E0069/): cannot declare 'await' inside async function<br>
- [E0070](https://quick-lint-js.com/errors/E0070/): commas are not allowed after spread parameter<br>
- [E0071](https://quick-lint-js.com/errors/E0071/): cannot declare 'yield' inside generator function<br>
- [E0072](https://quick-lint-js.com/errors/E0072/): methods should not use the 'function' keyword<br>
- [E0073](https://quick-lint-js.com/errors/E0073/): missing function parameter list<br>
- [E0074](https://quick-lint-js.com/errors/E0074/): '.' operator needs a key name; use + to concatenate strings; use [] to access with a dynamic key<br>
- [E0075](https://quick-lint-js.com/errors/E0075/): indexing requires an expression<br>
- [E0076](https://quick-lint-js.com/errors/E0076/): cannot declare and export variable with 'export default'<br>
- [E0077](https://quick-lint-js.com/errors/E0077/): function call before declaration in blocked scope<br>
- [E0078](https://quick-lint-js.com/errors/E0078/): missing expression between parentheses<br>
- [E0079](https://quick-lint-js.com/errors/E0079/): missing name of exported function<br>
- [E0080](https://quick-lint-js.com/errors/E0080/): missing name of class<br>
- [E0081](https://quick-lint-js.com/errors/E0081/): missing name of exported class<br>
- [E0082](https://quick-lint-js.com/errors/E0082/): assigning to 'async' in a for-of loop requires parentheses<br>
- [E0083](https://quick-lint-js.com/errors/E0083/): missing value for object property<br>
- [E0084](https://quick-lint-js.com/errors/E0084/): do-while loop needs parentheses around condition<br>
- [E0085](https://quick-lint-js.com/errors/E0085/): do-while loop is missing '(' or ')' around condition<br>
- [E0086](https://quick-lint-js.com/errors/E0086/): redundant delete statement on variable<br>
- [E0087](https://quick-lint-js.com/errors/E0087/): while loop needs parentheses around condition<br>
- [E0088](https://quick-lint-js.com/errors/E0088/): while loop is missing '(' or ')' around condition<br>
- [E0089](https://quick-lint-js.com/errors/E0089/): with statement needs parentheses around expression<br>
- [E0090](https://quick-lint-js.com/errors/E0090/): with statement is missing '(' or ')' around expression<br>
- [E0091](https://quick-lint-js.com/errors/E0091/): switch statement needs parentheses around condition<br>
- [E0092](https://quick-lint-js.com/errors/E0092/): switch statement is missing '(' or ')' around condition<br>
- [E0093](https://quick-lint-js.com/errors/E0093/): C-style for loop is missing its third component<br>
- [E0094](https://quick-lint-js.com/errors/E0094/): missing body for 'for'
loop<br>
- [E0095](https://quick-lint-js.com/errors/E0095/): Unicode byte order mark (BOM) cannot appear before #! at beginning of script<br>
- [E0096](https://quick-lint-js.com/errors/E0096/): missing for loop header<br>
- [E0097](https://quick-lint-js.com/errors/E0097/): for loop needs an iterable, or condition and update clauses<br>
- [E0098](https://quick-lint-js.com/errors/E0098/): for loop needs an iterable, or condition and update clauses<br>
- [E0099](https://quick-lint-js.com/errors/E0099/): missing semicolon between init and condition parts of for loop<br>
- [E0100](https://quick-lint-js.com/errors/E0100/): missing semicolon between condition and update parts of for loop<br>
- [E0101](https://quick-lint-js.com/errors/E0101/): missing body for do-while loop<br>
- [E0102](https://quick-lint-js.com/errors/E0102/): C-style for loops have only three semicolon-separated components<br>
- [E0103](https://quick-lint-js.com/errors/E0103/): missing 'while (condition)' for do-while statement<br>
- [E0104](https://quick-lint-js.com/errors/E0104/): missing body for while
loop<br>
- [E0105](https://quick-lint-js.com/errors/E0105/): missing parameters for arrow function<br>
- [E0106](https://quick-lint-js.com/errors/E0106/): missing body for 'switch' statement<br>
- [E0107](https://quick-lint-js.com/errors/E0107/): expected '{'<br>
- [E0108](https://quick-lint-js.com/errors/E0108/): 'in' disallowed in C-style for loop initializer<br>
- [E0109](https://quick-lint-js.com/errors/E0109/): for-of loop expression cannot have semicolons<br>
- [E0110](https://quick-lint-js.com/errors/E0110/): for-in loop expression cannot have semicolons<br>
- [E0111](https://quick-lint-js.com/errors/E0111/): missing body for class<br>
- [E0112](https://quick-lint-js.com/errors/E0112/): unexpected token in export; expected 'export default ...' or 'export {name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'<br>
- [E0113](https://quick-lint-js.com/errors/E0113/): incomplete export; expected 'export default ...' or 'export {name}' or 'export * from ...' or 'export class' or 'export function' or 'export let'<br>
- [E0114](https://quick-lint-js.com/errors/E0114/): unexpected token in variable declaration; expected variable name<br>
- [E0115](https://quick-lint-js.com/errors/E0115/): unexpected 'case' outside switch statement<br>
- [E0116](https://quick-lint-js.com/errors/E0116/): unexpected 'default' outside switch statement<br>
- [E0117](https://quick-lint-js.com/errors/E0117/): unexpected 'catch' without 'try'<br>
- [E0118](https://quick-lint-js.com/errors/E0118/): unexpected 'finally' without 'try'<br>
- [E0119](https://quick-lint-js.com/errors/E0119/): missing body for catch clause<br>
- [E0120](https://quick-lint-js.com/errors/E0120/): missing body for try statement<br>
- [E0121](https://quick-lint-js.com/errors/E0121/): missing body for finally clause<br>
- [E0122](https://quick-lint-js.com/errors/E0122/): missing catch or finally clause for try statement<br>
- [E0123](https://quick-lint-js.com/errors/E0123/): missing variable name<br>
- [E0124](https://quick-lint-js.com/errors/E0124/): cannot declare variable named keyword<br>
- [E0125](https://quick-lint-js.com/errors/E0125/): missing header and body for 'for' loop<br>
- [E0126](https://quick-lint-js.com/errors/E0126/): expected 'as' between '\*' and variable<br>
- [E0127](https://quick-lint-js.com/errors/E0127/): TypeScript's 'enum' feature is not yet implemented by quick-lint-js<br>
- [E0128](https://quick-lint-js.com/errors/E0128/): expected 'from' before module specifier<br>
- [E0129](https://quick-lint-js.com/errors/E0129/): expected 'from "name_of_module.mjs"'<br>
- [E0130](https://quick-lint-js.com/errors/E0130/): missing catch variable name between parentheses<br>
- [E0131](https://quick-lint-js.com/errors/E0131/): expected ',' between object literal entries<br>
- [E0132](https://quick-lint-js.com/errors/E0132/): missing ',' between variable declarations<br>
- [E0133](https://quick-lint-js.com/errors/E0133/): error generator function star belongs before name<br>
- [E0134](https://quick-lint-js.com/errors/E0134/): unclosed code block; expected '}' by end of file<br>
- [E0135](https://quick-lint-js.com/errors/E0135/): expected variable name for 'catch'<br>
- [E0136](https://quick-lint-js.com/errors/E0136/): cannot update variable with '+=' while declaring it<br>
- [E0137](https://quick-lint-js.com/errors/E0137/): missing condition for switch statement<br>
- [E0138](https://quick-lint-js.com/errors/E0138/): missing condition for if statement<br>
- [E0139](https://quick-lint-js.com/errors/E0139/): missing condition for while statement<br>
- [E0140](https://quick-lint-js.com/errors/E0140/): expected expression after 'case'<br>
- [E0141](https://quick-lint-js.com/errors/E0141/): expected ')' to close function call<br>
- [E0142](https://quick-lint-js.com/errors/E0142/): missing property name after '.' operator<br>
- [E0143](https://quick-lint-js.com/errors/E0143/): unmatched '}'<br>
- [E0144](https://quick-lint-js.com/errors/E0144/): cannot export variable named keyword<br>
- [E0145](https://quick-lint-js.com/errors/E0145/): cannot import variable named keyword<br>
- [E0146](https://quick-lint-js.com/errors/E0146/): missing ':' in conditional expression<br>
- [E0147](https://quick-lint-js.com/errors/E0147/): unexpected identifier in expression; missing operator before<br>
- [E0148](https://quick-lint-js.com/errors/E0148/): missing body for statement; a function statement is not allowed as the body of statement<br>
- [E0149](https://quick-lint-js.com/errors/E0149/): missing body for statement; a class statement is not allowed as the body of statement<br>
- [E0150](https://quick-lint-js.com/errors/E0150/): missing body for statement; a lexical declaration is not allowed as the body of statement<br>
- [E0151](https://quick-lint-js.com/errors/E0151/): invalid function
parameter<br>
- [E0152](https://quick-lint-js.com/errors/E0152/): legacy octal literals may not contain underscores<br>
- [E0153](https://quick-lint-js.com/errors/E0153/): forwarding exports are only allowed in export-from<br>
- [E0154](https://quick-lint-js.com/errors/E0154/): unexpected expression; missing key for object entry<br>
- [E0155](https://quick-lint-js.com/errors/E0155/): cannot reference private variables without object; use 'this.'<br>
- [E0156](https://quick-lint-js.com/errors/E0156/): private properties are not allowed in object literals<br>
- [E0157](https://quick-lint-js.com/errors/E0157/): missing end of array; expected ']'<br>
- [E0158](https://quick-lint-js.com/errors/E0158/): unexpected '=>'; expected parameter for arrow function, but got a literal instead<br>
- [E0159](https://quick-lint-js.com/errors/E0159/): unexpected literal in parameter list; expected parameter name<br>
- [E0160](https://quick-lint-js.com/errors/E0160/): unexpected '=>'; expected parameter for arrow function, but got an expression instead<br>
- [E0161](https://quick-lint-js.com/errors/E0161/): unclosed object literal; expected '}'<br>
- [E0162](https://quick-lint-js.com/errors/E0162/): 'await' is only allowed in async functions<br>
- [E0163](https://quick-lint-js.com/errors/E0163/): newline is not allowed between 'async' and arrow function parameter list<br>
- [E0164](https://quick-lint-js.com/errors/E0164/): JSON syntax error<br>
- [E0165](https://quick-lint-js.com/errors/E0165/): TypeScript style const
field<br>
- [E0166](https://quick-lint-js.com/errors/E0166/): "globals" descriptor "shadowable" property must be a boolean<br>
- [E0167](https://quick-lint-js.com/errors/E0167/): "globals" descriptor "writable" property must be a boolean<br>
- [E0168](https://quick-lint-js.com/errors/E0168/): "globals" must be an
object<br>
- [E0169](https://quick-lint-js.com/errors/E0169/): "global-groups" must be a boolean or an array<br>
- [E0170](https://quick-lint-js.com/errors/E0170/): "global-groups" entries must be strings<br>
- [E0171](https://quick-lint-js.com/errors/E0171/): "globals" descriptor must be a boolean or an object<br>
- [E0172](https://quick-lint-js.com/errors/E0172/): missing body for
function<br>
- [E0173](https://quick-lint-js.com/errors/E0173/): cannot assign to loop variable in for of/in loop<br>
- [E0174](https://quick-lint-js.com/errors/E0174/): functions/methods should not have '=>'<br>
- [E0175](https://quick-lint-js.com/errors/E0175/): expected variable name for 'import'-'as'<br>
- [E0176](https://quick-lint-js.com/errors/E0176/): missing arrow operator for arrow function<br>
- [E0177](https://quick-lint-js.com/errors/E0177/): React/JSX is not yet implemented<br>
- [E0178](https://quick-lint-js.com/errors/E0178/): 'await' cannot be followed by an arrow function; use 'async' instead<br>
- [E0179](https://quick-lint-js.com/errors/E0179/): return statement returns nothing (undefined)<br>
- [E0180](https://quick-lint-js.com/errors/E0180/): stray comma in function parameter<br>
- [E0181](https://quick-lint-js.com/errors/E0181/): unclosed string literal<br>
- [E0182](https://quick-lint-js.com/errors/E0182/): '>' is not allowed directly in JSX text; write {'>'} or &amp;gt; instead<br>
- [E0183](https://quick-lint-js.com/errors/E0183/): '}' is not allowed directly in JSX text; write {'}'} instead<br>
- [E0184](https://quick-lint-js.com/errors/E0184/): missing 'if' after
'else'<br>
- [E0185](https://quick-lint-js.com/errors/E0185/): assignment to imported variable; imported variable declared here<br>
- [E0186](https://quick-lint-js.com/errors/E0186/): missing '...' in JSX attribute spread<br>
- [E0187](https://quick-lint-js.com/errors/E0187/): mismatched JSX tags; expected &lt;/foo><br>
- [E0188](https://quick-lint-js.com/errors/E0188/): '=' changes variables; to compare, use '===' instead<br>
- [E0189](https://quick-lint-js.com/errors/E0189/): missing '&lt;>' and '&lt;/>' to enclose multiple children<br>
- [E0190](https://quick-lint-js.com/errors/E0190/): missing comparison; '===' does not extend to the right side of '||'<br>
- [E0191](https://quick-lint-js.com/errors/E0191/): event attributes must be camelCase<br>
- [E0192](https://quick-lint-js.com/errors/E0192/): attribute has wrong capitalization<br>
- [E0193](https://quick-lint-js.com/errors/E0193/): misspelled React attribute; write 'className' instead<br>
- [E0194](https://quick-lint-js.com/errors/E0194/): missing parentheses around left-hand side of `**`; `**` operator cannot be used after unary `-` without parentheses<br>
- [E0195](https://quick-lint-js.com/errors/E0195/): missing parentheses around operand of `typeof`; `typeof` operator cannot be used before `**` without parentheses<br>
- [E0196](https://quick-lint-js.com/errors/E0196/): new variable shadows existing variable<br>
- [E0197](https://quick-lint-js.com/errors/E0197/): '“' is not allowed for strings; use " instead<br>
- [E0199](https://quick-lint-js.com/errors/E0199/): unclosed class; expected '}' by end of file<br>
- [E0200](https://quick-lint-js.com/errors/E0200/): break can only be used inside of a loop or switch<br>
- [E0201](https://quick-lint-js.com/errors/E0201/): continue can only be used inside of a loop<br>
- [E0202](https://quick-lint-js.com/errors/E0202/): missing '=' after
variable<br>
- [E0203](https://quick-lint-js.com/errors/E0203/): depth limit exceeded<br>
- [E0204](https://quick-lint-js.com/errors/E0204/): error generator function star belongs after keyword function<br>
- [E0205](https://quick-lint-js.com/errors/E0205/): error missing initializer in const declaration<br>
- [E0206](https://quick-lint-js.com/errors/E0206/): label named 'await' not allowed in async function<br>
- [E0207](https://quick-lint-js.com/errors/E0207/): code point in Unicode escape sequence must not be greater than U+10FFFF<br>
- [E0208](https://quick-lint-js.com/errors/E0208/): cannot access private identifier outside class<br>
- [E0209](https://quick-lint-js.com/errors/E0209/): commas are not allowed between class methods<br>
- [E0210](https://quick-lint-js.com/errors/E0210/): unopened block comment<br>
- [E0269](https://quick-lint-js.com/errors/E0269/): 'async static' is not allowed; write 'static async' instead<br>
