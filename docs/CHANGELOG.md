# Release notes

This document chronicles quick-lint-js' releases. This document's format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

quick-lint-js' version numbers are arbitrary. quick-lint-js does *not* adhere to
Semantic Versioning.

## Unreleased

### Added

* [E0144][] and [E0145][] are now reported for `implements`, `interface`,
  `package`, `private`, `protected`, and `public` (in addition to other
  keywords).

### Fixed

* [E0150][] no longer crashes quick-lint-js when using a German (`de`) locale.
  (Fixed by [Nico Sonack][].)
* The npm package now includes `copyright` files listing software licenses and
  copyright notices.

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

[AidenThing]: https://github.com/AidenThing
[Amir]: https://github.com/ahmafi
[Christian Mund]: https://github.com/kkkrist
[Daniel La Rocque]: https://github.com/dlarocque
[Dave Churchill]: https://www.cs.mun.ca/~dchurchill/
[David Vasileff]: https://github.com/dav000
[Erlliam Mejia]: https://github.com/erlliam
[Himanshu]: https://github.com/singalhimanshu
[Jenny "Jennipuff" Wheat]: https://twitter.com/jennipaff
[Jimmy Qiu]: https://github.com/lifeinData
[Kim "Linden"]: https://github.com/Lindenbyte
[Lee Wannacott]: https://github.com/LeeWannacott
[Matheus de Sousa]: https://github.com/keyehzy
[Nico Sonack]: https://github.com/herrhotzenplotz
[Piotr Dąbrowski]: https://github.com/yhnavein
[Rob Miner]: https://github.com/robminer6
[Sarah Schulte]: https://github.com/cgsdev0
[Shivam Mehta]: https://github.com/maniac-en
[Tony Sathre]: https://github.com/tonysathre
[coc.nvim]: https://github.com/neoclide/coc.nvim
[config-global-groups]: https://quick-lint-js.com/config/#global-groups
[install-powershell-completions]: https://github.com/quick-lint/quick-lint-js/blob/master/completions/README.md#powershell
[tiagovla]: https://github.com/tiagovla
[wagner riffel]: https://github.com/wgrr

[E0001]: https://quick-lint-js.com/errors/E0001/
[E0003]: https://quick-lint-js.com/errors/E0003/
[E0013]: https://quick-lint-js.com/errors/E0013/
[E0016]: https://quick-lint-js.com/errors/E0016/
[E0019]: https://quick-lint-js.com/errors/E0019/
[E0020]: https://quick-lint-js.com/errors/E0020/
[E0026]: https://quick-lint-js.com/errors/E0026/
[E0036]: https://quick-lint-js.com/errors/E0036/
[E0038]: https://quick-lint-js.com/errors/E0038/
[E0040]: https://quick-lint-js.com/errors/E0040/
[E0053]: https://quick-lint-js.com/errors/E0053/
[E0054]: https://quick-lint-js.com/errors/E0054/
[E0057]: https://quick-lint-js.com/errors/E0057/
[E0060]: https://quick-lint-js.com/errors/E0060/
[E0073]: https://quick-lint-js.com/errors/E0073/
[E0094]: https://quick-lint-js.com/errors/E0094/
[E0104]: https://quick-lint-js.com/errors/E0104/
[E0106]: https://quick-lint-js.com/errors/E0106/
[E0108]: https://quick-lint-js.com/errors/E0108/
[E0110]: https://quick-lint-js.com/errors/E0110/
[E0111]: https://quick-lint-js.com/errors/E0111/
[E0119]: https://quick-lint-js.com/errors/E0119/
[E0144]: https://quick-lint-js.com/errors/E0144/
[E0145]: https://quick-lint-js.com/errors/E0145/
[E0150]: https://quick-lint-js.com/errors/E0150/
[E0151]: https://quick-lint-js.com/errors/E0151/
[E0173]: https://quick-lint-js.com/errors/E0173/
[E0176]: https://quick-lint-js.com/errors/E0176/
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
