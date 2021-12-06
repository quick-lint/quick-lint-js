# Release notes

This document chronicles quick-lint-js' releases. This document's format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

quick-lint-js' version numbers are arbitrary. quick-lint-js does *not* adhere to
Semantic Versioning.

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
  Silicon. [See the patch
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

* Various new diagnostics (implemented by [Himanshu][] and [Matheus Sousa][]).
* VS Code: The new `quick-lint-js.logging` setting allows you to show
  quick-lint-js' internal log messages in an Output window.
* VS Code: Apple Silicon (e.g. M1) (ARM64) is now supported.
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
  "Jennipuff" Wheat][]);
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
* quick-lint-js no longer looks for files named `.quick-lint-js.config`. To
  configure quick-lint-js, name your file `quick-lint-js.config` instead.
* LSP benchmarks have been rewritten. The new benchmarks should produce more
  stable numbers and be fairer to linters with a high start-up time such as
  Flow.

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

[AidenThing]: https://github.com/AidenThing
[Amir]: https://github.com/ahmafi
[Daniel La Rocque]: https://github.com/dlarocque
[David Vasileff]: https://github.com/dav000
[Erlliam Mejia]: https://github.com/erlliam
[Himanshu]: https://github.com/singalhimanshu
[Jenny "Jennipuff" Wheat]: https://twitter.com/jennipaff
[Jimmy Qiu]: https://github.com/lifeinData
[Kim "Linden"]: https://github.com/Lindenbyte
[Lee Wannacott]: https://github.com/LeeWannacott
[Matheus Sousa]: https://github.com/keyehzy
[Nico Sonack]: https://github.com/herrhotzenplotz
[Shivam Mehta]: https://github.com/maniac-en
[coc.nvim]: https://github.com/neoclide/coc.nvim
[config-global-groups]: https://quick-lint-js.com/config/#global-groups
[tiagovla]: https://github.com/tiagovla
[wagner riffel]: https://github.com/wgrr

[E0001]: https://quick-lint-js.com/errors/#E0001
[E0020]: https://quick-lint-js.com/errors/#E0020
[E0053]: https://quick-lint-js.com/errors/#E0053
[E0057]: https://quick-lint-js.com/errors/#E0057
[E0073]: https://quick-lint-js.com/errors/#E0073
[E0094]: https://quick-lint-js.com/errors/#E0094
[E0104]: https://quick-lint-js.com/errors/#E0104
[E0106]: https://quick-lint-js.com/errors/#E0106
[E0108]: https://quick-lint-js.com/errors/#E0108
[E0110]: https://quick-lint-js.com/errors/#E0110
[E0111]: https://quick-lint-js.com/errors/#E0111
[E0119]: https://quick-lint-js.com/errors/#E0119
[E0144]: https://quick-lint-js.com/errors/#E0144
[E0173]: https://quick-lint-js.com/errors/#E0173
[E0179]: https://quick-lint-js.com/errors/#E0179
[E0205]: https://quick-lint-js.com/errors/#E0205
