# Release notes

This document chronicles quick-lint-js' releases. This document's format is
based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

quick-lint-js' version numbers are arbitrary. quick-lint-js does *not* adhere to
Semantic Versioning.

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
* `for (const x of xs)` no longer incorrectly reports E205 (missing initializer
  in const declaration) (fixed by [Himanshu][])
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
[Daniel La Rocque]: https://github.com/dlarocque
[David Vasileff]: https://github.com/dav000
[Erlliam Mejia]: https://github.com/erlliam
[Himanshu]: https://github.com/singalhimanshu
[Jimmy Qiu]: https://github.com/lifeinData
[Kim "Linden"]: https://github.com/Lindenbyte
[Lee Wannacott]: https://github.com/LeeWannacott
[Matheus Sousa]: https://github.com/keyehzy
[Shivam Mehta]: https://github.com/maniac-en
[config-global-groups]: https://quick-lint-js.com/config/#global-groups
[tiagovla]: https://github.com/tiagovla
[wagner riffel]: https://github.com/wgrr
