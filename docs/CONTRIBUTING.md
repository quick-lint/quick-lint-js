# Contributing to quick-lint-js

## Checklist

* All tests pass locally.
* On CI, all jobs succeed, including the Linux, macOS, Windows, and WASM builds.
* C++ code is formatted using [clang-format](#clang-format).

## clang-format

quick-lint-js' C++ code is formatted using the [clang-format][] tool, **version
9.0.x**. During development, use clang-format in one of two ways:

* [Install clang-format][llvm-build-9.0.0] yourself, then run the `tools/format`
  script to format all of quick-lint-js' code with one command.
* Run clang-format in your editor ([CLion][clang-format-clion],
  [Emacs][clang-format-emacs], [Vim][clang-format-vim], [Visual
  Studio][clang-format-vs], [Visual Studio Code][clang-format-vscode]). Be
  sure to use version 9.0.x, or you might have problems with the CI clang-format
  checker.

[clang-format-clion]: https://clang.llvm.org/docs/ClangFormat.html#clion-integration
[clang-format-emacs]: https://clang.llvm.org/docs/ClangFormat.html#emacs-integration
[clang-format-vim]: https://clang.llvm.org/docs/ClangFormat.html#vim-integration
[clang-format-vs]: https://clang.llvm.org/docs/ClangFormat.html#visual-studio-integration
[clang-format-vscode]: https://marketplace.visualstudio.com/items?itemName=xaver.clang-format
[clang-format]: https://clang.llvm.org/docs/ClangFormat.html
[llvm-build-9.0.0]: https://releases.llvm.org/download.html#9.0.0
