# Fuzz-testing quick-lint-js

This document describes quick-lint-js' self fuzz-testing for contributors.

Instructions in this document are written for Linux and macOS users.
Fuzz-testing might work on Windows, but it hasn't been tested.

## libFuzzer-based tests

quick-lint-js' fuzz tests were built with [libFuzzer][] in mind and have only
been tested with libFuzzer.

### Building fuzz tests

When running CMake to configure quick-lint-js, do the following:

1. Use Clang to compile (not GCC or MSVC).
2. At a minimum, compile with the `-fsanitize=fuzzer-no-link` flag.
3. Set the `QUICK_LINT_JS_ENABLE_LLVM_LIBFUZZER_TESTS` CMake variable to `ON`.

Compiling with `-fsanitize=address,undefined` (to catch undefined behavior) and
`-UNDEBUG` (to enable assertions) is also recommended.

For example:

    $ mkdir build-fuzz
    $ cd build-fuzz
    $ CC=clang CXX=clang++ CFLAGS='-fsanitize=address,undefined,fuzzer-no-link' CXXFLAGS='-fsanitize=address,undefined,fuzzer-no-link' \
         cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DQUICK_LINT_JS_ENABLE_LLVM_LIBFUZZER_TESTS=ON ..
    $ cd -

Then, build the fuzz test executables:

    $ ninja -C build-fuzz
    $ ls build-fuzz/fuzz
    CMakeFiles                             quick-lint-js-fuzz-parse
    cmake_install.cmake                    quick-lint-js-fuzz-utf-8-decode-encode
    quick-lint-js-fuzz-lex                 quick-lint-js-fuzz-utf-8-lsp
    quick-lint-js-fuzz-options

### Running fuzz tests

Create a directory to contain the fuzz corpus files. I recommend one directory
per fuzz test:

    $ mkdir -p fuzz-corpus/quick-lint-js-fuzz-lex/
    $ mkdir -p fuzz-corpus/quick-lint-js-fuzz-options/
    $ mkdir -p fuzz-corpus/quick-lint-js-fuzz-parse/
    $ mkdir -p fuzz-corpus/quick-lint-js-fuzz-utf-8-decode-encode/
    $ mkdir -p fuzz-corpus/quick-lint-js-fuzz-utf-8-lsp/

Run a fuzz-test executable, giving it the corpus directory as an argument:

    $ ./build-fuzz/fuzz/quick-lint-js-fuzz-lex fuzz-corpus/quick-lint-js-fuzz-lex/

For usage information, see [libFuzzer's documentation][libFuzzer] or run
`./build-fuzz/fuzz/quick-lint-js-fuzz-lex -help=1`.

[libFuzzer]: https://www.llvm.org/docs/LibFuzzer.html
