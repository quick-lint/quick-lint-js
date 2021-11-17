# LSP server benchmarks

This directory contains a tool which measures the speed of LSP servers.

## Setup

Install the LSP servers you want to benchmark:

* **Deno**: Install [Deno][]. Ensure the `deno` command is in `$PATH`.
* **ESLint**: Run `npm install` in the `eslint/` directory.
* **Flow**: Run `npm install` in the `flow/` directory.
* **RSLint**: Install [RSLint's rslint_lsp crate][install-rslint]. Ensure the
  `rslint-lsp` command is in `$PATH`.
* **TypeScript**: Run `npm install` in the `typescript/` directory.
* **quick-lint-js**: Install quick-lint-js. Ensure the `quick-lint-js` command
  is in `$PATH`.

## Building

Install a compiler which supports C++20 coroutines (such as Clang version 12).
[Configure quick-lint-js using CMake with
`-DQUICK_LINT_JS_ENABLE_BENCHMARKS=YES`](../../docs/BUILDING.md). Build the
`quick-lint-js-benchmark-lsp-servers` target. For example, on Linux:

    $ mkdir build
    $ cd build
    $ CC=clang-12 CXX=clang++-12 CXXFLAGS=-stdlib=libc++ cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DQUICK_LINT_JS_ENABLE_BENCHMARKS=YES ..
    $ cd ..
    $ ninja -C build quick-lint-js-lsp-benchmark-servers

## Running

Customize `benchmark-config.json` with the LSP servers you want to benchmark.

Print a list of benchmarks:

    $ ./build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers benchmark/benchmark-lsp/benchmark-config.json --list
    eslint-server/open-wait-close/tiny.js
    eslint-server/open-wait-close/edex-ui-filesystem.class.js
    eslint-server/open-wait-close/express-router.js
    eslint-server/change-wait/tiny.js
    [snip]
    TypeScript/incremental-change-wait/express-router.js
    TypeScript/full-change-wait/express-router.js

Run the benchmarks and generate a JSON file for later processing:

    $ ./build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers --output-json results.json benchmark/benchmark-lsp/benchmark-config.json

Debug a single benchmark with an *N* of 5:

    $ QLJS_BENCHMARK_LSP_DEBUG=1 ./build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers --iterations 5 benchmark/benchmark-lsp/benchmark-config.json RSLint/change-wait/tiny.js

## Benchmarks

### change-wait

Setup (untimed):

1. Start the LSP server.
2. Wait for initialization to finish.
3. Open *N* documents with empty contents.
4. Wait for diagnostics (if applicable).

Work (timed):

1. Repeat the following steps once for each opened document serially:
   1. Change the document's contents to the contents of *file*.
   2. Wait for diagnostics.

### incremental-change-wait

Setup (untimed):

1. Start the LSP server.
2. Wait for initialization to finish.
3. Open one document with contents from *file*.
4. Wait for diagnostics.

Work (timed):

1. Repeat the following steps *N* times:
   1. Change a few characters in the document with an incremental LSP message.
   2. Wait for diagnostics.

### full-change-wait

Setup (untimed):

1. Start the LSP server.
2. Wait for initialization to finish.
3. Open one document with contents from *file*.
4. Wait for diagnostics.

Work (timed):

1. Repeat the following steps *N* times:
   1. Change a few characters in the document, sending the entire new document
      in an LSP message.
   2. Wait for diagnostics.

### open-wait-close

Setup (untimed):

1. Start the LSP server. 
2. Wait for initialization to finish.

Work (timed):

1. Repeat the following steps *N* times serially:
   1. Open a document with contents to the contents of *file*.
   2. Wait for diagnostics.
   3. Close the opened document.

[Deno]: https://deno.land/
[Stack]: https://haskellstack.org/
[install-rslint]: https://rslint.org/guide/
