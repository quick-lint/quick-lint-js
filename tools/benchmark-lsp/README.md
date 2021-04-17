# LSP server benchmarks

This directory contains a tool which measures the speed of LSP servers.

## Setup

Install the LSP servers you want to benchmark:

* **Deno**: Install [Deno][]. Ensure the `deno` command is in `$PATH`.
* **ESLint**: Run `npm install` in the `eslint/` directory.
* **Flow**: Run `npm install` in the `flow/` directory.
* **Rome**: Run `npm install` in the `rome/` directory.
  * Also, in `benchmark-config.json`, change `"enable": false` to
    `"enable": true` for Rome.
* **RSLint**: Install [RSLint's rslint_lsp crate][install-rslint]. Ensure the
  `rslint-lsp` command is in `$PATH`.
* **TypeScript**: Run `npm install` in the `typescript/` directory.
* **quick-lint-js**: Install quick-lint-js. Ensure the `quick-lint-js` command
  is in `$PATH`.

## Building

Install [Stack][], then run the following command:

    $ stack build

## Running

Customize `benchmark-config.json` with the LSP servers you want to benchmark.

Print a list of benchmarks:

    $ stack run -- --list
    Deno/change-wait/edex-ui-filesystem.class.js
    Deno/change-wait/express-router.js
    ...
    quick-lint-js/open-wait-close/express-router.js
    quick-lint-js/open-wait-close/tiny.js

Run the benchmarks and generate an HTML report with graphs:

    $ stack run -- --output benchmark-report.html

Debug a single benchmark with an *N* of 5:

    $ QLJS_BENCHMARK_LSP_DEBUG=1 stack run -- --iters 5 RSLint/change-wait

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
