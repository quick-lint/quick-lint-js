# Benchmarks

This directory contains benchmark results for display on the website.

## Generating

Follow the [benchmark-lsp build
instructions](../../../benchmark/benchmark-lsp/README.md). Then run the
benchmarks:

    $ ./build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers \
      --iterations 10 \
      --samples 10 \
      --output-json website/public/benchmarks/full-change-wait-express-router-js.json \
      benchmark/benchmark-lsp/benchmark-config.json \
      full-change-wait/express-router.js

    $ ./build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers \
      --iterations 10 \
      --samples 10 \
      --output-json website/public/benchmarks/incremental-change-wait-express-router-js.json \
      benchmark/benchmark-lsp/benchmark-config.json \
      incremental-change-wait/express-router.js
