# Benchmarks

This directory contains benchmark results for display on the website.

## Generating

Follow the [benchmark-lsp build
instructions](../../../benchmark/benchmark-lsp/README.md). Then run the
benchmarks:

    $ ./build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers \
      --iterations 10 \
      --samples 10 \
      --output-json website/public/benchmarks/open-wait-close-express-router-js.json \
      benchmark/benchmark-lsp/benchmark-config.json \
      open-wait-close/express-router.js
