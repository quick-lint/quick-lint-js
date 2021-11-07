# Benchmarks

This directory contains benchmark results for display on the website.

## Generating

    $ # Run the benchmarks:
    $ cd benchmark/benchmark-lsp/
    $ stack run -- -m glob "*/open-wait-close/express-router.js" --json ../../website/public/benchmarks/open-wait-close-express-router-js.json --time-limit 20
