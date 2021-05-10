# Benchmarks

This directory contains benchmark results for display on the website.

## Generating

    $ # Run the benchmarks:
    $ cd benchmark/benchmark-lsp/
    $ stack run -- -m glob "*/open-wait-close/express-router.js" --json ../../website/benchmarks/open-wait-close-express-router-js.json --time-limit 20
    $ cd ../../

    $ # Generate index.html:
    $ python3 ./benchmark/benchmark-lsp/generate-benchmark-html website/benchmarks/index.template.html website/benchmarks/index.html

    $ # Reformat HTML code:
    $ cd website/
    $ yarn fmt
