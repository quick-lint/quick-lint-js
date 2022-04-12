# quick-lint-js benchmarks

This directory contains microbenchmarks which measure quick-lint-js'
performance.

The benchmark-lsp subdirectory contains benchmarks which measure the performance
of different LSP servers, including quick-lint-js'.

## Running microbenchmarks

Set the `QLJS_PARSE_BENCHMARK_SOURCE_FILE` and `QLJS_LINT_BENCHMARK_SOURCE_FILE`
environment variables to the path of a longish JavaScript file, such as
[jquery-3.5.1.js](https://code.jquery.com/jquery-3.5.1.js):

    # Bash
    $ export QLJS_PARSE_BENCHMARK_SOURCE_FILE=jquery-3.5.1.js
    $ export QLJS_LINT_BENCHMARK_SOURCE_FILE=jquery-3.5.1.js

    # PowerShell
    PS> set $env:QLJS_PARSE_BENCHMARK_SOURCE_FILE = 'jquery-3.5.1.js'
    PS> set $env:QLJS_LINT_BENCHMARK_SOURCE_FILE = 'jquery-3.5.1.js'

Then run the `quick-lint-js-benchmark-` from the build directory. For example:

    $ ./build-perf/benchmark/quick-lint-js-benchmark-lint \
        --benchmark_min_time=1 \
        --benchmark_repetitions=3 \
        --benchmark_counters_tabular=true \
        --benchmark_filter=benchmark_parse_and_lint

## Comparing benchmarks before and after

After making a change, you might want to compare the performance of the builds
before and after the change. When doing this, you must keep a few things in
mind:

* The length of the command line, including the executable path, can affect
  benchmark results.
* CPU scheduling can affect benchmark results.

To compare:

1. Run the unpatched benchmark executable with
   `--benchmark_out_format=json --benchmark_out=before.json`.
2. Run the patched benchmark executable with
   `--benchmark_out_format=json --benchmark_out=after0.json`. (The `0` pads the
   command line to have the same length as when running the unpatched
   benchmark.)
3. Run `python vendor/benchmark/tools/compare.py benchmarks before.json
   after0.json`.

Here's the script strager uses to compare benchmarks on Linux:

    #!/usr/bin/env bash
    # Example usage:
    # ./bench parse --benchmark_filter=benchmark_parse_file --benchmark_repetitions=10
    set -e
    set -u

    benchmark_name="${1}"
    shift

    export QLJS_PARSE_BENCHMARK_SOURCE_FILE=jquery-3.5.1.js
    export QLJS_LINT_BENCHMARK_SOURCE_FILE=jquery-3.5.1.js
    ninja -C build-perf quick-lint-js-benchmark-"${benchmark_name}"
    cp ./build-perf/benchmark/quick-lint-js-benchmark-"${benchmark_name}" ./quick-lint-js-benchmark-"${benchmark_name}".after0

    taskset 4 ./quick-lint-js-benchmark-"${benchmark_name}".before --benchmark_min_time=1 --benchmark_repetitions=3 --benchmark_counters_tabular=true --benchmark_out_format=json --benchmark_out=before.json "${@}"
    taskset 4 ./quick-lint-js-benchmark-"${benchmark_name}".after0 --benchmark_min_time=1 --benchmark_repetitions=3 --benchmark_counters_tabular=true --benchmark_out_format=json --benchmark_out=after0.json "${@}"
    nix-shell -p 'python3.withPackages (pythonPackages: [ pythonPackages.scipy ])' --run 'vendor/benchmark/tools/compare.py benchmarks before.json after0.json'

Some notes:

* strager uses `taskset` to pin the benchmark to CPU 2 ((1<<2) == 4).
* strager renames the before and after benchmark executables so they have the
  same length.
* strager compares benchmark results using google-benchmark's `compare.py`
  script. He installs Python and SciPy using the Nix package manager. (You can
  use whatever package manager you want, of course.)
