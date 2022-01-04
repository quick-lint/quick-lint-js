# Profiling quick-lint-js

* [Speed](#run-time-speed)
  * [Benchmarks](#benchmarks)
  * [Vector profiler](#vector-profiler)
  * [Parse tracing](#parse-tracing)
* [Build size](#build-size)
* [Build speed](#build-speed)

## Run-time speed

### Benchmarks

[quick-lint-js has benchmarks](../benchmark/README.txt) for various things,
including parsing, location tracking, and LSP.

TODO: Write a script to compare two builds.

You can profile a benchmark using tools like `perf` (Linux) or Instruments
(macOS) or any other tool. Run a benchmark for a while to allow enough samples:

    $ ninja -C build quick-lint-js-benchmark-parse
    $ QLJS_PARSE_BENCHMARK_SOURCE_FILE=~/tmp/jquery-3.5.1.js \
        perf record \
        ./build/benchmark/quick-lint-js-benchmark-parse \
        --benchmark_min_time=2 \
        --benchmark_filter=benchmark_parse_file
    $ perf report

### Vector profiler

Collect and dump statistics about our usage of vectors (resizable arrays).

Current statistics:

* Max sizes: How big does a vector get? This helps decide good initial
  reservations.
* Capacity changes: How often do vector items get copied due to
  `push_back`/`emplace_back`? This helps decide whether reservations are
  working, and help tune vector growth strategies.

1. Set the `QUICK_LINT_JS_FEATURE_VECTOR_PROFILING` CMake variable to `ON`.
2. Build `quick-lint-js` (CLI).
3. When running the CLI, set the `QLJS_DUMP_VECTORS` environment variable to
   `1`.

Example output:

    Max sizes for parse_arrow_function_expression_remainder:
    0  ( 0%)
    1  (ALL)  *

    Max sizes for parse_expression_remainder call children:
    0  ( 0%)
    1  (40%)  ******
    2  (53%)  ********
    3  ( 7%)  *

    Max sizes for parse_expression_remainder children:
    0  ( 0%)
    1  (ALL)  ***************************************************

    Max sizes for parse_object_literal entries:
    0  ( 0%)
    1  ( 0%)
    2  (ALL)  *

    Max sizes for parse_template children:
    0  ( 0%)
    1  (50%)  *
    2  ( 0%)
    3  ( 0%)
    4  (50%)  *

    vector capacity changes:
    (C=copied; z=initial alloc; -=used internal capacity)
    parse_arrow_function_expression_remainder:
     0C  1z  0_ |zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz|
    parse_expression_remainder call children:
     0C 15z 10_ |zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz___________________________|
    parse_expression_remainder children:
     0C 59z  0_ |zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz|
    parse_object_literal entries:
     0C  1z  1_ |zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz_________________________________|
    parse_template children:
     0C  2z  3_ |zzzzzzzzzzzzzzzzzzzzzzzzzz________________________________________|

### Parse tracing

quick-lint-js' Visual Studio Code extension can [log parse
times](../plugin/vscode/PERFORMANCE-TRACING.md).

## Build speed

[Clang has a time
profiler](https://aras-p.info/blog/2019/01/16/time-trace-timeline-flame-chart-profiler-for-Clang/)
which helps you determine what might be slow to parse or code-gen.

Find files which are slow compile: Generate a gantt chart from Ninja (build
system) timings with [strager's gnuplot script published November 13,
2020](https://strager.net/microblog.html).

## Build size

Before and after making an optimization, see big quick-lint-js is:

    $ ninja -C build quick-lint-js && strip -o build/quick-lint-js{.stripped,} && ls -lh build/quick-lint-js*

[Bloaty profiles executable sizes](https://github.com/google/bloaty). Run it on
a `CMAKE_BUILD_TYPE=Release` build of quick-lint-js:

    $ bloaty -d symbols -n 300 build/quick-lint-js

Track quick-lint-js [build sizes over time](../tools/build-sizes/index.html).
