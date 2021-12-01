# ADR013: Dependencies

**Status**: Accepted and active.

## Context

Rarely does a project develop all of its own tools from scratch. Projects depend
on compilers, interpreters, libraries, and operating systems written by others.

quick-lint-js might want to rely on other tools for several needs.

## Decision

quick-lint-js has three categories of dependencies: in-build, out-build, and
dev.

In-build dependencies are necessary to build and run quick-lint-js, but are
provided in quick-lint-js' source distribution. These dependencies are written
in C or C++ (see [ADR002](ADR002-Programming-language.md)) and exist in the
`vendor/` directory of the repository (see [ADR003](ADR003-Vendor-sources.md)).

Out-build dependencies are necessary to build and run quick-lint-js, but are
*not* provided in quick-lint-js' source distribution. The following out-build
dependencies are allowed:

* CMake and a supported build system
* C++ compiler and standard library
* Python 3
* Tooling to download quick-lint-js' code, such as Git or curl+tar+gunzip

Dev dependencies are *not* necessary to build and run quick-lint-js, and are
*not* provided in quick-lint-js' source distribution. The following is an
incomplete list of dev dependencies:

* Asciidoctor
* Bash
* Go (programming language)
* Node.js and npm
* Packages installed through npm
* clang-format
* cloc

## Consequences

Keeping the list of out-build dependencies small makes it easier for new
developers to start developing quick-lint-js.

Keeping the list of out-build dependencies small and using only
industry-standard tools makes it easier to package quick-lint-js in software
distributions.

Limiting out-build dependencies means generated sources need to be checked into
the source repository. This has negative consequences, generated sources getting
out of date with their source code. See [ADR004](ADR004-Generated-sources.md)
for details.

Mixing programming languages in dev dependencies reduces code reuse. This has
only been a problem for a few things so far (e.g. Python check-esprima vs Go
check-test262

Allowing best-tool-for-the-job in dev dependencies is sometimes a good thing.
For example, check-test262.go was originally written in Python, but it was too
slow. A prototype in Node.js was also too slow. The Go version was nice and
fast.

Web developers are likely familiar with Node.js and npm, so the website backend
being written in Node.js makes it easier to for new contributors to the website
to get up and running.

We have a few "soft" out-build dependencies. `test/test-lex-unicode.cpp`
requires the third-party icu4c library, but if icu4c is missing, that test file
is not compiled. This is kinda gross, and means we don't get full testing
everywhere.
