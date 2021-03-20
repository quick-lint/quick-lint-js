# ADR003: Vendor sources

**Status**: Accepted and active.

## Context

The quick-lint-js project demands functionality which can be outsourced to third
parties. Using external libraries and programs is common in C++, but there are
many strategies for referencing and using these dependencies.

## Decision

Run-time dependencies are extracted from source code release archives into the
`vendor` directory. For each dependency, `vendor/README.txt` documents the name
of the dependency, precisely where it was extracted to, usage licensing and
copyright, where it was downloaded from, and other information.

Run-time dependencies are integrated with quick-lint-js' CMake build system
using `add_subdirectory` if possible.

Compile-time dependencies are disallowed aside from the following:

* compiler toolchain (compiler, linker, archiver, etc.)
* build system (CMake, Ninja, Visual Studio, Xcode, etc.)
* source control system (Git, HTTPS client, etc.)

Development tool dependencies are discouraged, but are allowed for some files.
See [ADR004](ADR004-Generated-sources.md) for details.

## Consequences

quick-lint-js can use API-unstable software, such as simdjson, without being
tied to the versions supported by popular distributions (like Homebrew or
Debian).

We can patch bugs found in third-party dependencies without waiting for a formal
release. (`googletest-result-of.patch` is a good example of this.)

In most cases, build instructions are simple for contributors.

Including third-party project source code bloats the Git repository, slowing
down fresh clones of the repository. The extra code inflates the line-of-code
metrics on GitHub.

Because code bloat can be a problem with Git repositories, lots of engineering
time has been spent reducing the size of vendor projects (especially Boost).
This complicates maintenance. For example, because of the work done to reduce
Boost's footprint, upgrading Boost is more complicated than extracting the
new source archive.

Some projects have partially broken or hostile CMake build systems which
sometimes need to be worked around with hacks of our own.

Some people might be confused by the term `vendor`. Perhaps `third-party` would
be a better name.

The structure of `vendor/README.txt` makes it a bit easier to generate copyright
documents for release.
