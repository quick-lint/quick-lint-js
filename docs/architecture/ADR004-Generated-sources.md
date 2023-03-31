# ADR004: Generated sources

**Status**: Accepted and active.

## Context

Sometimes, source code needs to be compiled with tools not typically part of a
language's toolchain. For example, Flex compiles .l files into .c files, and
those .c files should be compiled with the normal C toolchain.

In this document, we refer to these tools as *unorthodox tools*, their inputs as
*unorthodox sources* or *unorthodox source files*, and their corresponding
outputs as *unorthodox artifacts*.

In a perfect world, unorthodox tools could either be avoided entirely or could
integrate seamlessly with build systems. In the real world, tradeoffs must be
made.

## Decision

Unorthodox artifacts (in addition to unorthodox sources) are checked into the
source code repository.

Each unorthodox artifact must include the word 'generated' in its file name.

Each unorthodox artifact must mention in a comment at the top of the file that
the file is generated, which unorthodox tool generated the file, and what the
unorthodox sources were.

The set of unorthodox source files is kept small. Most changes a typical
contributor might make do not involve those source files.

Unorthodox tools are not included in the `vendor` directory (see
[ADR003](docs/architecture/ADR003-Vendor-sources.md)).

## Consequences

No unorthodox tools is required in order to build quick-lint-js. Unorthodox
tools are only required when changing certain source files.

It's clear whether a file which is checked into the repository is generated. A
contributor might modify an unorthodox artifact, but there are several
indications that this is a bad idea.

Different versions of unorthodox tools can produce different artifacts given the
same sources. This can cause thrash it Git logs if two developers trade changes
to sources but use different different tool versions.

The set of unorthodox sources turned out to be larger than anticipated. For
example, *all* sources are potentially unorthodox sources for the translation
tools, and the often-changed `diagnostic-types.h` file is definitely an
unorthodox source.

It's easy for unorthodox artifacts to get out of sync with their unorthodox
sources. This is clearly true for translation files (.po).

Generation scripts are written in various languages. Some languages are a bit
hostile to certain platforms, such as POSIX shell on Windows.
