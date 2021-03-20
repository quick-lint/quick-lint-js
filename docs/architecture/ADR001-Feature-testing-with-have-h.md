# ADR001: Feature testing with have.h

**Status**: Accepted and active.

## Context

Different operating systems support different APIs. Different compilers support
different language features. Sometimes, implementations are broken and require
workarounds.

quick-lint-js is a portable software project. quick-lint-js compiles with
different compilers and for different operating systems. Parts of quick-lint-js
need to react to differences and quirks of the compilation environment.

Traditionally, C++ programs use *feature test macros* to control how they use
operating system and language features. Projects have different mechanisms for
exposing these feature test macros to the C++ compiler.

## Decision

`<quick-lint-js/have.h>` defines macros with names starting with `QLJS_HAVE_`. A
macro is defined with value `1` if a feature is likely present and working, and
is defined with a value of `0` if a feature is likely absent or broken.

The value of `QLJS_HAVE_` macros are decided by checks using the C++
preprocessor, such as `#if __has_include` and `#if defined(__unix__)`.

All `QLJS_HAVE_` macros are always defined, regardless of whether the macro is
used or whether the feature is present. As a corollary, uses of `QLJS_HAVE_`
macros use `#if`, never `#ifdef`.

If a `QLJS_HAVE_` macro is defined before `<quick-lint-js/have.h>` is included,
its original value is preserved; including `<quick-lint-js/have.h>` has no
effect on that macro.

## Consequences

Testing exclusively in C++ means that the build system can be simpler. For
example, on major platforms with recent compilers, all of quick-lint-js can be
compiled with a few basic compilation command lines with no explicit macro
definitions.

If a `QLJS_HAVE_` macro's checks are incorrect in `<quick-lint-js/have.h>`, a
porter/packager can define the macro with its correct value in the build system
without patching quick-lint-js' source code

The C++ preprocessor code checks to decide the value of a `QLJS_HAVE_` macro
might be error prone or downright incorrect.

In practice, some `QLJS_HAVE_` macros need to be configured in the build system
because the C++ preprocessor is not powerful enough. `QLJS_HAVE_CHARCONV_HEADER`
is one example.
