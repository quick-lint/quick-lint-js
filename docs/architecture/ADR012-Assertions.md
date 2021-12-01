# ADR012: Assertions

**Status**: Accepted and active.

## Context

Software bugs happen. [Defensive programming][] is one way to catch bugs early
and to reduce damage when bugs happen. A common defensive programming technique
is the *assertion*.

C++ has a built-in assertions library called `<cassert>` which defines a macro
called `assert`. Many C++ libraries have their own assertion macros.

## Decision

* `<quick-lint-js/assert.h>` defines custom assertion macros. `<cassert>` is
  avoided.
* Defining `NDEBUG` disables assertions, except assertions marked `ALWAYS`.
* Defining `QLJS_DEBUG` enables extra assertions.
* A default CMake build (where `CMAKE_BUILD_TYPE=None`) enables assertions.
* An optimized CMake build (where `CMAKE_BUILD_TYPE=Release`) disables
  assertions (except assertions marked `ALWAYS`).

## Consequences

Assertions can slow down execution. This used to negatively affect
quick-lint-js' Debian package (where assertions are enabled), hurting
quick-lint-js in benchmarks.

A custom macro reduces usage of `[[maybe_unused]]`.

A custom macro traps debuggers at the offending line of code, not some
`__assert_imp` or `_abort` function a few call frames down.

Assertion failures on Linux and macOS lead to SIGILL ("illegal instruction").
This confuses people.

[defensive programming]: https://en.wikipedia.org/wiki/Defensive_programming
