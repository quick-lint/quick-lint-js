# ADR017: Lexer tables

**Status**: Rejected.

## Context

There are a few approches to writing lexers for programming language source
code:

* Manual `if`/`switch` statements
* Table-based lexer generators such as [flex][]
* Hybrid table+`if`/`switch` lexer

quick-lint-js needs to be fast, thus it should be written using the fastest
lexing approach.

## Decision

Write a hybrid table+`if`/`switch` lexer by hand:

* Symbols such as `!=` and `&&` are recognized using state machine tables.
* Identifiers, numbers, strings, etc. are detected by looking at the first
  character. Parsing for these things is done by hand (e.g. using SIMD), not
  using state machine tables.

## Consequences

Parsing performance was at least 1.2% slower compared to a manual `switch` lexer
on several compilers on several architectures.

The lexer state machine tables were much harder to read and modify compared to
plain C++ `switch` statements.

[flex]: https://github.com/westes/flex
