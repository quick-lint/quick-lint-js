# ADR005: Incremental lexing

**Status**: Accepted and active.

## Context

Parsers for programming languages are generally split into two phases: lexing
and parsing. There are various strategies for how lexing and parsing are
interleaved, and each has its own tradeoffs.

## Decision

Lexing is done one token at a time. The parser requests that the lexer feed the
parser a new token, and to satisify that request, the lexer lexes the token (and
only that token).

## Consequences

The lexer doesn't need much state. It doesn't need to know the depth of template
literals. It doesn't need to know whether a `/` should be interpreted as a
division operator or the start of a regular expression literal.

The lexer's interface is non-trivial. The parser needs to call different lexing
functions depending on the state of the parser (`skip`; `skip_in_template`;
`reparse_as_regexp`; `insert_semicolon`).

Backtracing the lexer results in re-lexing. Therefore, backtracking is
discouraged in the parser for performance reasons.

The CPU switches between lexing and parsing frequently. This might have a
negative impact on performance, especially for CPUs with smaller icaches.
