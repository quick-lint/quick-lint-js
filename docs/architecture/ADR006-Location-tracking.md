# ADR006: Location tracking

**Status**: Accepted and active.

## Context

In order to provide good diagnostics, compilers report *where* in the source
code an error occurs. Editors consume this location information in order to
highlight issues for the user.

In addition to reporting diagnostics, incremental compilers (e.g. LSP servers)
need to consume location information given by editors when the user makes
changes to their source code.

Different editors use different schemes for communicating locations. Some
editors use byte offsets, and others use line-and-column-number. Editors which
use line and column numbers use different methods for counting lines (e.g. first
line is 0 or 1; lines end in CR/LF/CRLF or maybe some other sequences) and
counting columns.

## Decision

The lexer does not count line or column numbers. The lexer only communicates a
pointer inside the input string (effectively a byte offset).

Different `_locator` classes are used to implement different rules used by
different editors. Each `_locator` class is independent and has a different
interface suited for the protocol. (A CLI `_locator` class has different needs
from an LSP server `_locator` class.)

When reporting an error, it is the responsibility of an `error_reporter` derived
class to give the input string pointer (i.e. byte offset) to the `_locator`.

## Consequences

For the CLI, if source code has no errors, location information is not needed,
thus quick-lint-js avoids redundant work.

The `_locator` system came in handy when implementing the LSP server. When
applying line-number-based edits from the LSP client, line numbers don't need to
be recomputed in order to find which byte to edit in the input string.

Different `_locator` classes can coexist while reusing the same lexer code. The
lexer doesn't need a polymorphic (e.g. template parameter) reference to any
state to keep track of line numbers.

Passing around location-aware tokens involves just passing two pointers (start
and end). This representation is pretty compact compared to a bunch of integers.
For identifiers, we need the pointers anyway (though Unicode escapes in
identifiers tarnish the utopia of only needing two pointers).

If multiple diagnostics need to be reported, then the `_locator` classes need
caching to avoid scanning the input multiple times. This caching is hard to get
right, and has lead to several bugs (e.g. Git commits
e33b0a4ed72d236ccee9615ba42978ee72a92aff and
507bbfa75802bd0ec71d91f8c6bfec94dc48116e).
