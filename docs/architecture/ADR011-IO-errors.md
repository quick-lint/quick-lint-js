# ADR011: IO errors

**Status**: Accepted and active.

## Context

quick-lint-js needs to interact with various system APIs, mostly filesystem
APIs. There are several possibilities for communicating these I/O errors:

* C++ exceptions or setjmp/longjmp
  * Note: [ADR008 bans C++ exceptions](ADR008-Exceptions.md)
* Variant of result and error information (e.g. std::variant; boost::outcome)
* Optional result with out-of-band error information (e.g. POSIX errno; Win32
  GetLastError(); [LEAF][])

## Decision

Functions which might fail due to an I/O error return `Result<Result, Error>` or
`Result<Result, Error, Other_Error>`. `Error` and `Other_Error` are classes
whose names end with `_IO_Error`. An `_IO_Error` class stores all information
relevant to the function (or functions it calls). `_IO_Error` objects can be
converted to a human-readable message.

## Consequences

Human-readable messages are easy to format from the member variables of
`_IO_Error` classes.

Error types need to be manually composed. A library like [LEAF][] would automate
this composition.

Error types are easy to store and compare. This matters for
`Configuration_Loader` which needs to detect when an error changes (in order for
the UI to report changes to the user). Storing errors with [LEAF][] is difficult
(and this was the reason we switched away from LEAF).

Error types can become verbose (e.g. `Result<Loaded_Config_File*,
Canonicalize_Path_IO_Error, Read_File_IO_Error, Watch_IO_Error>`).

[LEAF]: https://www.boost.org/doc/libs/1_76_0/libs/leaf/doc/html/index.html
