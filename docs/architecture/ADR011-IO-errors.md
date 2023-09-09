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

Functions which might fail due to an I/O error return `Result<Result, Error>`
where `Error` is some class whose names end with `_IO_Error`. An `_IO_Error`
class stores all information relevant to the function (or functions it calls).
`_IO_Error` objects can be converted to a human-readable message.

## Consequences

Human-readable messages are easy to format from the member variables of
`_IO_Error` classes.

Error types are easy to store and compare. This matters for
`Configuration_Loader` which needs to detect when an error changes (in order for
the UI to report changes to the user). Storing errors with [LEAF][] is difficult
(and this was the reason we switched away from LEAF).

Errors need to be converted. For example, `Read_File_IO_Error` and
`Canonicalize_Path_IO_Error` need to be converted to
`Configuration_Load_IO_Error`.

* `Result<>::propagate` became a natural mechanism to perform conversions, and
  this turned out to be simpler and cleaner than expected.
* Error type conversion can lead to quirks such as
  `Configuration_Load_IO_Error::canonicalizing_path` being only sometimes used.
* Error type "hierarchies" need to be constructed manually. A library like
  [LEAF][] would automate this composition.

Some error types contain extra information such as the path which errored. This
information is used in tests to make sure certain error conditions occur.
However, this complicates errors slightly just for testing.

[LEAF]: https://www.boost.org/doc/libs/1_76_0/libs/leaf/doc/html/index.html
