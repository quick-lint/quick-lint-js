# ADR015: Logging and tracing

**Status**: Accepted and active.

## Context

Server programs need to log data for a variety of reasons, including the
following:

* errors
* general activity
* performance

Logged data needs to be consumed, either by humans directly or by analysis tools.

quick-lint-js can run as a server or something server-like. Its LSP server is a
long-running server, and its Visual Studio Code extension is also long-running.

## Decision

quick-lint-js has two mechanisms: logging and tracing.

### Logging

Debugging messages are strings which are appended to a text file. If the C++
macro `QLJS_DEBUG_LOGGING_FILE` is defined, messages are logged to the file at
that path. To write messages, call the `QLJS_DEBUG_LOG` macro.

### Tracing

quick-lint-js can write a binary log. The format is compatible with the [Common
Trace Format][] and is described by a [CTF metadata
file](../../src/trace-metadata.cpp). See the [documentation on our tracing
format](../TRACING.md).

## Consequences

### Logging

Logging to a file (rather than stdout/stderr) is useful when using vscode-test.
vscode-test often swallows data written to stdout and stderr.

Controlling logging with a macro is gross. quick-lint-js needs to be rebuilt in
order to enable or disable logging, and logging can accidentally be enabled (see
commit 149f3a64ab513ef594598bb17c751a4947aafc6c). Logging can't reasonably be
enabled for end users.

Logging text is slow and is thus avoided, even when it might be useful.

Line-oriented logs don't allow for binary data, such as bytes that might come
from an input file.

Log files aren't rotated, causing them to grow indefinitely.

Logging is straightforward and familiar to C and C++ programmers as it requires
only `printf`-style format strings.

The logging implementation is simple (as it relies on the C++ standard library
to do string formatting and file I/O).

### Tracing

To be determined.

[Common Trace Format]: https://diamon.org/ctf/
