# ADR019: I/O manager

**Status**: Accepted and active.

## Context

Applications, and especially servers, need to perform I/O. They read and write
files, use IPC channels to talk to other programs, and listen to filesystem
changes. For example, quick-lint-js's LSP server needs to use I/O to talk to the
LSP client.

There are several ways to do I/O, and the details very depending on the
operating system. The following are few common arrangements:

* Synchronous I/O; no overlapping
* Synchronous I/O overlapped using threads
* Asynchronous I/O with one or more event pump threads

Sometimes, these are mixed. Most of the program might use asynchronous I/O with
event pumps, but the beginning of the program might read configuration files
using synchronous I/O.

## Decision

For LSP client-to-server and server-to-client IPC:
* macOS: non-blocking pipes; kqueue event loop (`Event_Loop_Kqueue`)
* Linux: non-blocking pipes; poll() event loop (`Event_Loop_Poll`)
* Windows: blocking pipes with a thread for ReadFile
  (`Event_Loop_Windows::Registered_Pipe_Read`) and another thread for WriteFile
  (`Background_Thread_Pipe_Writer`)

For filesystem notifications:
* macOS: kqueue event loop (`Event_Loop_Kqueue`)
* Linux: inotify with a poll() event loop (`Event_Loop_Poll`)
* Windows: oplocks with an I/O completion port event loop (`Event_Loop_Windows`)

An `Event_Loop` class centralizes the aforementioned IPC and filesystem
notifications and abstracts over some platform differences. The event loop can
be stopped arbitrarily. Events are registered by attaching callbacks, thus
different users can listen for different events.

For file I/O, use synchronous file I/O with no overlapping.

## Consequences

The `Event_Loop` abstraction made it relatively easy to use
`Change_Detecting_Filesystem` implementations in different environments: the LSP
server (where we have full control) and the Visual Studio Code extension (where
we need to avoid stepping on the Node.js event loop).

Supporting different methods of I/O for different operating systems is complex.
The current solution is leaning toward bankruptcy; using the `Event_Loop`
abstraction requires a lot of platform-specific code (via `#if`), making the
code hard to follow. Some of the caller complexity is necessary, but there is
much to be improved in this area. In particular, `Event_Loop`'s interaction with
`Pipe_Writer` is a portability problem.

`Event_Loop` does not handle all IPC. `Pipe_Writer` handles the write part on
Windows, and it is littered with `#if` too.

Synchronous file I/O with no overlapping is problematic when quick-lint-js is
embedded. For example, in the Visual Studio Code extension, synchronous file I/O
occurs on the Node.js main thread, preventing other extensions from executing
while the file I/O occurs. This is problematic for users who use network drives,
for example. As another example, synchronous file I/O cannot be modelled in a
web browser, thus quick-lint-js cannot run in Codespaces. (Codespaces support
would [give quick-lint-js a competitive
advantage](https://github.com/quick-lint/quick-lint-js/issues/417) over ESLint.)
