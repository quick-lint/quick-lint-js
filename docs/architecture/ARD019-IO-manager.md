# ADR019: I/O manager

**Status**: Accepted and pending migration (`Event_Loop` -> `Event_Loop2`).

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
* macOS: non-blocking pipes; kqueue event loop (`Kqueue_Event_Loop`)
* Linux: non-blocking pipes; poll() event loop (`Poll_Event_Loop`)
* Windows: blocking pipes with a thread for ReadFile (`Windows_Event_Loop`) and
  another thread for WriteFile (`Background_Thread_Pipe_Writer`)

For filesystem notifications:
* macOS: kqueue event loop (`Kqueue_Event_Loop`)
* Linux: inotify with a poll() event loop (`Poll_Event_Loop`)
* Windows: oplocks with an I/O completion port event loop (`Windows_Event_Loop`)

An old `Event_Loop` class centralizes the aforementioned IPC and filesystem
notifications and abstracts over some platform differences. Some parts of
`Event_Loop`, such as write-ready pipe notification and filesytem notification,
are optional, enabling code reuse. `Event_Loop` accomplishes optionality by
periodically polling a CRTP derived class for which events to listen to.

A new `Event_Loop2` class centralizes the aforementioned IPC and filesystem
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
The current solution is halfway toward bankruptcy; using the `Event_Loop`
abstraction requires a lot of platform-specific code (via `#if`), making the
code hard to follow. Some of the caller complexity is necessary, but there is
much to be improved in this area. The new `Event_Loop2` abstraction should make
this better, but `Event_Loop2`'s interaction with `Pipe_Writer` is still a
portability problem.

`Event_Loop` does not handle all IPC. `Pipe_Writer` handles the write part on
Windows, and it is littered with `#if` too.

Regarding `Event_Loop`'s
`get_readable_pipe`/`get_pipe_write_fd`/`get_inotify_fd` design of periodically
polling a CRTP derived class for events to listen to: It has been the source of
basic (but not immediately detected) bugs, mostly because this optionality was
tacked onto the design after the fact. (The most [egregeous
bug](https://github.com/quick-lint/quick-lint-js/issues/1057) was there from the
beginning, though.) The design also makes testing more convoluted. The new
`Event_Loop2` is a callback-based design like libevent and libuv. It has better
ergonomics and is easier to find edge cases in.

Testing of `Event_Loop` is lacking, hence the presence of some bugs.
`Event_Loop2` is much more testable and is much better tested.

`Event_Loop`'s livetime is coupled to its reader IPC pipe. This makes its use in
the Visual Studio Code extension (which has no IPC pipe) ugly. (The extension
code creates a dummy pipe to keep the event loop alive.) It also has lead to
hacks in the LSP benchmarks for servers which refuse to close the other end of
the reader IPC pipe. The new `Event_Loop2` design has explicit retain/release
calls for the event loop, removing the need for a dummy pipe. (Technically, the
dummy pipe still exists within the `Event_Loop2` implementation, but only on
Linux.)

Synchronous file I/O with no overlapping is problematic when quick-lint-js is
embedded. For example, in the Visual Studio Code extension, synchronous file I/O
occurs on the Node.js main thread, preventing other extensions from executing
while the file I/O occurs. This is problematic for users who use network drives,
for example. As another example, synchronous file I/O cannot be modelled in a
web browser, thus quick-lint-js cannot run in Codespaces. (Codespaces support
would [give quick-lint-js a competitive
advantage](https://github.com/quick-lint/quick-lint-js/issues/417) over ESLint.)
