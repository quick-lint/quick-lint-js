# Tracing in quick-lint-js

quick-lint-js supports a binary log format. This document describes the format
and tools used to process the format.

See [ADR005 Logging and tracing](architecture/ADR015-Logging-and-tracing.md) for
rationale.

## Format

quick-lint-js' binary log is compatible with the [Common Trace Format][]. The
format of each stream is described by a [CTF metadata
file](../src/trace-metadata.cpp).

A trace is a directory containing a *metadata file* and one or more *stream
files*.

The metadata file is a text file describing the format of the stream files. The
metadata file is called `metadata` in the directory, as expected by
[Babeltrace][].

A stream file is a binary file containing logged data. Each stream file is
called `thread###` (where `###` is one or more decimal digits).

## Producing traces

To produce a trace with the Visual Studio Code extension, set the
`quick-lint-js.tracing` setting to `"verbose"`.

To find the trace directory: Open the Command Palette (ctrl-shift-P) then type
"Developer: Open Extension Logs Folder". Open the `quick-lint.quick-lint-js`
directory. Traces are written into a directory named `trace_%TIMESTAMP%` (e.g.
`trace_2022-05-17-19-41-14.zlNiwm`).

## Consuming traces

There are a few tools which can consume traces produced by quick-lint-js.

### quick-lint-js-analyze-trace

Build the quick-lint-js-analyze-trace CMake target, then run it with a trace
file. For example:

    $ ninja -C build quick-lint-js-analyze-trace
    $ ./build/tools/quick-lint-js-analyze-trace .../thread1
    @0000.000000000 init version='2.4.2'
    @0000.000000000 document 0x2b40027ce4c0 opened: file:///home/strager/Projects/quicklint-js/sandbox/hello.js
    @0000.000000000 document 0x2b40027ce4c0 changed
                    3:0->3:0: 'h'
    [snip]
    @0000.000000000 document 0x2b40027ce4c0 changed
                    3:9->3:10: ''
    $ ./build/tools/quick-lint-js-analyze-trace --dump-final-document-content 0x2b40027ce4c0 .../thread1
    const x = 3;
    x = 4;sadasdsad
    hello
    hellohell

### Babeltrace

The [Babeltrace][] tool can dump traces:

    $ babeltrace2 ~/.config/Code/logs/20220516T161030/exthost22/quick-lint.quick-lint-js/2022-05-17-19-29-57.zVvhTp
    [16:00:00.000000000] (+?.?????????) init: { thread_id = 164877, compression_scheme = 0 }, { quick_lint_js_version = "2.4.2" }
    [16:00:00.000000000] (+0.000000000) vscode_document_opened: { thread_id = 164877, compression_scheme = 0 }, { doc_id = 3221267211232, uri = { code_unit_count = 60, code_units = [ ... ] }, language_id = { code_unit_count = 15, code_units = [ ... ] }, content = { code_unit_count = 110, code_units = [ ... ] } }
    [16:00:00.000000000] (+0.000000000) vscode_document_closed: { thread_id = 164877, compression_scheme = 0 }, { doc_id = 0, uri = { code_unit_count = 12, code_units = [ ... ] }, language_id = { code_unit_count = 3, code_units = [ ... ] } }
    [16:00:00.000000000] (+0.000000000) vscode_document_opened: { thread_id = 164877, compression_scheme = 0 }, { doc_id = 3221267214816, uri = { code_unit_count = 59, code_units = [ ... ] }, language_id = { code_unit_count = 10, code_units = [ ... ] }, content = { code_unit_count = 35, code_units = [ ... ] } }

### 010 Editor

[010 Editor][] is a hex editor which supports decoding file formats.
[trace.bt](../tools/trace.bt) contains a plugin (*template*) for 010 Editor,
allowing you to decode and inspect quick-lint-js trace files.

[010 Editor]: https://www.sweetscape.com/010editor/
[Babeltrace]: https://babeltrace.org/
[Common Trace Format]: https://diamon.org/ctf/
