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

(not yet implemented)

## Consuming traces

There are a few tools which can consume traces produced by quick-lint-js.

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
