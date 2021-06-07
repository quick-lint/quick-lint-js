# ADR010: LSP server

**Status**: Accepted and active.

## Context

The [Language Server Protocol (LSP)][LSP] makes code editor plugins easier to
write by standardizing several aspects of editor<->language interaction. In an
LSP world, there are three major components components:

* editor (language-agnostic)
* editor plugin (LSP client) (editor-specific; language-specific)
* language server (LSP server) (editor-agnostic; language-specific)

Developers of language tooling are expected to implement the language server.

[LSP]: https://langserver.org/

## Decision

quick-lint-js develops its own LSP server. It is integrated into the same
executable as the CLI and is accessible with the `--lsp-server` option.

quick-lint-js develops its own editor plugins. Some of these editor plugins
utilize the LSP server.

JSON parsing is done with simdjson. JSON serializing is done manually.

## Consequences

LSP has some accidental complexities. For example, locations count the number of
UTF-16 code units since the beginning of a line. Counting in this way is
non-trivial for the LSP server and is complicated for some LSP clients (editors)
too. Non-LSP editor plugins don't necessarily have this complexity;
quick-lint-js can use the editor's native location scheme without multiple
layers of translation.

Error handling with simdjson, particularly with its On Demand API, is tedious.
This is because of [ADR008: Exceptions][ADR008] banning exception in
quick-lint-js. simdjson's exception-based interface would be much easier to use.

Contrary to what LSP marketing might imply, LSP still requires editor plugins to
be written for the specific LSP server. Developing and maintaining editor
plugins for popular editors is still necessary with LSP.

[ADR008]: ./ADR008-Exceptions.md
