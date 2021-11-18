# quick-lint-js architecture

* [Life of a JavaScript bug](#life-of-a-JavaScript-bug)
* [Tips and tricks](#tips-and-tricks)
* [ADRs: Architecture Decision Records](#adrs-architecture-decision-records)

## Life of a JavaScript bug

1. [Loading](#loading)
2. [Lexing and parsing](#lexing-and-parsing)
3. [Analyzing](#analyzing)
4. [Error reporting](#error-reporting)

### Loading

To begin, quick-lint-js needs JavaScript code. How this code is found depends on
the interface used:

* command line interface: User specifies file paths on the command line.
  quick-lint-js loads each file into memory using OS filesystem APIs.
* LSP server: Editor notifies quick-lint-js of newly-opened documents via JSON
  messages over standard input. Editor can send the entire document or
  incremental changes. quick-lint-js manages this state by mapping URIs to
  `document` objects.
* VS Code extension: Editor notifies quick-lint-js of newly-opened documents via
  WebAssembly API calls. Editor can send incremental changes, which
  quick-lint-js understand with a `document` object.
* Demo on website: Editor notifies quick-lint-js of code changes via
  WebAssembly API calls. Each change includes the full text.

JavaScript code is always loaded into memory as a single continuous array of
bytes (`padded_string`).

### Lexing and parsing

Lexing, parsing, analyzing, and error reporting are interleaved. Here we discuss
just lexing and parsing.

quick-lint-js' lexer (`lexer` in `lex.h`) is a hand-written token-at-a-time
lexer. Interpretation of characters, such as `/` (divide or regular expression)
and `}` (symbol or continuation of a template literal), is context-sensitive and
is accomplished with help from the parser. See [ADR005: Incremental
lexing](ADR005-Incremental-lexing.md) for details.

quick-lint-js' parser (`parser` in `parse.h`) is a recursive-descent parser. It
has two main parts: statement parsing and expression parsing.

Expression parsing is ad-hoc, particularly precedence parsing. We hope to
implement a more principled solution in the future. Expression parsing generates
ASTs (`expression` in `expression.h`). ASTs effectively allow arbitrary
lookahead without backtracking to handle expressions like `[a, b, c] = xs`
(where `a` is assigned to by virtue of the `=`) and `(a, b, c) => d` (where `a`
is declared by virtue of the `=>`).

Statement parsing emits events which we call visits. Take the following code for
example:

    async function f(uri) {
      let url = `${api}/${uri}`
      await fetch(url);
    }

Statement parsing will emit the following events:

    visit_variable_declaration("f", function)
    visit_enter_function_scope        // start declaring parameters for 'f'
    visit_variable_declaration("uri", parameter)
    visit_enter_function_scoped_body  // start the body of 'f'
    visit_variable_use("api")
    visit_variable_use("uri")
    visit_variable_declaration("url", const)
    visit_variable_use("fetch")
    visit_variable_use("url")
    visit_exit_function_scope         // leave the body of 'f'
    visit_end_of_module               // end of file

These events are fed to [analysis passes](#analyzing). Emitting an event is
merely a method call, but in some cases, these events can be buffered. Buffering
events (with `buffering_visitor`) might not be needed, but it sounded like a
good idea when we first implemented it. One day we may reconsider buffering
events during parsing.

Syntax errors are reported as they are discovered using the [error
reporting system](#error-reporting). If a syntax error happens (due to an
unexpected token), usually an attempt is made to recover from the syntax error
and continue parsing.

### Analyzing

Analysis is interleaved with parsing. As parsing emits events, analyses update
state and report errors.

There is currently only one analysis: `linter`. `linter` tracks variable uses
and declarations:

* A `visit_variable_use` event causes `linter` to add the variable to a list of
  used variables.
* A `visit_variable_declaration` event causes `linter` to add the variable to a
  list of declared variables and check for use-before-declaration errors.
* A `visit_enter_function_scope` event causes `linter` to push an empty set of
  variable uses and declarations onto a stack.
* A `visit_exit_function_scope` event causes `linter` to pop a variable uses and
  declarations off of a stack. Unbound variable uses are copied to the parent
  scope. Variable declarations are discarded.
* A `visit_end_of_module` event causes `linter` to report errors for variable
  uses which have no corresponding declaration.

### Error reporting

When encountering a syntax error or a semantic error, quick-lint-js needs to
show the error to the user. quick-lint-js implements this using an interface
(`error_reporter` in `error.h`) containing one method per kind of error. The
interface has many implementations, including the following:

* `c_api_error_reporter` for the VS Code plugin and the demo on the website
* `lsp_error_reporter` for LSP clients
* `text_error_reporter` for the command-line interface
* `vim_qflist_json_error_reporter` for the batch-style Vim plugin

Each kind of error has its own class, such as
`error_assignment_before_variable_declaration`. Each class contains data related
to that error, such as the location where the error occurred. Each class also
contains a method which formats error messages. The error classes are written in
a narly macro called `QLJS_X_ERROR_TYPES` in `error.h`.

Error classes contain byte offsets. To convert byte offsets into line numbers
and column numbers, different `error_reporter` classes use different `locator`
classes which produce different answers and make different tradeoffs. For
example:

* `vim_locator`, used by `vim_qflist_json_error_reporter`, makes a cache of line
  start offsets. Querying the line number involves a binary search, and querying
  the column number involves subtraction because Vim uses a byte offset for the
  column number.
* `lsp_locator`, used by `lsp_error_reporter`, makes a cache of line start
  offsets and also a cache of whether each line contains only ASCII characters.
  Querying the line number involves a binary search (like with `vim_locator`),
  but querying the column number involves counting UTF-16 code units for
  non-ASCII lines (per LSP's specification). Also, the cache is double-buffered
  to allow incremental updates from the editor.

See [ADR006: Location tracking](ADR006-Location-tracking.md) for more details.

Error messages can be translated using GNU gettext-style translations.
Translation data is embedded into the executable to simplify deployment and
avoid some performance pitfalls of GNU gettext.

## Tips and tricks

The input JavaScript source code is stored as a contiguous array of UTF-8 code
units. This means that reading the next character involves incrementing a
pointer and dereferencing that pointer.

The input JavaScript source code is null-terminated. This means that every
lookup involves one fewer comparison on average (except when null bytes are
inside the input, which is very rare); the current byte offset doesn't need to
be compared against the length of the input array.

The input JavaScript source code has extra unused bytes at the end. This allows
using SIMD to process many bytes in parallel during lexing. SIMD is used to
parse comments and identifiers 16 bytes at a time, and could be used to parse
string literals and other constructs too.

All tokens track source locations as pointers (i.e. indexes) into the input
source code array. This means that it's easy to recover and print the exact
input source code for a token when reporting errors, and that separate location
information is unnecessary. See [ADR006: Location
tracking](ADR006-Location-tracking.md) for more details.

Identifiers, like other tokens, are represented as pointers (i.e. indexes) into
the original source code. This means that identifiers don't need to be copied
into a symbol table. However, comparing identifiers in analyses takes linear
time, and Unicode-escaped identifiers (such as `\u{68}ello`) need special care.

The parser does not use exceptions. However, sometimes we just want to stop
parsing altogether. The `QLJS_PARSER_UNIMPLEMENTED` macro will call `longjmp` to
go back to the beginning of the parser (which called `setjmp`). This avoids
overheads caused by an exception *possibly* happening, but might result in
memory leaks as destructors aren't called. See [ADR008:
Exceptions](ADR008-Exceptions.md) for more details.

## ADRs: Architecture Decision Records

An ADR is an Architecture Decision Record. ADRs document design choices made in
a project.

This directory contains quick-lint-js' ADRs.

For more information on ADRs, see [Documenting Architecture Decisions by Michael
Nygard][ADR-bible].

### Active ADRs

* [ADR001: Feature testing with have.h](ADR001-Feature-testing-with-have-h.md)
* [ADR002: Programming language](ADR002-Programming-language.md)
* [ADR003: Vendor sources](ADR003-Vendor-sources.md)
* [ADR004: Generated sources](ADR004-Generated-sources.md)
* [ADR005: Incremental lexing](ADR005-Incremental-lexing.md)
* [ADR006: Location tracking](ADR006-Location-tracking.md)
* [ADR008: Exceptions](ADR008-Exceptions.md)
* [ADR009: Website](ADR009-Website.md)
* [ADR010: LSP server](ADR010-LSP-server.md)
* [ADR011: IO errors](ADR011-IO-errors.md)
* [ADR012: Assertions](ADR012-Assertions.md)

### Rejected ADRs

* [ADR007: Expression stack](ADR007-Expression-stack.md)

[ADR-bible]: https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions
