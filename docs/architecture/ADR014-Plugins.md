# ADR014: Plugins

**Status**: Accepted and active.

## Context

Static analysis tools an have every-growing list of patterns to check. Some
patterns are team-specific and don't make sense to include in the main static
analyzer code base. A common solution for third-party patterns is a plugin
architecture.

quick-lint-js is written in C++. Plugins in C++ typically impose a few
constraints:

* verisioning: stable ABI or RPC
* security: plugins can't be sandboxed (unless NaCl or WebAssembly is used), and
  loading plugins from a project's directory is risky
* compilation: plugins may need to be compiled for many architectures, similar
  to quick-lint-js itself.

Areas of quick-lint-js a plugin might want to integrate with:

* linting
* error reporting
* auto-fixes
* configuration
* parsing and lexing

## Decision

quick-lint-js will not support plugins.

## Consequences

Users of quick-lint-js cannot write custom, fast rules, even if they would be
relatively easy to implement.

quick-lint-js isn't locked into a particular design for parsing, rules, or
reporting errors. This has negatively affected
[Babel](https://rome.tools/blog/2020/08/08/introducing-rome#history) before, but
hasn't affected quick-lint-js yet.

The quick-lint-js team didn't need to spend the engineering time designing a
plugin architecture and getting feedback from users.

Plugins can't slow down quick-lint-js and give it a bad impression. (Plugins can
and do slow down tools like ESLint and Babel.)
