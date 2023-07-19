# ADR016: Collections

**Status**: Accepted and active.

## Context

Programs need to deal with variable-sized data, and quick-lint-js is no
exception. Strings, arrays, and dictionaries are needed for various tasks. There
are many implementations for such collections with different tradeoffs.

## Decision

quick-lint-js uses a mix of standard and custom collections.

Code for custom collections lives in
[`src/quick-lint-js/container/`](../../src/quick-lint-js/container/).

For fixed-sized lists, quick-lint-js uses:
* C-style arrays
* `std::array`

For variable-sized lists, quick-lint-js uses:
* `quick_lint_js::Bump_Vector` in performance-critical code
* `quick_lint_js::Linked_Vector` where pointer stability is required
* `quick_lint_js::Byte_Buffer` for byte arrays destined for I/O syscalls
* `quick_lint_js::Padded_String` for strings which need to be parsed
* C strings, `std::string`, and `quick_lint_js::String8`
  (`std::basic_string<char8_t>`) for strings
* `std::vector` otherwise
* Banned: `std::deque`
* Banned: `std::list`
* Banned: `std::slist`

For associative dictionaries, quick-lint-js uses:
* `quick_lint_js::Hash_Map`
* `quick_lint_js::Hash_Set`
* Banned: `std::unordered_map` (except when implementing
  `quick_lint_js::Hash_Map`)
* Banned: `std::unordered_set` (except when implementing
  `quick_lint_js::Hash_Set`)
* Banned: `std::map`
* Banned: `std::set`

For functions, quick-lint-js uses:
* Polymorphic base classes
* `quick_lint_js::Heap_Function`
* Banned: `std::function`

For variants, quick-lint-js uses:
* `std::optional`
* `quick_lint_js::Result`
* `quick_lint_js::Variant`
* Banned: `std::variant`

## Consequences

Implementing custom collections lets us add convenience functions, such as a
backport for `contains` on `Hash_Set` (`std::unordered_set`).

Custom collections easily reduce compile times compared to standard library
options. This was the motivation for custom `std::deque`, `std::function`, and
`std::variant`.

Custom collections can be instrumented more easily, making performance tuning
easier. See the [vector profiler](../profiling.md#vector-profiler) for an
example.

Custom collections means lots of code, opening us up to more bugs.
