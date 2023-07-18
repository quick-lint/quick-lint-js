# ADR018: Naming style

**Status**: Accepted and active.

## Context

Symbols in programs need to be named. There are several common conventions for
naming symbols, even in C++. Consistent naming improves readability and reduces
developer infighting.

## Decision

quick-lint-js uses the following styles for C++ code based on the C++ standard
library's naming conventions:

* types
    * classes: `lower_snake_case`
    * enums: `lower_snake_case`
    * type aliases: `lower_snake_case`
    * type template parameters: `UpperCamelCase`
* values
    * local variables: `lower_snake_case`
    * global variables: `lower_snake_case`
    * member variables
      * public: `lower_snake_case`
      * private: `lower_snake_case_` (trailing underscore)
    * enum members: `lower_snake_case`
    * parameter variables: `lower_snake_case`
    * variable template parameters: `UpperCamelCase`
* functions
    * free functions: `lower_snake_case`
    * class methods: `lower_snake_case`
* misc
    * macros: `SHOUTING_SNAKE_CASE`
    * namespaces: `lower_snake_case`
    * file names: `lower-kebab-case`
    * concepts: `lower_snake_case`

## Consequences

Because types and values have the same naming convention, we are sometimes
forced to either qualify a variable's type or choose an inferior name. For
example:

```c++
quick_lint_js::output_format output_format =
    quick_lint_js::output_format::default_format;
```
