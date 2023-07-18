# ADR018: Naming style

**Status**: Accepted and active; pending migration.

## Context

Symbols in programs need to be named. There are several common conventions for
naming symbols, even in C++. Consistent naming improves readability and reduces
developer infighting.

## Decision

quick-lint-js uses the following styles for C++ code based on the C++ standard
library's naming conventions:

* types
    * classes: `Upper_Snake_Case` (previously: `lower_snake_case`)
    * enums: `Upper_Snake_Case` (previously: `lower_snake_case`)
    * type aliases: `Upper_Snake_Case` (previously: `lower_snake_case`)
    * type template parameters: `Upper_Snake_Case` (previously: `UpperCamelCase`)
* values
    * local variables: `lower_snake_case`
    * global variables: `lower_snake_case`
    * member variables
      * public: `lower_snake_case`
      * private: `lower_snake_case_` (trailing underscore)
    * enum members: `lower_snake_case`
    * parameter variables: `lower_snake_case`
    * variable template parameters: `lower_snake_case` (previously: `UpperCamelCase`)
* functions
    * free functions: `lower_snake_case`
    * class methods: `lower_snake_case`
* misc
    * macros: `SHOUTING_SNAKE_CASE`
    * namespaces: `lower_snake_case`
    * file names: `lower-kebab-case`
    * concepts: `lower_snake_case`

## Consequences

Previously, because types and values have the same naming convention, we are
sometimes forced to either qualify a variable's type or choose an inferior name.
For example:

```c++
quick_lint_js::output_format output_format =
    quick_lint_js::output_format::default_format;
```

This issue has been fixed in an update to this ADR. The code can now be more
succinct:

```c++
Output_Format output_format = Output_Format::default_format;
```
