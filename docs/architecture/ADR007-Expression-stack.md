# ADR007: Expression stack

**Status**: Rejected.

## Context

JavaScript expressions are complicated. Most compilers create ASTs to represent
expressions in languages like JavaScript.

quick-lint-js aims to be fast. Constructing and traversing ASTs is expensive,
especially if traversing is done only once. quick-lint-js has different needs
from most compilers, so it can use other techniques for parsing expressions
which require less allocation and copying.

## Decision

When parsing an expression, the parser maintains a stack of nodes. When the
parser is certain how a node should be treated, it immediately removes that node
from the stack and tells the linter about it.

When parsing an expression such as `a+b`, the parser performs these steps:

1. See `a`: Push variable reference `a`; go to pending-operator state.
2. See `+`: Report top as a *use* (`visit_variable_use`); pop; go to
   pending-primary state.
3. See `b`: Push variable reference `b`; go to pending-operator state.
4. See end: Report top as a *use* (`visit_variable_use`).

When parsing an expression such as `a=b`, the parser performs these steps:

1. See `a`: Push variable reference `a`; go to pending-operator state.
2. See `=`: Report top as an *assignment* (`visit_variable_assignment`); pop; go
   to pending-primary state.
3. See `b`: Push variable reference `b`; go to pending-operator state.
4. See end: Report top as a *use* (`visit_variable_use`).

When parsing an expression such as `a[i] = n`, the parser performs these steps:

1. See `a`: Push variable reference `a`; go to pending-operator state.
2. See `[`: Report top as a *use* (`visit_variable_use`); pop; push open-square
   node; go to pending-primary state.
3. See `i`: Push variable reference `i`; go to pending-operator state.
4. See `]`: Report top as a *use* (`visit_variable_use`); pop; pop open-square
   node; go to pending-operator state.
5. See `=`: Pop; go to pending-primary state.
6. See `n`: Push variable reference `n`; go to pending-operator state.
7. See end: Report top as a *use* (`visit_variable_use`).

## Consequences

The logic needed to implement this scheme turned out to be complicated.
Nodes needed to be variable-length anyway (e.g. `[a,b,c,d].map()` vs
`[a,b,c,d]=xs` both need the set of variables inside the array literal for
different reasons). It was too hard to implement. An AST-based approach was
extremely easy to implement in comparison.

Expression parsing didn't seem to be a major bottleneck in the parser. Lexing
and variable lookup are a bigger hotspots compared to an AST-based expression
parser. Therefore, the performance benefits of this expression stack scheme
would be overshadowed by the performance costs of other parts of quick-lint-js.
Optimization effort is better spent elsewhere.
