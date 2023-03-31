# ADR002: Programming language

**Status**: Accepted and active.

## Context

Software engineers choose programming languages for a variety of reasons. The
choice of a programming language has major consequences. The wrong choice can
destroy or severely delay a project.

## Decision

quick-lint-js's main components are written in C++.

quick-lint-js' editor plugins are written in a mix of C++ and the editor's
native plugin language.

## Consequences

C++ is familiar to strager, the original author of quick-lint-js. This makes
strager productive and happy.

C++ is a popular programming language in the free software, compiler, and open
source communities. This makes it easier to attract talent and patches.

C++ programs can have a low start-up time, which made a quick-and-dirty LSP-less
Vim plugin very effective for getting a feel for quick-lint-js.

C++ gives the programmer access to CPU features such as x86's SSE, making it
easier to optimize.

C++ can compile to WebAssembly using emscripten, making it possible to run C++
in the web browser for web demos.

C++ is unpopular for JavaScript tooling compared to JavaScript. Convincing
JavaScript enthusiasts to contribute to a C++ codebase might be a challenge.

C++'s integration with typical JavaScript tooling such as npm is weak.
Integrating a fast CLI build with npm requires distributing different
pre-compiled executables for different platforms, increasing download size and
adding to the complexity of the release processing.

Not writing all of quick-lint-js in Vimscript makes it more difficult to install
quick-lint-js in Vim.

C++' template feature made it possible to mock with zero run-time overhead. This
made fine-grained testing a lot easier. This was the primary reason
quick-lint-js was not originally written in C. However, as of commit
40a89e9e22952d1bd1a6ac7e0132ceda28efdbf4, templates are no longer needed for
mocking in tests.
