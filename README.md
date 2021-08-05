# quick-lint-js

[quick-lint-js][] finds bugs in JavaScript programs.

**Warning**: This is pre-release software. quick-lint-js is under active
development and is unstable. Use at your own risk.

quick-lint-js finds many bugs, including:

* Using a variable which does not exist
* Assigning to a `const` variable
* Using `await` in a non-`async` function
* Syntax errors

![Demonstration of quick-lint-js in Visual Studio Code](plugin/vscode-wasm/demo.webp)

## Installing

See [installation instructions](https://quick-lint-js.com/install/) for how
to install quick-lint-js' CLI, LSP server, and editor plugins.

See [build instructions](docs/BUILDING.md) for how to build quick-lint-js for
development.

## Contact

**Bugs and feature requests**: [file an issue on GitHub](https://github.com/quick-lint/quick-lint-js/issues)

**IRC**: ask questions in [#quick-lint-js][quick-lint-js-irc-web] on
[Libera.Chat][]

**Security bug reports (private disclosure)**: email
[strager.nds@gmail.com](mailto:strager.nds@gmail.com)

## Values

* **Performance**. You use quick-lint-js in text editors and IDEs to show bugs
  as they are written. quick-lint-js must be *fast* to make feedback *responsive*.
  * **Small design**. The less infrastructure and developer conveniences used
    within quick-lint-js, the less time quick-lint-js wastes due to this bloat.
  * **Few features**. Features add run-time costs. With fewer features,
    quick-lint-js gives you feedback sooner.

* **High signal**. quick-lint-js finds bugs, not nitpicks. You can use
  quick-lint-js in *any* project, no matter the size or style.
  * **No opinions**. quick-lint-js doesn't complain about style issues, like
    using `'strings'` vs `"strings"`. It complains about real bugs which
    everyone agrees are bugs.
  * **No false positives**. If quick-lint-js complains, you know it's a bug in
    your code.
  * **No configuration**. quick-lint-js works out-of-the-box. You don't need
    configuration files to tell quick-lint-js what buggy code looks like.

[Libera.Chat]: https://libera.chat/
[quick-lint-js-irc-web]: https://kiwiirc.com/nextclient/irc.libera.chat/#quick-lint-js
[quick-lint-js]: https://quick-lint-js.com/
