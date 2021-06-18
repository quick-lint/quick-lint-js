# quick-lint-js Visual Studio Code plugin

This directory contains a plugin for the [Visual Studio Code
editor][VisualStudioCode].

## Building

To build this extension, first [build the quick-lint-js-wasm
package](../../wasm/README.md#Building).

Then, run the following command:

    $ cd plugin/vscode/  # Navigate to this directory.
    $ yarn install --force

Finally, run the following command to create `quick-lint-js-0.2.0.vsix`:

    $ ./node_modules/.bin/vsce package --baseImagesUrl https://raw.githubusercontent.com/quick-lint/quick-lint-js/master/plugin/vscode/

## Testing

After [building](#Building), run `yarn test` to run the automated test suite.
Packaging is not necessary to run `yarn test`.

[Ninja]: https://ninja-build.org/
[Node.js]: https://nodejs.org/
[VisualStudioCode]: https://code.visualstudio.com/
[emscripten-sdk]: https://emscripten.org/docs/getting_started/downloads.html
