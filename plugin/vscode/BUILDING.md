# quick-lint-js Visual Studio Code plugin

This directory contains a plugin for the [Visual Studio Code
editor][VisualStudioCode].

## Building

To build this extension, [configure quick-lint-js with
CMake](../../docs/BUILDING.md) with `-DQUICK_LINT_JS_ENABLE_VSCODE=YES
-DCMAKE_POSITION_INDEPENDENT_CODE=YES`, then run the following commands:

    $ # Build the quick-lint-js Node.js addon:
    $ cmake --build build --target quick-lint-js-vscode-node quick-lint-js-vscode-node-licenses

    $ # Copy files into the VS Code extension:
    $ cmake --install build --component vscode-node --prefix plugin/vscode

Finally, run the following commands to create `quick-lint-js-2.3.0.vsix`:

    $ cd plugin/vscode/  # Navigate to this directory.
    $ yarn
    $ ./node_modules/.bin/vsce package --baseImagesUrl https://raw.githubusercontent.com/quick-lint/quick-lint-js/master/plugin/vscode/

## Testing

After [building](#Building), run the following commands to run the automated
test suite. Packaging is not necessary.

    $ cd plugin/vscode/  # Navigate to this directory.
    $ yarn
    $ yarn test

[VisualStudioCode]: https://code.visualstudio.com/
