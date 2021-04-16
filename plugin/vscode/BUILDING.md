
# quick-lint-js Visual Studio Code plugin

This directory contains a plugin for the [Visual Studio Code
editor][VisualStudioCode].

## Building

To build the extension, install the [emscripten SDK][emscripten-sdk], [Ninja][],
and [Node.js][], then run the following commands:

    $ cd plugin/vscode/  # Navigate to this directory.
    $ emcmake cmake -S ../.. -B build-emscripten -G Ninja -DCMAKE_BUILD_TYPE=Release
    $ emmake ninja -C build-emscripten quick-lint-js-vscode quick-lint-js-vscode-licenses
    $ emmake cmake --install build-emscripten --component vscode --prefix .

**Note for Windows Users**: In the [CMakeLists.txt](https://github.com/lifeinData/quick-lint-js/blob/master/plugin/vscode/CMakeLists.txt) file,  remove the apostrophe character before "[" and "]" in [line 14] (https://github.com/lifeinData/quick-lint-js/blob/d0d48525151101a46627c454618870cb7d6dbb2c/plugin/vscode/CMakeLists.txt#L14) 

Then, run the following command to create `quick-lint-js-0.2.0.vsix`:

    $ npx vsce package --baseImagesUrl https://raw.githubusercontent.com/quick-lint/quick-lint-js/master/plugin/vscode/

## Testing

After [building](#Building), run `yarn test` to run the automated test suite.
Packaging is not necessary to run `yarn test`.

To manually test the extension's WebAssembly bindings, [build](#Building), then
run `cli.js` :

    $ node plugin/vscode/cli.js PATH_TO_YOUR_JS_FILE.js

[Ninja]: https://ninja-build.org/
[Node.js]: https://nodejs.org/
[VisualStudioCode]: https://code.visualstudio.com/
[emscripten-sdk]: https://emscripten.org/docs/getting_started/downloads.html
