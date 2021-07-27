# quick-lint-js JavaScript interface

This directory contains a Node.js package which bridges JavaScript to
WebAssembly-compiled quick-lint-js.

In other words, this package contains WebAssembly bindings. It also contains a
Node.js native extension for some OS-specific support code.

## Building

To build this package, install a host C++ compiler, the [emscripten
SDK][emscripten-sdk], [Ninja][], and [Node.js][], then run the following
commands:

    $ cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release -DQUICK_LINT_JS_ENABLE_VSCODE=YES
    $ ninja -C build quick-lint-js-node-fs quick-lint-js-node-fs-licenses
    $ cmake --install build --component vscode --prefix wasm

    $ cd wasm/  # Navigate to this directory.
    $ emcmake cmake -S .. -B build-emscripten -G Ninja -DCMAKE_BUILD_TYPE=Release
    $ emmake ninja -C build-emscripten quick-lint-js-vscode quick-lint-js-vscode-licenses
    $ emmake cmake --install build-emscripten --component vscode --prefix .

## Testing

To manually test this package, [build](#Building), then run `cli.js`:

    $ node wasm/cli.js PATH_TO_YOUR_JS_FILE.js

[Ninja]: https://ninja-build.org/
[Node.js]: https://nodejs.org/
[emscripten-sdk]: https://emscripten.org/docs/getting_started/downloads.html
