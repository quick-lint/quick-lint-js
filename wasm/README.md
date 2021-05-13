# quick-lint-js JavaScript interface

This directory contains a Node.js package which bridges JavaScript to
WebAssembly-compiled quick-lint-js.

In other words, this package contains WebAssembly bindings.

## Building

To build this package, install the [emscripten SDK][emscripten-sdk], [Ninja][],
and [Node.js][], then run the following commands:

    $ cd wasm/  # Navigate to this directory.
    $ emcmake cmake -S .. -B build-emscripten -G Ninja -DCMAKE_BUILD_TYPE=Release
    $ emmake ninja -C build-emscripten quick-lint-js-vscode quick-lint-js-vscode-licenses
    $ emmake cmake --install build-emscripten --component vscode --prefix .

## Testing

To manually test this package, [build](#Building), then run `cli.js`:

    $ node wasm/cli.js PATH_TO_YOUR_JS_FILE.js
