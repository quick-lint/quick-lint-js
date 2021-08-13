# quick-lint-js JavaScript interface

This directory contains code which bridges JavaScript to WebAssembly-compiled
quick-lint-js.

In other words, this package contains WebAssembly bindings.

## Building

To build the bindings, install the [emscripten SDK][emscripten-sdk], [Ninja][],
and [Node.js][], then run the following commands:

    $ cd website/
    $ emcmake cmake -S .. -B build-emscripten -G Ninja -DCMAKE_BUILD_TYPE=Release
    $ emmake ninja -C build-emscripten quick-lint-js-vscode quick-lint-js-vscode-licenses
    $ emmake cmake --install build-emscripten --component vscode --prefix public/demo

## Testing

To manually test this package, [build](#Building), then run `cli.js`:

    $ node cli.js PATH_TO_YOUR_JS_FILE.js

[Ninja]: https://ninja-build.org/
[Node.js]: https://nodejs.org/
[emscripten-sdk]: https://emscripten.org/docs/getting_started/downloads.html
