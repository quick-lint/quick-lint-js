# quick-lint-js web demo

To build the demo, install the [emscripten SDK][emscripten-sdk] and
[Ninja][], then run the following commands:

    $ cd website/demo  # Navigate to this directory.
    $ emcmake cmake -S ../.. -B build-emscripten -G Ninja -DCMAKE_BUILD_TYPE=Release
    $ emmake ninja -C build-emscripten quick-lint-js-wasm-demo quick-lint-js-wasm-demo-licenses
    $ emmake cmake --install build-emscripten --component wasm-demo --prefix .

To run the demo on your own machine, execute the build instructions above, then
run a static file server in the `website` directory:

    $ cd website  # Navigate to the parent of this directory.
    $ python -m http.server 8080

Then, in your web browser, navigate to the demo page on the server (e.g.
`http://localhost:8080/demo/`).

[Ninja]: https://ninja-build.org/
[emscripten-sdk]: https://emscripten.org/docs/getting_started/downloads.html
