# quick-lint-js website

This directory contains the source code for quick-lint-js' website.

## Developing

Hack on the website locally by running a local web server:

    $ cd website  # Navigate to this directory.
    $ yarn        # Install dependencies.
    $ yarn start
    Server running: http://127.0.0.1:9001/

Open after starting the local web server, open http://127.0.0.1:9001/ in your
browser.

If you want to work on the demo or on the error documentation, you need to
[build quick-lint-js for WebAssembly](wasm/README.md).

After building quick-lint-js-wasm, you might need to re-install it with Yarn:

    $ cd website
    $ yarn install --force

## Packaging for deployment

Prepare the website for publishing on GitHub pages by first building
[quick-lint-js for WebAssembly](wasm/README.md).

Then, run `yarn build`:

    $ cd website  # Navigate to this directory.
    $ yarn        # Install dependencies.
    $ yarn build

Look at the copied and generated files in `website/www/`.

For instructions on deploying to quick-lint-js.com, see the [release
documentation](../docs/RELEASE.md).
