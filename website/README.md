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

If you want to work on the demo, see [demo documentation](demo/README.md).

If you want to work on the error documentation, see [errors
documentation](public/errors/README.md).

## Packaging for deployment

Prepare the website for publishing on GitHub pages by first building the [web
demo](demo/README.md) and [quick-lint-js-wasm](../wasm/README.md).

Then, run `yarn build`:

    $ cd website  # Navigate to this directory.
    $ yarn        # Install dependencies.
    $ yarn build

Look at the copied and generated files in `website/www/`.

For instructions on deploying to quick-lint-js.com, see the [release
documentation](docs/RELEASE.md).
