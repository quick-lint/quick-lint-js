# browser-globals

This directory contains tools for extracting global variable definitions from
web specifications, including the HTML, WebGL, and SVG specifications.

## Setup

Install [Node.js][] and [Yarn v1][].

Install dependencies:

    $ cd tools/browser-globals/  # Navigate to this directory.
    $ yarn install

Download the [web-specs repository](https://github.com/quick-lint/web-specs):

    $ cd tools/browser-globals/  # Navigate to this directory.
    $ git clone https://github.com/quick-lint/web-specs
    $ cd web-specs/
    $ git submodule update --init --depth 1
    $ cd ../../../

## Running

Run the following command to regenerate src/global-variables-browser.cpp:

    $ node tools/browser-globals/index.js tools/browser-globals/web-specs/specs.json >src/global-variables-browser.cpp

[Node.js]: https://nodejs.org/en/
[Yarn v1]: https://classic.yarnpkg.com/lang/en/
