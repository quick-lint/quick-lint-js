# quick-lint-js Debian package

This directory contains a Debian source package, which are rules for installing
quick-lint-js on Debian, Ubuntu, and other Linux distributions.

## Building

On a Debian-based machine, install dependencies for .deb generation:

    $ sudo apt-get install cmake debhelper dpkg-dev gcc-9 g++-9 git lintian

Then, run the `build.sh` script will build
`dist/debian/quick-lint-js_0.1.0-1_amd64.deb` and related files.

## Installing

On a Debian-based system, after building the .deb file (per the above
instructions), install the .deb file:

    $ sudo dpkg -i dist/debian/quick-lint-js_0.1.0-1_amd64.deb
