# quick-lint-js Debian package

This directory contains a Debian source package, which are rules for installing
quick-lint-js on Debian, Ubuntu, and other Linux distributions.

## Building

On a Debian-based machine, install dependencies for .deb generation:

    $ sudo apt-get install cmake debhelper dpkg-dev gcc-9 g++-9 git lintian

Then, run the `build.sh` script will build
`dist/debian/quick-lint-js_0.6.0-1_amd64.deb`,
`dist/debian/quick-lint-js-vim_0.6.0-1_all.deb`, and related files.

## Installing

On a Debian-based system, after building the .deb file (per the above
instructions), install the .deb file:

    $ sudo dpkg -i dist/debian/quick-lint-js_0.6.0-1_amd64.deb

### Vim plugin

For quick-lint-js support in vim, you will need to install a supported styling plugin - see the [quick-lint-js vim plugin documentation](https://github.com/quick-lint/quick-lint-js/blob/master/plugin/vim/quick-lint-js.vim/doc/quick-lint-js.txt) for options and instructions.

Then, install quick-lint-js-vim and its dependencies:

    $ sudo apt install vim-addon-manager
    $ sudo dpkg -i dist/debian/quick-lint-js_0.6.0-1_amd64.deb
    $ sudo dpkg -i dist/debian/quick-lint-js-vim_0.6.0-1_all.deb
~
