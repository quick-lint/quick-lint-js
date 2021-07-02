# quick-lint-js Sublime Text plugin

This directory contains a plugin for the [Sublime Text editor](https://www.sublimetext.com/).

## Building

To build this extension, install the [Ninja](https://ninja-build.org/), then run the following commands:

    $ cd plugin/sublime-text/  # Navigate to this directory.
    $ # for Sublime Text 3
    $ cmake -S ../.. -B build -G Ninja -DCMAKE_BUILD_TYPE=Release \
        -DQUICK_LINT_JS_SUBLIME_TEXT_3=ON -DBUILD_SHARED_LIBS=ON \
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
    $ # for Sublime Text 4
    $ cmake -S ../.. -B build -G Ninja -DCMAKE_BUILD_TYPE=Release \
        -DQUICK_LINT_JS_SUBLIME_TEXT_4=ON -DBUILD_SHARED_LIBS=ON \
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
    $ ninja -C build quick-lint-js-sublime-text

## Installing

To install this extension on your machine after building it, run the following command:

    $ cmake --install build --component sublime-text
