# quick-lint-js Sublime Text plugin

This directory contains a plugin for the [Sublime Text editor].

## Building

<!-- TODO(cahian): Improve these instructions -->
To build this plugin, [configure quick-lint-js with CMake] with
`-DQUICK_LINT_JS_SUBLIME_TEXT_VERSION=3` or `-DQUICK_LINT_JS_SUBLIME_TEXT_VERSION=4`
or whatever version greater than 4 (if there is), then run the following commands:

    $ cmake --build build --target quick-lint-js-sublime-text quick-lint-js-sublime-text-licenses
    $ cmake --install build --component sublime-text #?? --prefix plugin/sublime-text

[Sublime Text editor]: https://www.sublimetext.com/
[configure quick-lint-js with CMake]: ../../docs/BUILDING.md
