# quick-lint-js Sublime Text plugin

This directory contains a plugin for the [Sublime Text editor].

## Building

To build this plugin, [configure quick-lint-js with CMake] with
`-DQUICK_LINT_JS_SUBLIME_TEXT_VERSION=<put the major version here>`,
then run the following commands:

    $ cmake --build build --target quick-lint-js-sublime-text quick-lint-js-sublime-text-licenses
    $ cmake --install build --component sublime-text #?? --prefix plugin/sublime-text

[Sublime Text editor]: https://www.sublimetext.com/
[configure quick-lint-js with CMake]: ../../docs/BUILDING.md
