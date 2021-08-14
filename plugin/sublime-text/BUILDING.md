# quick-lint-js Sublime Text plugin

This directory contains a plugin for the [Sublime Text editor].

## Building

To build the plugin version made for Sublime Text 3, change all
`-DQUICK_LINT_JS_SUBLIME_TEXT_4=ON` to `-DQUICK_LINT_JS_SUBLIME_TEXT_3=ON`.

### Linux

To build this extension, install [GCC], [CMake], and [Ninja], then run the
following commands:

```shell
cd plugin/sublime-text/  # Navigate to this directory.
cmake -S ../.. -B build -G Ninja -DCMAKE_BUILD_TYPE=Release \
    -DQUICK_LINT_JS_SUBLIME_TEXT_4=ON -DBUILD_SHARED_LIBS=ON \
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON
ninja -C build quick-lint-js-sublime-text
```

### Windows

To build this extension, install [Visual Studio C/C++], [CMake], and [Ninja],
then open the [developer command prompt] belonging to Visual Studio and run the
following commands:

```shell
cd plugin/sublime-text/  # Navigate to this directory.
cmake -S ../.. -B build -G Ninja -DCMAKE_BUILD_TYPE=Release ^
    -DQUICK_LINT_JS_SUBLIME_TEXT_4=ON -DBUILD_SHARED_LIBS=ON ^
    -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DCMAKE_WINDOWS_EXPORT_ALL_SYMBOLS=ON
ninja -C build quick-lint-js-sublime-text
```

## Installing

To install this extension on your machine after building it, run the following
command:

```shell
cmake --install build --component sublime-text
```

[Sublime Text editor]: https://www.sublimetext.com/
[GCC]: https://gcc.gnu.org/
[Visual Studio C/C++]: https://visualstudio.microsoft.com/vs/features/cplusplus/
[CMake]: https://cmake.org/
[Ninja]: https://ninja-build.org/
[developer command prompt]: https://docs.microsoft.com/en-us/cpp/build/building-on-the-command-line?view=msvc-160#developer_command_prompt_shortcuts
