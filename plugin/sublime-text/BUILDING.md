# quick-lint-js Sublime Text plugin

This directory contains a plugin for the [Sublime Text editor].

## Building

To build the plugin version made for Sublime Text 3, change all
`-DQLJS_SUBLIME_TEXT=4` to `-DQLJS_SUBLIME_TEXT=3`
and `build-sublime-text-4` to `build-sublime-text-3`.

### Linux

To build this extension, install [GCC], [CMake], and [Ninja], then run the
following commands:

```shell
cd plugin/sublime-text/  # Navigate to this directory

cmake -S ../.. -B build-sublime-text-4 -G Ninja \
      -D CMAKE_BUILD_TYPE=Release -D QLJS_SUBLIME_TEXT_4=ON

cmake --build build-sublime-text-4/ --target quick-lint-js-sublime-text
```

### macOS

To build this extension, install [LLVM], [CMake], and [Ninja] using [Homebrew]:

```shell
brew install llvm cmake ninja
```

then run the following commands:

```shell
cd plugin/sublime-text/  # Navigate to this directory

PATH="$(brew --prefix)/opt/llvm/bin:$PATH"       \
CC=clang                                         \
CXX=clang++                                      \
CPPFLAGS="-I$(brew --prefix)/opt/llvm/include"   \
CXXFLAGS=-D_LIBCPP_DISABLE_AVAILABILITY          \
LDFLAGS="-L$(brew --prefix)/opt/llvm/lib"        \
cmake -S ../.. -B build-sublime-text-4/ -G Ninja \
      -D CMAKE_BUILD_TYPE=Release -D QLJS_SUBLIME_TEXT=4

cmake --build build-sublime-text-4/ --target quick-lint-js-sublime-text
```

### Windows

To build this extension, install [Visual Studio C/C++], [CMake], and [Ninja],
then open the [developer command prompt] belonging to Visual Studio and run the
following commands:

```batch
cd plugin/sublime-text/  &REM Navigate to this directory.

cmake -S ../.. -B build -G Ninja ^
      -D CMAKE_BUILD_TYPE=Release -D QLJS_SUBLIME_TEXT_4=ON

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
[LLVM]: https://llvm.org/
[Visual Studio C/C++]: https://visualstudio.microsoft.com/vs/features/cplusplus/
[CMake]: https://cmake.org/
[Ninja]: https://ninja-build.org/
[Homebrew]: https://brew.sh/
[developer command prompt]: https://docs.microsoft.com/en-us/cpp/build/building-on-the-command-line?view=msvc-160#developer_command_prompt_shortcuts
