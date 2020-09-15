# Building quick-lint-js

## For contributors

quick-lint-js contributors should use the CMake meta build system.

Recommended process:

1. Run CMake to create a build directory containing project files.
2. Run your build tool to compile quick-lint-js and its tests.
3. Run quick-lint-js' tests directly or using CTest.

The exact commands you need to run differs depending on your preferred
development environment and build tool:

* [macOS and Linux: Ninja](#macos-and-linux-ninja)
* [macOS and Linux: make](#macos-and-linux-make)
* [Windows: Visual Studio](#windows-visual-studio)

---

### macOS and Linux: Ninja

#### 1. Configure with CMake

Install [CMake][] and [Ninja][], then run the following command to create a
directory called `build`:

    $ cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -S . -B build

#### 2. Build

Run the following command to build the quick-lint-js executable, quick-lint-js'
tests, and quick-lint-js' benchmarks:

    $ ninja -C build

If you only want to build the quick-lint-js executable:

    $ ninja -C build quick-lint-js

If you only want to build quick-lint-js' tests:

    $ ninja -C build quick-lint-js-test

#### 3. Run

Run the following command to run quick-lint-js' test suite:

    $ ninja -C build test

If you want to run the test executable manually (e.g. to run it with `gdb` or
`valgrind`):

    $ ./build/test/quick-lint-js-test

If you want to run the quick-lint-js program:

    $ ./build/src/quick-lint-js

---

### macOS and Linux: make

#### 1. Configure with CMake

Install [CMake](https://cmake.org/), then run the following command to create a
directory called `build`:

    $ cmake -G 'Unix Makefiles' -DCMAKE_BUILD_TYPE=Debug -S . -B build

#### 2. Build

Run the following command to build the quick-lint-js executable, quick-lint-js'
tests, and quick-lint-js' benchmarks:

    $ make -j4 -C build

If you only want to build the quick-lint-js executable:

    $ make -j4 -C build quick-lint-js

If you only want to build quick-lint-js' tests:

    $ make -j4 -C build quick-lint-js-test

#### 3. Run

Run the following command to run quick-lint-js' test suite:

    $ make -C build test

If you want to run the test executable manually (e.g. to run it with `gdb` or
`valgrind`):

    $ ./build/test/quick-lint-js-test

If you want to run the quick-lint-js program:

    $ ./build/src/quick-lint-js

---

### Windows: Visual Studio

#### 1. Configure with CMake

Install [CMake][] and [Visual Studio 2019][Visual Studio]. Open *x64 Native
Tools Command Prompt for VS*. Use `cd` to navigate to your quick-lint-js project
directory. Run the following command to create a directory called `build`:

    > cmake -G "Visual Studio 16 2019" -S . -B build

#### 2. Build

Open the Visual Studio solution in the `build` directory:

    > start build\quick-lint-js.sln

In Visual Studio, build the solution: press F6, or select the Build > Build
Solution menu option.

#### 3. Run

Open the Solution Explorer panel, right-click the quick-lint-js-test project,
and select the Set as StartUp Project menu option.

Run the test program: press F5, or select the Debug > Start Debugging menu
option.

If you want to run the quick-lint-js program, you can find it in
`build\src\Debug\quick-lint-js.exe`. Run it from a command prompt or PowerShell
window.

[CMake]: https://cmake.org/
[Ninja]: https://ninja-build.org/
[Visual Studio]: https://visualstudio.microsoft.com/vs/
