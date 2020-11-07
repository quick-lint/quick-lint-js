# Building quick-lint-js

## For contributors

quick-lint-js contributors should use the CMake meta build system.

Recommended process:

1. Run CMake to create a build directory containing project files.
2. Run your build tool to compile quick-lint-js and its tests.
3. Run quick-lint-js' tests directly.

The exact commands you need to run differs depending on your preferred
development environment and build tool:

* [Linux: Ninja](#linux-ninja)
* [Linux: make](#linux-make)
* [macOS: Homebrew LLVM and Ninja](#macos-homebrew-llvm-and-ninja)
* [Windows: Visual Studio](#windows-visual-studio)
* [macOS and Linux: nix](#macos-and-linux-nix)

---

### Linux: Ninja

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

    $ ./build/test/quick-lint-js-test

If you want to run the quick-lint-js program:

    $ ./build/quick-lint-js

---

### Linux: make

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

    $ ./build/test/quick-lint-js-test

If you want to run the quick-lint-js program:

    $ ./build/quick-lint-js

---

### macOS: Homebrew LLVM and Ninja

#### 0. Install Homebrew packages

Install LLVM, CMake, and Ninja using [Homebrew][] by running the following
command:

    $ brew install cmake llvm ninja

#### 1. Configure with CMake

Run the following command to use Homebrew's version of LLVM and create a
directory called `build`:

    $ PATH="$(brew --prefix)/opt/llvm/bin:$PATH" \
      CC=clang \
      CXX=clang++ \
      CPPFLAGS="-I$(brew --prefix)/opt/llvm/include" \
      CXXFLAGS=-D_LIBCPP_DISABLE_AVAILABILITY \
      LDFLAGS="-L$(brew --prefix)/opt/llvm/lib" \
      cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -S . -B build

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

    $ ./build/test/quick-lint-js-test

If you want to run the quick-lint-js program:

    $ ./build/quick-lint-js

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
`build\Debug\quick-lint-js.exe`. Run it from a command prompt or PowerShell
window.

---

### macOS and Linux: nix

This is for advanced users only. You need to
[install the Nix package manager](https://nixos.org/download.html) if you have
not already done so.

[Nix][] uses derivation files that contain instructions for automation on how
to build and package software. The `dist/` folder contains such derivations for
quick-lint-js and can be used to download dependencies and build it.

#### 0. Prepare environment

The derivations in `dist/` contain the necessary dependencies to build
quick-lint-js using cmake and ninja. We can use `nix-shell` to download those
dependencies and make them available for development.

    $ nix-shell dist/shell.nix

While it seems this does not do anything, you will be dropped in a shell that
has the build tools available necessary to build quick-lint-js. Once you exit
this shell, the downloaded dependencies will not be available anymore. You
can use the above command again to re-create a shell with the build tools
available at any time.

#### 1. Configure

After running above command, we configure the build environment:

    $ cmakeConfigurePhase

This will create a `build/` folder in the project folder and changes the
current directory to it.

#### 2. Build

To build quick-lint-js, make sure you are in the `build/` folder; the
previous command should have already put you there. To start the build
using ninja, you use:

    $ ninjaBuildPhase

#### 3. Run

After building, the binary is available as `quick-lint-js` in the build
folder:

    $ ./quick-lint-js

To run the tests, execute the test binary in the build directory:

    $ ./test/quick-lint-js-test

[CMake]: https://cmake.org/
[Ninja]: https://ninja-build.org/
[Visual Studio]: https://visualstudio.microsoft.com/vs/
[Nix]: https://nixos.org/manual/nix/stable/
