# Installing quick-lint-js

## Installing from source

Even though quick-lint-js is still in early development, you might want to
experiment with installing the binary in your environment. Currently we do not
offer pre-built binaries so quick-lint-js needs to be installed from source.

Depending on how you want to build quick-lint-js, the installation instructions
might differ.

* [macOS and Linux: nix](#macos-and-linux-nix)

---

### macOS and Linux: nix

This is for advanced users only. You need to
[install the Nix package manager](https://nixos.org/download.html) if you have
not already done so.

#### Build and Install

[Nix][] uses derivation files that contain instructions for automation on how
to build and package software. The `dist/` folder contains such derivations and
can be used to build and install quick-lint-js from source. Nix will automatically
download all dependencies, so installing dependencies other than nix yourself is
not necessary.

Use `nix-env -i` to build and install the binary all in one go:

    $ nix-env -f dist -i

Now the binary is available in your user path:

    $ quick-lint-js --help

#### Uninstall

Use `nix-env -e` to uninstall a previously installed binary from your user env:

    $ nix-env -e quick-lint-js

[Nix]: https://nixos.org/features.html
