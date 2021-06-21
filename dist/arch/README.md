# quick-lint-js Arch Linux package

This directory contains Arch Linux source packages, meant for distribution on
the AUR.

This directory contains three different PKGBUILD files:

* `PKGBUILD-dev` builds from the current Git checkout.
* `PKGBUILD-git` downloads the latest development (unreleased) version of
  quick-lint-js.
* `PKGBUILD-release` downloads the latest official released version of
  quick-lint-js.

## Building

To build the quick-lint-js package, on an Arch Linux installation, run the
following commands:

    $ sudo pacman -S base-devel git namcap
    $ cd dist/arch/  # Navigate to this directory.
    # If you want to build PKGBUILD-git or PKGBUILD-release, replace
    # 'PKGBUILD-dev' in the following command:
    $ makepkg --syncdeps --cleanbuild -p PKGBUILD-dev
    $ namcap PKGBUILD-dev PKGBUILD-git PKGBUILD-release ./quick-lint-js-*.pkg.tar.zst

To install the built package, run the following command:

    $ sudo pacman -U ./quick-lint-js-*.pkg.tar.zst
