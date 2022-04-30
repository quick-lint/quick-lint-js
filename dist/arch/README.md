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
    $ ./lint.sh

To install the built package, run the following command:

    $ sudo pacman -U ./quick-lint-js-*.pkg.tar.zst

## Docker image

For convenience, we have a Docker image based on Arch Linux.

### Updating the Docker image

Pick a version number for the new Docker image (e.g. `v3`), then run the
following commands:

    $ tar ch -C dist/arch/ . | docker build --tag ghcr.io/quick-lint/quick-lint-js-dist-arch:VERSION_NUMBER_HERE -
    $ docker login ghcr.io -u YOUR_GITHUB_USER_NAME_HERE
    $ docker push ghcr.io/quick-lint/quick-lint-js-dist-arch:VERSION_NUMBER_HERE

Then, change the container tag in `update-aur.sh` refer to your new version.
