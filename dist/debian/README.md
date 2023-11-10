# quick-lint-js Debian package

This directory contains a Debian source package, which are rules for installing
quick-lint-js on Debian, Ubuntu, and other Linux distributions.

## Building

On a Debian-based machine, install dependencies for .deb generation:

    # libgmock-dev, libgtest-dev, libsimdjson-dev, and pkg-config are optional
    # when building with --bionic.
    $ sudo apt-get install build-essential cmake debhelper dpkg-dev fakeroot git libgmock-dev libgtest-dev libsimdjson-dev lintian pkg-config

Then, run the `build.sh` script:

    # If you are on an old distribution (such as Ubuntu 18.04 Bionic), give
    # the '--bionic' flag to build.sh.
    $ ./dist/debian/build.sh

The above command will create `dist/debian/quick-lint-js_2.4.2-1_amd64.deb`,
`dist/debian/quick-lint-js-vim_2.4.2-1_all.deb`, and related files.

## Installing

On a Debian-based system, after building the .deb file (per the above
instructions), install the .deb file:

    $ sudo apt-get install ./dist/debian/quick-lint-js_2.4.2-1_amd64.deb
    $ sudo apt-get install ./dist/debian/quick-lint-js-vim_2.4.2-1_all.deb

## AppStream

`asgen-config.json` configures [appstream-generator][]. appstream-generator is
invoked by the `sync-releases-to-apt` script to expose AppStream metadata to
apt. This metadata allows quick-lint-js to appear in the Pop!\_Shop, for
example.

To test `asgen-config.json` or metadata changes locally:

1. Install the `appstream-generator` Debian package.
   * You might need to work around
     <https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=959893>.
2. Create a new empty directory called `debian`.
3. Copy `dist/debian/apt-ftparchive.conf` from quick-lint-js' source code into
   the `debian` directory.
4. Copy `dist/debian/asgen-config.json` from quick-lint-js' source code into the
   `debian` directory. Edit `debian/asgen-config.json`:
   * Change `MediaBaseUrl` to `"http://localhost:8069/appstream/export/media/"`.
   * Change `HtmlBaseUrl` to `"http://localhost:8069/appstream/export/html/"`.
5. Create a directory `debian/pool/`.
6. Copy `dist/debian/*2.18.0*` (built by the [Building](#Building) instructions
   above) into the `debian/pool/` directory.
7. Run `./dist/debian/update-repository path/to/debian`.
8. Start an HTTP server in the `debian` directory. For example:
    `cd /path/to/debian && python -m http.server 8068`

After this setup, you can visit <http://localhost:8069/appstream/export/html/>
in your web browser to look at warnings from appstream-generator.

To view in the Pop!\_Shop:

1. Add the following entry to `/etc/apt/sources.list.d/quick-lint-js.list`:
   `deb [arch=amd64 trusted=yes allow-insecure=yes] http://localhost:8069/ experimental main`
2. Run `sudo apt update`.
3. Open the Pop!\_Shop.
4. Search for "quick-lint". (For some reason, searching for "quick-lint-js"
   shows no results.)

## Releasing downstream

To release to downstream Debian, we [ship a source package to Debian mentors][].

1. Download a signed release .tar.gz and .tar.gz.asc (e.g. from
   <https://c.quick-lint-js.com/releases/latest/source/>).
2. Create a package using `package.sh`:
   `./dist/debian/package.sh --output-directory debian-package --orig path/to/quick-lint-js-2.18.0.tar.gz --sign`
   * NOTE: `package.sh` will use the `debian` sources from your checkout
     (`./dist/debian/debian/`), not from the signed release tarball.
3. Upload the package: `dput mentors debian-package/quick-lint-js_2.4.2-1_source.changes`

## Signing

### Update expired signing key

    # Update the expiration date of the key.
    $ gpg --edit-key AEB5AF8EC17B8516781C1572DF275514A27D9439
    gpg> expire
    Key is valid for? (0) 3y
    gpg> key DF275514A27D9439
    gpg> expire
    Key is valid for? (0) 3y
    gpg> save

    # Publish the key to c.quick-lint-js.com.
    $ gpg --armor --export AEB5AF8EC17B8516781C1572DF275514A27D9439 >quick-lint-js-release.key
    $ # Replace '2022' with the year of the old key.
    $ rsync --backup --suffix=.old-2022 quick-lint-js-release.key root@c.quick-lint-js.com:/var/www/c.quick-lint-js.com/quick-lint-js-release.key

    # Re-sign the apt repository.
    $ ./dist/debian/sync-releases-to-apt

    # Somehow let people know that they need to run the following command (from
    # the Debian install instructions):
    # $ curl https://c.quick-lint-js.com/quick-lint-js-release.key | sudo apt-key add -

[appstream-generator]: https://github.com/ximion/appstream-generator
[debian-mentors]: https://mentors.debian.net/intro-maintainers/
