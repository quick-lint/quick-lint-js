# quick-lint-js Debian package

This directory contains a Debian source package, which are rules for installing
quick-lint-js on Debian, Ubuntu, and other Linux distributions.

## Building

On a Debian-based machine, install dependencies for .deb generation:

    $ sudo apt-get install cmake debhelper dpkg-dev gcc-9 g++-9 git lintian

Then, run the `build.sh` script:

    $ ./dist/debian/build.sh

The above command will create `dist/debian/quick-lint-js_2.3.1-1_amd64.deb`,
`dist/debian/quick-lint-js-vim_2.3.1-1_all.deb`, and related files.

## Installing

On a Debian-based system, after building the .deb file (per the above
instructions), install the .deb file:

    $ sudo apt-get install ./dist/debian/quick-lint-js_2.3.1-1_amd64.deb
    $ sudo apt-get install ./dist/debian/quick-lint-js-vim_2.3.1-1_all.deb

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
6. Copy `dist/debian/*2.3.1*` (built by the [Building](#Building) instructions
   above) into the `debian/pool/` directory.
7. Run `./dist/debian/update-repository path/to/debian`.
8. Start an HTTP server in the `debian` directory. For example:
    `cd /path/to/debian && python -m http.server 8068`

After this setup, you can visit <http://localhost:8069/appstream/export/html/>
in your web browser to look at warnings from appstream-generator.

To view in the Pop!\_Shop:

1. Add the following entry to `/etc/apt/sources.list.d/quick-lint-js.list`:
   `deb [arch=amd64 trusted=yes allow-insecure=yes] http://172.17.0.1:8069/ experimental main`
2. Run `sudo apt update`.
3. Open the Pop!\_Shop.
4. Search for "quick-lint". (For some reason, searching for "quick-lint-js"
   shows no results.)

[appstream-generator]: https://github.com/ximion/appstream-generator
