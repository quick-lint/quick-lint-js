# quick-lint-js webinstaller manifest

This directory contains manifests for [webinstaller](https://webinstall.dev/), a developer tools installer.

The `make-webinstaller-manifest.js` script creates the output file with the appropriate release version number and its release date, then add content of the output file into `all-versions-manifest/all-versions-manifest.json` which hosts all the data of every release.

## Manually building the manifest

Run:

```sh
node  dist/webinstaller/make-webinstaller-manifest.js -releaseVersion VERSION -releaseDate RELEASE_DATE -Out OUTPUT_FILE.json
```
