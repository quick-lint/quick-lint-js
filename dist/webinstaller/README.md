# quick-lint-js webinstaller manifest

This directory contains manifests for [webinstaller](https://webinstall.dev/), a developer tools installer.

The `make-webinstaller-manifest.js` script creates the output file with the appropriate release version number and its release date.

## Manually building the manifest

Run:

```sh
node  dist/webinstaller/make-webinstaller-manifest.js -releaseVersion VERSION -releaseDate RELEASE_DATE -Out OUTPUT_FILE.json
```
