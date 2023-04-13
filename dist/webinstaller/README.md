# quick-lint-js webinstaller manifest

This directory contains manifests for [webinstaller][], a developer tools installer.

The `make-webinstaller-manifest.js` script insert the ouput file with the appropriate release version number and its release date.

## Manually building the manifest

Run:
   `node  dist/webinstaller/make-webinstaller-manifest.js -BaseURI https://c.quick-lint-js.com/releases/RELEASE_VERSION/ -releaseDate RELEASE_DATE -Out OUTPUT_FILE.json`
