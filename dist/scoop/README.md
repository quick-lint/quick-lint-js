# quick-lint-js Scoop manifest

This directory contains manifests for [Scoop][], a Windows
package manager.

The `make-manifest.go` script modifies `quick-lint-js.json`.

## Manually building the manifest

1. Download `windows.zip` and `windows-x86.zip` from
   <https://c.quick-lint-js.com/>.
2. Run:
   `go run dist/scoop/make-manifest.go -BaseURI https://c.quick-lint-js.com/builds/BUILD_COMMIT_HASH/ -x86-ZIP windows-x86.zip -x64-ZIP windows.zip -Out quick-lint-js.json`
3. Test installation: `scoop install .\quick-lint-js.json`

## Publishing

Every build, [CI](../../.github/workflows/build-static.yml) updates
`quick-lint-js.json`'s URLs to refer to
https://c.quick-lint-js.com/builds/COMMIT_HASH/, hashes the referenced .zip
files, adds the hashes to the manifest, and uploads the modified manifest to
https://c.quick-lint-js.com/builds/COMMIT_HASH/scoop/quick-lint-js.json.

When [making a release](../../docs/RELEASE.md), the release engineer updates
`quick-lint-js.json`'s URLs to refer to
https://c.quick-lint-js.com/releases/VERSION/, hashes the referenced .zip files,
adds the hashes to the manifest, and uploads the modified manifest to
https://c.quick-lint-js.com/releases/VERSION/scoop/quick-lint-js.json.

[Scoop]: https://scoop.sh/
