# quick-lint-js winget manifest

This directory contains manifests for [Windows Package Manager][winget] aka
winget.

The `make-manifests.go` script modifies the `.yaml` files.

## Testing

To test quick-lint-js' winget manifests, perform the following steps on your
Windows machine:

1. Download a signed `quick-lint-js.msix` from a `windows` directory in
   <https://c.quick-lint-js.com/releases/>.
2. Create a directory called `windows` and put `quick-lint-js.msix` inside it.
3. Outside the `windows` directory, start a static HTTP server. For example,
   run: `npx http-server -p 8069` The HTTP server must support `GET` requests
   with `Range`. (Python's `http.server` does not support this.)
4. Build the manifests: run
   `go run dist/winget/make-manifests.go -BaseURI http://localhost:8069/ -MSIX path\to\windows\quick-lint-js.msix -OutDir test-winget-manifests`
5. Install the manifests: `winget install --manifest test-winget-manifests`

## Publishing

Every build, [CI](../../.github/workflows/build-static.yml) updates URLs in YAML
files to refer to https://c.quick-lint-js.com/builds/COMMIT_HASH/, hashes the
referenced installer files, adds the hashes to the YAML files, and uploads the
modified YAML files to https://c.quick-lint-js.com/builds/COMMIT_HASH/winget/.

When [making a release](../../docs/RELEASE.md), the release engineer updates
URLs in YAML files to refer to https://c.quick-lint-js.com/releases/VERSION/,
hashes the referenced installer files, adds the hashes to the YAML files, and
uploads the modified YAML files to
https://c.quick-lint-js.com/releases/VERSION/winget/.

[winget]: https://docs.microsoft.com/en-us/windows/package-manager/
