# Releasing quick-lint-js

Follow the following steps to release a new version of quick-lint-js:

1. Update version number and release date. Change these files containing version
   numbers:
   * dist/debian/README.md
   * dist/debian/build.sh
   * dist/debian/debian/changelog
   * dist/nix/quick-lint-js.nix
   * dist/npm/BUILDING.md
   * dist/npm/package.json
   * docs/quick-lint-js.1
   * plugin/vscode-lsp/README.md
   * plugin/vscode-lsp/package.json
   * plugin/vscode/BUILDING.md
   * plugin/vscode/package.json
   * src/quick-lint-js/version.h
   * website/index.html
   * website/install.html

2. Create a commit. Push it to GitHub on a non-main branch on
   https://github.com/quick-lint/quick-lint-js (not a fork).

3. Wait for all GitHub Actions workflows to finish and to succeed.

4. Download the following artifacts from the artifact server:
   * `https://c.quick-lint-js.com/builds/%YOUR_COMMIT_HASH%/vscode/quick-lint-js-%YOUR_VERSION_NUMBER%.vsix`
   * `https://c.quick-lint-js.com/builds/%YOUR_COMMIT_HASH%/npm/quick-lint-js-%YOUR_VERSION_NUMBER%.tgz`

5. ssh into the artifact server (c.quick-lint-js.com). Copy
   `/var/www/c.quick-lint-js.com/builds/%YOUR_COMMIT_HASH%` to
   `/var/www/c.quick-lint-js.com/releases/%YOUR_VERSION_NUMBER%`.

6. Publish the packages:
   * With the `vscode/quick-lint-js-*.vsix` artifact:
     `npx vsce publish --packagePath ./quick-lint-js-*.vsix`
   * With the `npm/quick-lint-js-*.tgz` artifact:
     `npm publish ./quick-lint-js-*.tgz`
   * Run the `dist/debian/sync-releases-to-apt` script.

7. Publish the website:
   1. Visit
      https://github.com/quick-lint/quick-lint-js/actions/workflows/deploy-website.yml?query=workflow%3A%22deploy+website%22
   2. Click the "Run workflow" menu, select your branch, enter the commit's
      hash, then click "Run workflow" button.

8. Create a Git tag named after the version number (e.g. `0.1.0`). Push it to
   GitHub.

9. Push the commit to the `master` branch on GitHub.
