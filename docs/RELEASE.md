# Releasing quick-lint-js

Follow the following steps to release a new version of quick-lint-js:

1. Update version number. Change these files containing version numbers:
   * dist/debian/README.md
   * dist/debian/build.sh
   * dist/debian/debian/changelog
   * dist/nix/quick-lint-js.nix
   * dist/npm/BUILDING.md
   * dist/npm/package.json
   * plugin/vscode-lsp/README.md
   * plugin/vscode-lsp/package.json
   * plugin/vscode/BUILDING.md
   * plugin/vscode/package.json
   * src/quick-lint-js/version.h
   * website/install.html

2. Create a commit. Push it to GitHub on a non-main branch.

3. Download artifacts from GitHub Actions workflows:
   * plugin-vscode-COMMIT
   * quick-lint-js-npm-COMMIT

4. Open the quick-lint-js-npm-COMMIT artifact. Audit the copyright files.

5. Create a Git tag named after the version number (e.g. `0.1.0`). Push it to
   GitHub.

6. Push the commit to the `master` branch on GitHub.

7. Wait for all GitHub actions builds on `master` to finish.

8. ssh into the arifact server (c.quick-lint-js.com). Copy
   `/var/www/c.quick-lint-js.com/builds/%YOUR_COMMIT_HASH%` to
   `/var/www/c.quick-lint-js.com/releases/%YOUR_VERSION_NUMBER%`.

9. Publish the packages:
   * With the plugin-vscode-COMMIT artifact:
     `npx vsce publish --packagePath ./quick-lint-js-*.vsix`
   * With the quick-lint-js-npm-COMMIT artifact:
     `npm publish ./quick-lint-js-*.tgz`
   * Run the `dist/debian/sync-releases-to-apt` script.

10. Publish the website:
   1. Visit
      https://github.com/quick-lint/quick-lint-js/actions/workflows/deploy-website.yml?query=workflow%3A%22deploy+website%22
   2. Click the "Run workflow" menu, enter the commit's hash, then click "Run
      workflow" button.
