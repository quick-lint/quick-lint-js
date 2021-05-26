# Releasing quick-lint-js

Follow the following steps to release a new version of quick-lint-js:

1. Update version number and release date. Change these files containing version
   numbers:
   * .github/workflows/tag-build.yml
   * Formula/quick-lint-js.rb
   * dist/arch/PKGBUILD-dev
   * dist/arch/PKGBUILD-git
   * dist/arch/PKGBUILD-release
   * dist/debian/README.md
   * dist/debian/build.sh
   * dist/debian/debian/changelog
   * dist/nix/quick-lint-js.nix
   * dist/npm/BUILDING.md
   * dist/npm/package.json
   * docs/cli.adoc
   * docs/config.adoc
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
   * With the `vscode/quick-lint-js-*.vsix` artifact:
     `npx ovsx publish --packagePath ./quick-lint-js-*.vsix --pat YOUR_ACCESS_TOKEN`
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

10. Update Arch Linux user repositories (AUR):
    1. Clone ssh://aur@aur.archlinux.org/quick-lint-js with Git.
    2. Update README to point to the tag's commit.
    3. Copy `dist/arch/PKGBUILD-release` into the checkout as `PKGBUILD`.
    4. On Arch Linux, run `makepkg --printsrcinfo PKGBUILD >.SRCINFO`.
    5. Commit all files with message "Update quick-lint-js to version
       VERSION_NUMBER".
    6. Push to the `master` branch on AUR.
