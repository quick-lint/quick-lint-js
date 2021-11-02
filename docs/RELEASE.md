# Releasing quick-lint-js

Follow the following steps to release a new version of quick-lint-js:

1. Update the release notes file: docs/CHANGELOG.md

2. Update version number and release date. Change these files containing version
   numbers:
   * Formula/quick-lint-js.rb
   * dist/arch/PKGBUILD-dev
   * dist/arch/PKGBUILD-git
   * dist/arch/PKGBUILD-release
   * dist/debian/README.md
   * dist/debian/debian/changelog
   * dist/npm/BUILDING.md
   * dist/npm/package.json
   * plugin/vim/quick-lint-js.vim/doc/quick-lint-js.txt
   * plugin/vscode-lsp/README.md
   * plugin/vscode-lsp/package.json
   * plugin/vscode/BUILDING.md
   * plugin/vscode/package.json
   * version

3. Re-generate man pages to include the updated version number by running
   `./docs/man/generate-man-pages`.

4. Create a commit. Push it to GitHub on a non-main branch on
   https://github.com/quick-lint/quick-lint-js (not a fork).

5. Wait for all GitHub Actions workflows to finish and to succeed.

6. Download the build artifacts from the artifact server:
   `rsync -av c.quick-lint-js.com:/var/www/c.quick-lint-js.com/builds/$YOUR_COMMIT_HASH/ builds/`

7. Sign the build artifacts:
   `go run dist/sign-releases.go -AppleCodesignIdentity=quick-lint-js -PrivateKeyPKCS12=dist/certificates/quick-lint-js-PRIVATE.p12 builds/ signed-builds/`
   * **Warning**: This signing command only works on macOS hosts.

8. Upload the signed build artifacts to the artifact server:
   `rsync -av signed-builds/ c.quick-lint-js.com:/var/www/c.quick-lint-js.com/releases/$YOUR_VERSION_NUMBER/`

9. Publish the packages:
   * With the `vscode/quick-lint-js-*.vsix` artifact:
     `npx vsce publish --packagePath signed-builds/quick-lint-js-*.vsix`
   * With the `vscode/quick-lint-js-*.vsix` artifact:
     `npx ovsx publish signed-builds/quick-lint-js-*.vsix --pat YOUR_ACCESS_TOKEN`
   * With the `npm/quick-lint-js-*.tgz` artifact:
     `npm publish signed-builds/quick-lint-js-*.tgz`
   * Run the `dist/debian/sync-releases-to-apt` script.

10. Publish the website: Run `./website/deploy.sh COMMIT_HASH_HERE`.

11. Create a Git tag named after the version number (e.g. `0.1.0`). Push it to
    GitHub.

12. Push the commit to the `master` branch on GitHub.

13. Update Arch Linux user repositories (AUR):
    1. Clone ssh://aur@aur.archlinux.org/quick-lint-js with Git.
    2. Update README to point to the tag's commit.
    3. Run `dist/arch/update-aur.sh --docker --test /path/to/quick-lint-js-aur-clone`.
    4. Commit all files with message "Update quick-lint-js to version
       VERSION_NUMBER".
    5. Push to the `master` branch on AUR.
