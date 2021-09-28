# Code-signing quick-lint-js

## macOS

### Prerequisites

In order to sign quick-lint-js, you need a code signing certificate. You can
create a self-signed certificate for local testing (but not distribution):

1. [Create a self-signed CA for code signing][macos-create-ca].
2. [Create a certificate for code signing][macos-create-cert].

In the Keychain Access app, export your code signing certificate (not CA)'s
certificate as a .cer file. We'll call this .cer file
`/path/to/code-sign-cert.cer`.

### Building

When running CMake, set the `QUICK_LINT_JS_ENABLE_APPLE_CODE_SIGN` option to
`TRUE`, and set the `QUICK_LINT_JS_APPLE_CODE_SIGN_IDENTITY` option to your
certificate's Common Name (CN). For example:

    $ cmake -G Ninja \
      -DCMAKE_BUILD_TYPE=Debug \
      -DQUICK_LINT_JS_ENABLE_APPLE_CODE_SIGN=TRUE \
      -DQUICK_LINT_JS_APPLE_CODE_SIGN_IDENTITY="quick-lint-js dev (DO NOT TRUST)" \
      -S . -B build

Build the `quick-lint-js` target. The signed CLI executable should be
`build/quick-lint-js`.

Verify the signature embedded in the CLI executable:

    $ codesign -vvv -R="$(csreq -r='certificate leaf = "/path/to/code-sign-cert.cer"' -t)" build/quick-lint-js
    build/quick-lint-js: valid on disk
    build/quick-lint-js: satisfies its Designated Requirement
    build/quick-lint-js: explicit requirement satisfied

### CI/CD

Our CI system (GitHub Actions) signs macOS binaries. The key is specified using
the `APPLE_CODE_SIGN_KEY_PKCS12_PEM` repository secret on GitHub. To change the
key, run the following steps:

1. Open Keychain Access.
2. Select both the *certificate* and the *private key* (but not the *public
   key*). (Use CMD-LeftClick to select multiple items.)
3. Export the items to the .p12 (PKCS12) format.
4. Run the following command to convert the .p12 into a text-based format used
   by our CI jobs:

    $ base64 --input YOUR_FILE_HERE.p12 --break 64

5. [Add the secret on GitHub][github-secret].

[github-secret]: https://github.com/quick-lint/quick-lint-js/settings/secrets/actions
[macos-create-ca]: https://www.simplified.guide/macos/keychain-ca-code-signing-create
[macos-create-cert]: https://www.simplified.guide/macos/keychain-cert-code-signing-create
