# Code-signing quick-lint-js

## macOS

In order to sign quick-lint-js, you need a code signing certificate. You can
create a self-signed certificate for local testing (but not distribution):

1. [Create a self-signed CA for code signing][macos-create-ca].
2. [Create a certificate for code signing][macos-create-cert].

In the Keychain Access app, export your code signing certificate (not CA)'s
public key as a .cer file. Call it `dist/certificates/quick-lint-js.cer`.

When you run the `dist/sign-release.go` program, specify
`-AppleCodesignIdentity COMMON_NAME_OF_YOUR_KEY`.

[macos-create-ca]: https://www.simplified.guide/macos/keychain-ca-code-signing-create
[macos-create-cert]: https://www.simplified.guide/macos/keychain-cert-code-signing-create
