# Code-signing quick-lint-js

In order to sign quick-lint-js, you need a code signing certificate. You can
create a self-signed certificate for local testing (but not distribution):

1. [Create a self-signed CA for code signing][macos-create-ca].
2. [Create a certificate for code signing][macos-create-cert].

In the Keychain Access app, export your code signing certificate (not CA)'s
public key as a .cer file. Call it `dist/certificates/quick-lint-js.cer`.

In the Keychain Access app, export your code signing certificate (not CA) and
private key as a .p12 file. Call it
`dist/certificates/quick-lint-js-PRIVATE.p12`. **Do not commit this file.**

Then, convert the `.p12` file into an `.key` RSA key file by running the
following command (**do not commit the `.key` file**):

    $ openssl pkcs12 -in dist/certificates/quick-lint-js-PRIVATE.p12 -nocerts -out /dev/stdout -passout pass:temporarypass | openssl rsa -in /dev/stdin -out dist/certificates/quick-lint-js-PRIVATE.key -passin pass:temporarypass

You will also need a GnuPG key. Create the key, then export the public key to
`dist/certificates/quick-lint-js.gpg.key`.

When you run the `dist/sign-release.go` program, specify
`-GPGIdentity GPG_KEY_FINGERPRINT -RelicConfig dist/certificates/relic-config.yaml`.

[macos-create-ca]: https://www.simplified.guide/macos/keychain-ca-code-signing-create
[macos-create-cert]: https://www.simplified.guide/macos/keychain-cert-code-signing-create
