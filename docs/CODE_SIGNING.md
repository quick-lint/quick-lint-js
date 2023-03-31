# Code-signing quick-lint-js

In order to sign quick-lint-js, you need a code signing certificate.

## Creating a certificate

You can create a self-signed certificate for local testing (but not
distribution):

1. [Create a self-signed CA for code signing][macos-create-ca].
2. [Create a certificate for code signing][macos-create-cert].

You will also need a GnuPG key. Generate one with an RSA key of at least 4096
bits.

## Creating signing files

After creating a code signing certificate, you need to create files for signing.

In the Keychain Access app, export your code signing certificate (not CA)'s
public key as a .cer file. Call it `dist/certificates/quick-lint-js.cer`.

Then, convert the `.cer` DER (binary) file into a `.crt` PEM (text) file by
running the following command:

    $ openssl x509 -in dist/certificates/quick-lint-js.cer -inform der -out dist/certificates/quick-lint-js.crt

After creating the `.crt` file, [update the macOS code signing requirements
file][apple-csreq].

In the Keychain Access app, export your code signing certificate (not CA) and
private key as a .p12 file. Call it
`dist/certificates/quick-lint-js-PRIVATE.p12`. **Do not commit this file.**

Then, convert the `.p12` file into an `.key` RSA key file by running the
following command (**do not commit the `.key` file**):

    $ openssl pkcs12 -in dist/certificates/quick-lint-js-PRIVATE.p12 -nocerts -out /dev/stdout -passout pass:temporarypass | openssl rsa -in /dev/stdin -out dist/certificates/quick-lint-js-PRIVATE.key -passin pass:temporarypass

To export the GnuPG key, run the following commands (**do not commit the
`quick-lint-js-PRIVATE.gpg.key` file**):

    $ gpg --output dist/certificates/quick-lint-js.gpg.key --armor --export $YOUR_KEY_FINGERPRINT
    $ gpg --output dist/certificates/quick-lint-js-PRIVATE.gpg.key --export-secret-key $YOUR_KEY_FINGERPRINT

## Signing

When you run the `dist/sign-release.go` program, specify
`-RelicConfig dist/certificates/relic-config.yaml`.

## Updating production certificates

After sacrificing a goat to the PKI gods by purchasing a signing certificate
from a certificate authority (e.g. [SSL.com][]):

1. Open Keychain Access.
2. Right-click the private key.
3. Choose "Request a Certificate From a Certificate Authority".
4. Fill in the Common Name (CN) based on the CN provided by your certificate
   authority.
5. Give the CSR to your certificate authority.
6. Download the certificate chain .crt file, saving it to
   `dist/certificates/quick-lint-js.crt`.
7. [Update the macOS code signing requirements file.][apple-csreq]

[SSL.com]: https://www.ssl.com/
[macos-create-ca]: https://www.simplified.guide/macos/keychain-ca-code-signing-create
[macos-create-cert]: https://www.simplified.guide/macos/keychain-cert-code-signing-create
[apple-csreq]: apple/README.md
