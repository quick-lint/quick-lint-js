# Code signing certificates

This directory contains Public Key Infrastructure (PKI) certificates used for
code-signing quick-lint-js.

For information on how to use these files, see our [code signing
documentation](../../docs/CODE_SIGNING.md).

## Files

* `quick-lint-js.cer`: Self-signed certificate (public key). Use this file to
  verify signatures of quick-lint-js on macOS.
  * SHA1: dc4f675b74b3a86c1f59fbdac17538b7d996db99
* `quick-lint-js.gpg.key`: GPG key (public key). Use this file to verify GPG
  signatures of quick-lint-js.
