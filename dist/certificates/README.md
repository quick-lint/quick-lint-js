# Code signing certificates

This directory contains Public Key Infrastructure (PKI) certificates used for
code-signing quick-lint-js.

For information on how to use these files, see our [code signing
documentation](../../docs/CODE_SIGNING.md).

## Files

* `quick-lint-js.crt`: CA-signed certificate (public key). Use this file to
  verify signatures of quick-lint-js on macOS and Windows.
  * SHA1 for leaf: 15764dadd7d55ae74a614e7932896478251b7375
* `quick-lint-js.gpg.key`: GPG key (public key). Use this file to verify GPG
  signatures of quick-lint-js.
* `SSL_COM_ROOT_CERTIFICATION_AUTHORITY_RSA.crt`: SSL.com's certificate. Use
  this file to verify timestamp signatures and `quick-lint-js.crt`. Downloaded
  from <https://www.ssl.com/how-to/install-ssl-com-ca-root-certificates/>.
