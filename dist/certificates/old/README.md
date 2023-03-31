# Outdated code signing certificates

This directory contains Public Key Infrastructure (PKI) certificates used for
code-signing past quick-lint-js release. These certificates are no longer in
use.

## Files

* `quick-lint-js.gpg.key`: GPG key (public key). Use this file to verify GPG
  signatures of quick-lint-js.
* `quick-lint-js.cer`: Self-signed certificate (public key). Use this file to
  verify signatures of quick-lint-js on macOS and Windows.
  * SHA1: dc4f675b74b3a86c1f59fbdac17538b7d996db99
* `DigiCertAssuredIDRootCA_comb.crt.pem`: DigiCert's certificate. Use this file
  to verify timestamp signatures. Downloaded from
  <https://knowledge.digicert.com/generalinformation/INFO4231.html>.
* `DigiCertTrustedRootG4.crt`: DigiCert's certificate. Use this file
  to verify timestamp signatures. Downloaded from
  <https://www.digicert.com/kb/digicert-root-certificates.htm>.
