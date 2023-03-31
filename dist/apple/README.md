This directory contains files related to distribution on Apple operating systems
(such as macOS).

## `quick-lint-js.csreq`

`quick-lint-js.csreq` is generated from the following command:

    $ csreq -r <(printf 'identifier "quick-lint-js" and certificate leaf = H"%s"\n' "$(openssl x509 -in ./dist/certificates/quick-lint-js.crt -outform der | shasum -a 1 | cut -c1-40)") -b dist/apple/quick-lint-js.csreq
