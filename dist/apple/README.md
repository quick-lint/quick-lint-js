This directory contains files related to distribution on Apple operating systems
(such as macOS).

## `quick-lint-js.csreq`

`quick-lint-js.csreq` is generated from the following command:

    $ csreq -r <(printf 'identifier "quick-lint-js" and certificate leaf = H"%s"\n' "$(shasum -a 1 ./dist/certificates/quick-lint-js.cer | cut -c1-40)") -b dist/apple/quick-lint-js.csreq
