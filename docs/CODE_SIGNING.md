# Code-signing quick-lint-js

## macOS

### Development

Import the development CA and certificate into your keychain (**warning:
trusting these certificates is a bad idea**):

    $ security import dist/certificates/apple-dev-ca.p12 -P ""
    $ security import dist/certificates/apple-dev.p12 -P ""

When running CMake, set the `QUICK_LINT_JS_ENABLE_APPLE_CODESIGN` option to
`TRUE`. For example:

    $ cmake -G Ninja \
      -DCMAKE_BUILD_TYPE=Debug \
      -DQUICK_LINT_JS_ENABLE_APPLE_CODESIGN=TRUE \
      -S . -B build

Build the `quick-lint-js.signed` target. The signed CLI executable should be
`build/quick-lint-js.signed`.

@@@ bug: if codesign fails, re-running Ninja does not re-run codesign, 'cus
quick-lint-js.signed is left over.

@@@ todo: verify that signature is checked
$ spctl --assess --type execute myTool
