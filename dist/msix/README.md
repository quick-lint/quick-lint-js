# quick-lint-js MSIX

This directory contains files to build a Windows MSIX package for quick-lint-js.

To build an MSIX file, obtain `quick-lint-js.exe` and a
copyright file then run the following command on Windows:

    $ go run .\dist\msix\build-unsigned-msix.go -EXE .\path\to\quick-lint-js.exe -License .\path\to\copyright.txt -Out .\quick-lint-js.msix

After creating an MSIX, in order to install it, you must
sign the MSIX using `signtool` or `Relic` (or our `sign-release.go` script).
`signtool` example:

    $ signtool sign /fd SHA256 /a /f .\path\to\privatekey.pfx /p PRIVATEKEYPASSWORD .\quick-lint-js.msix

## Files

* AppxManifest.xml
* images/Square150x150Logo.png: dusty-right.svg padded and rasterized as a
  300x300-pixel PNG.
* images/Square44x44Logo.png: dusty-favicon.svg rasterized as a 88x88-pixel PNG.
* images/StoreLogo.png: dusty-left.svg padded and rasterized as a 300x300-pixel
  PNG.
* mapping.template.txt: Template manifest file for `makeappx`. Variables are
  substituted by `build-unsigned-msix.go`.
