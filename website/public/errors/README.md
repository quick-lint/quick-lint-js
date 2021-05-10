# quick-lint-js errors

This directory contains documentation on quick-lint-js' warnings and errors.

The source documents can be found in [docs/errors/](../../../docs/errors/). This
directory contains HTML generated from those source documents.

## Generating

    $ # Build the generator. See docs/BUILDING.md for more details.
    $ mkdir build ; cd build ; cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug .. ; cd "$OLDPWD"
    $ ninja -C build quick-lint-js-generate-error-docs

    $ # Generate index.html:
    $ ./build/docs/quick-lint-js-generate-error-docs \
      docs/errors/ website/public/errors/index.template.html website/public/errors/index.html
