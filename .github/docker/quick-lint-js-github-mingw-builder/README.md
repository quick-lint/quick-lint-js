# quick-lint-js-github-mingw-builder Docker image

## Updating the Docker image

First, clone the quick-lint fork of the LLVM MinGW repository into the
llvm-mingw directory:

    $ git clone https://github.com/quick-lint/llvm-mingw .github/docker/quick-lint-js-github-mingw-builder/llvm-mingw

Pick a version number for the new Docker image (e.g. `v3`), then run the
following commands:

    $ docker build --tag ghcr.io/quick-lint/quick-lint-js-github-mingw-builder:VERSION_NUMBER_HERE .github/docker/quick-lint-js-github-mingw-builder/
    $ docker login ghcr.io -u YOUR_GITHUB_USER_NAME_HERE
    $ docker push ghcr.io/quick-lint/quick-lint-js-github-mingw-builder:VERSION_NUMBER_HERE

Then, change the container tag in each workflow file in the .github/workflows/
directory to refer to your new version.

If you run out of memory during `docker build`, reduce parallelization with
`--build-arg CORES=4` for example.
