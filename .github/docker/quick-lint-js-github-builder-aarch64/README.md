# quick-lint-js-github-builder-aarch64 Docker image

## Updating the Docker image

Pick a version number for the new Docker image (e.g. `v3`), then run the
following commands **on an AArch64 (ARM64) machine**:

    $ docker build --tag ghcr.io/quick-lint/quick-lint-js-github-builder-aarch64:VERSION_NUMBER_HERE .github/docker/quick-lint-js-github-builder-aarch64/
    $ docker login ghcr.io -u YOUR_GITHUB_USER_NAME_HERE
    $ docker push ghcr.io/quick-lint/quick-lint-js-github-builder-aarch64:VERSION_NUMBER_HERE

Then, change the container tag in each workflow file in the .github/workflows/
directory to refer to your new version.
