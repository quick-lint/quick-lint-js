# quick-lint-js-github-emacs Docker image

Note: This Docker image depends on
[ghcr.io/quick-lint/quick-lint-js-github-gcc][].

## Updating the Docker image

Pick a version number for the new Docker image (e.g. `v3`), then run the
following commands:

    $ docker build --tag ghcr.io/quick-lint/quick-lint-js-github-emacs:VERSION_NUMBER_HERE .github/docker/quick-lint-js-github-emacs/
    $ docker login ghcr.io -u YOUR_GITHUB_USER_NAME_HERE
    $ docker push ghcr.io/quick-lint/quick-lint-js-github-emacs:VERSION_NUMBER_HERE

Then, change the container tag in each workflow file in the .github/workflows/
directory to refer to your new version.

[ghcr.io/quick-lint/quick-lint-js-github-gcc]: ../quick-lint-js-github-gcc/
