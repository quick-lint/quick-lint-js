# quick-lint-js infrastructure

This directory contains scripts and other stuff for hosting the quick-lint-js
website and related stuff.

## Files and folders

* `quick-lint-js-web-docker.service`: systemd service for running the
  quick-lint-js-web Docker container. Installed into
  `/lib/systemd/system/quick-lint-js-web-docker.service` on `quick-lint-js.com`.
* `quick-lint-js-web`: Configuration and Dockerfile for the HTTP server.
