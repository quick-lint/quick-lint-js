FROM debian:bullseye

RUN \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list >/etc/apt/sources.list.d/quick-lint-js-sources.list && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      ca-certificates \
      cmake \
      g++ \
      git \
      libc6-dev \
      make \
      ninja-build \
      nodejs \
      python3
