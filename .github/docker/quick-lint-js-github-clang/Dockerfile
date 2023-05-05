FROM debian:bullseye

RUN \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list >/etc/apt/sources.list.d/quick-lint-js-sources.list && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      clang-13 \
      cmake \
      git \
      libc++-13-dev \
      libc++abi-13-dev \
      libc6-dev \
      libstdc++-10-dev \
      ninja-build \
      nodejs \
      python3
