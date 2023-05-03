FROM debian:buster

RUN \
    apt-get update && \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list >/etc/apt/sources.list.d/quick-lint-js-sources.list && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      build-essential \
      cmake \
      debhelper \
      dpkg-dev \
      fakeroot \
      git \
      libgmock-dev \
      libgtest-dev \
      lintian \
      ninja-build \
      nodejs \
      pkg-config \
      python3.7 \
      sudo \
      tar \
      unzip \
      zip

RUN \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      ca-certificates
