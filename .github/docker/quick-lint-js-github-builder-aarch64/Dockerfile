FROM debian:bookworm

RUN \
    apt-get update && \
    sed -i -e 's/^Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/debian.sources && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      build-essential \
      cmake \
      debhelper \
      dpkg-dev \
      fakeroot \
      git \
      libgmock-dev \
      libgtest-dev \
      libsimdjson-dev \
      lintian \
      ninja-build \
      nodejs \
      pkg-config \
      python3 \
      sudo \
      tar \
      unzip \
      zip

RUN \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      ca-certificates
