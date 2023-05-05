FROM debian:buster

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      gnupg \
      software-properties-common && \
    curl https://apt.kitware.com/keys/kitware-archive-latest.asc | apt-key add - && \
    echo 'deb  http://apt.kitware.com/ubuntu/ bionic main' >/etc/apt/sources.list.d/kitware.list && \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list >/etc/apt/sources.list.d/quick-lint-js-sources.list && \
    echo 'Acquire { https::Verify-Peer false }' >/etc/apt/apt.conf.d/quick-lint-js-kitware-https-hack && \
    apt-get update && \
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
