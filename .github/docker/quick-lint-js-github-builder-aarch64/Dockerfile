FROM ubuntu:18.04

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      apt-transport-https \
      curl \
      gnupg \
      software-properties-common && \
    curl --insecure 'https://deb.nodesource.com/gpgkey/nodesource.gpg.key' | apt-key add - && \
    apt-add-repository 'deb http://deb.nodesource.com/node_14.x xenial main' && \
    add-apt-repository -y ppa:neovim-ppa/unstable && \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list /etc/apt/sources.list.d/*.list >/etc/apt/sources.list.d/quick-lint-js-sources.list

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      bash \
      build-essential \
      cmake \
      coreutils \
      debhelper \
      dpkg-dev \
      emacs-nox \
      fakeroot \
      g++-8 \
      gcc-8 \
      git \
      libasound2 \
      libatk-bridge2.0-0 \
      libatk1.0-0 \
      # HACK(strager): Install libc6 to avoid error when collecting copyright \
      # information: \
      # E: Can not find version '2.27-3ubuntu1.3' of package 'glibc' \
      libc6 \
      libcairo2 \
      libgbm1 \
      libgdk-pixbuf2.0-0 \
      libgtk-3-0 \
      libnss3 \
      libpango-1.0-0 \
      libxcomposite1 \
      libxcursor1 \
      libxi6 \
      libxrandr2 \
      libxss1 \
      libxtst6 \
      lintian \
      neovim \
      ninja-build \
      nodejs \
      python3.7 \
      python3.7-venv \
      sudo \
      tar \
      unzip \
      vim \
      xvfb \
      zip && \
    npm install -g yarn
