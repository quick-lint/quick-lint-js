FROM ubuntu:16.04

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      apt-transport-https \
      curl \
      software-properties-common && \
    curl 'https://apt.kitware.com/keys/kitware-archive-latest.asc' | apt-key add - && \
    curl 'https://apt.llvm.org/llvm-snapshot.gpg.key' | apt-key add - && \
    curl 'https://deb.nodesource.com/gpgkey/nodesource.gpg.key' | apt-key add - && \
    apt-add-repository 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-9 main' && \
    apt-add-repository 'deb https://apt.kitware.com/ubuntu/ xenial main' && \
    apt-add-repository 'deb https://deb.nodesource.com/node_14.x xenial main' && \
    add-apt-repository -y ppa:deadsnakes/ppa && \
    add-apt-repository -y ppa:git-core/ppa && \
    add-apt-repository -y ppa:neovim-ppa/unstable && \
    add-apt-repository -y ppa:ubuntu-toolchain-r/test && \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list /etc/apt/sources.list.d/*.list >/etc/apt/sources.list.d/quick-lint-js-sources.list

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      bash \
      build-essential \
      clang-9 \
      clang-format-9 \
      cmake \
      coreutils \
      debhelper \
      dpkg-dev \
      emacs-nox \
      fakeroot \
      g++-8 \
      g++-9 \
      gcc-8 \
      gcc-9 \
      git \
      libasound2 \
      libatk-bridge2.0-0 \
      libatk1.0-0 \
      libc++-9-dev \
      libc++abi-9-dev \
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
      sudo \
      tar \
      unzip \
      vim \
      xvfb \
      zip && \
    ln -s /usr/bin/clang-format-9 /usr/bin/clang-format && \
    npm install -g yarn
