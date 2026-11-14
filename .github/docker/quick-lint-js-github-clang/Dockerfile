FROM debian:bookworm

RUN \
    sed -i -e 's/^Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/debian.sources && \
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
      libstdc++-12-dev \
      ninja-build \
      nodejs \
      python3
