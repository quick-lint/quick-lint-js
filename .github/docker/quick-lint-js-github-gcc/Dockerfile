FROM debian:bookworm

RUN \
    sed -i -e 's/^Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/debian.sources && \
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
