FROM debian:bookworm

RUN \
    sed -i -e 's/^Types: deb$/Types: deb deb-src/' /etc/apt/sources.list.d/debian.sources && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      ca-certificates \
      git \
      neovim \
      nodejs \
      vim \
      zip
