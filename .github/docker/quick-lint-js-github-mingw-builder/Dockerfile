# Stage 1: build the cross compiler.
FROM ubuntu:20.04 AS bootstrap

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      cmake \
      curl \
      ninja-build \
      python3 \
      python3-distutils \
      tar \
      xz-utils

WORKDIR /build
RUN curl --location https://github.com/mingw-w64/mingw-w64/archive/a3f6d363d534e9d5563a0b7c677889101e6bbd42.tar.gz | tar xz && \
    mv mingw-w64-a3f6d363d534e9d5563a0b7c677889101e6bbd42 mingw-w64
RUN curl --location https://github.com/llvm/llvm-project/archive/refs/tags/llvmorg-14.0.0.tar.gz | tar xz && \
    mv llvm-project-llvmorg-14.0.0 llvm-project

ENV TOOLCHAIN_ARCHS="armv7 aarch64"
ARG CORES=

COPY llvm-mingw/build-llvm.sh ./
RUN ./build-llvm.sh /opt/llvm-mingw
COPY llvm-mingw/install-wrappers.sh ./
COPY llvm-mingw/wrappers/* ./wrappers/
RUN ./install-wrappers.sh /opt/llvm-mingw
COPY llvm-mingw/build-mingw-w64.sh ./
RUN ./build-mingw-w64.sh /opt/llvm-mingw
COPY llvm-mingw/build-compiler-rt.sh ./
RUN ./build-compiler-rt.sh /opt/llvm-mingw
COPY llvm-mingw/build-libcxx.sh ./
RUN ./build-libcxx.sh /opt/llvm-mingw
COPY llvm-mingw/strip-llvm.sh ./
RUN ./strip-llvm.sh /opt/llvm-mingw

# Stage 2: create a minimal image for cross-compilation.
FROM ubuntu:20.04

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      apt-transport-https \
      ca-certificates \
      curl \
      gnupg \
      software-properties-common && \
    curl 'https://deb.nodesource.com/gpgkey/nodesource.gpg.key' | apt-key add - && \
    apt-add-repository 'deb https://deb.nodesource.com/node_14.x focal main' && \
    sed -n -e 's/^deb /deb-src /p' /etc/apt/sources.list >/etc/apt/sources.list.d/quick-lint-js-sources.list

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      cmake \
      git \
      ninja-build \
      nodejs

COPY --from=bootstrap /opt/llvm-mingw /opt/llvm-mingw
ENV PATH=/opt/llvm-mingw/bin:$PATH
